{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Data.Maybe
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Text.Read (readMaybe)
import Reflex.Dom.Core
import Data.Aeson (ToJSON)
import Common.Api
import Common.Route
import Control.Monad.Fix

-- CONFIGURAÇÃO DAS PÁGINAS

data Pagina = HomePage | NovoUsuarioPage

getPath :: R BackendRoute -> T.Text
getPath p = renderBackendRoute checFullREnc p

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

-- PÁGINAS

  -- HomePage

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_ListarUsuarios :/ ())) def

data Acao = Mostrar Int | Editar Int | Apagar Int

regUsuario :: (PostBuild t m, DomBuilder t m) => Dynamic t Usuario -> m (Event t Acao)
regUsuario u = do 
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . usuarioId) u)
        el "td" (dynText $ fmap (T.pack . show .usuarioNome) u) 
        evtMostrar <- fmap (fmap (const Mostrar)) (button "Registro de Atividades Físicas")
        evtEditar <- fmap (fmap (const Editar)) (button "Editar")
        evtApagar <- fmap (fmap (const Apagar)) (button "Apagar")
        return (attachPromptlyDynWith (flip ($)) (fmap usuarioId u) (leftmost [evtMostrar, evtEditar, evtApagar]))

tabelaUsuariosRequest :: 
  ( DomBuilder t m
  , Prerender js t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m) => Workflow t m T.Text
tabelaUsuariosRequest = Workflow $ do
  el "p" (text "Bem vindo ao aplicativo mais consistente para fazer o registro de seus exercícios e atividades físicas!")
  elAttr "div" ("class" =: "tabela-usuarios") $ do
    btnMostrarUsuarios <- button "Mostrar Usuários"
    us :: Dynamic t (Event t (Maybe [Usuario])) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> btnMostrarUsuarios))
    evtUs <- return (fmap (fromMaybe []) $ switchDyn us)
    dynUs <- foldDyn (++) [] evtUs
    tb <- el "table" $ do
      el "thead" $ do
        el "tr" $ do
          el "th" (text "Id")
          el "th" (text "Nome")
          el "th" blank
          el "th" blank
          el "th" blank
      el "tbody" $ do
        simpleList dynUs regUsuario
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("", acao <$> tb')
    where
      acao (Mostrar uid) = mostrarUsuariosPage uid
      acao (Editar uid) = editarUsuariosPage uid
      acao (Apagar uid) = apagarUsuariosPage uid

getUsuarioRequest :: Int -> XhrRequest ()
getUsuarioRequest uid = xhrRequest "GET" (getPath (BackendRoute_BuscarUsuario :/ uid)) def

mostrarUsuariosPage :: 
  ( DomBuilder t m
  , Prerender js t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m) => Int -> Workflow t m T.Text
mostrarUsuariosPage uid = Workflow $ do
  btnAuxMostrarUsuario <- button "Mostrar Usuario"
  u :: Dynamic t (Event t (Maybe Usuario)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getUsuarioRequest uid) <$> btnAuxMostrarUsuario))
  mdyn <- holdDyn Nothing (switchDyn u)
  dynU <- return ((fromMaybe (Usuario 0 "")) <$> mdyn)
  el "div" $ do
    el "div" (dynText $ fmap usuarioNome dynU)
  -- INCLUIR PAGINA DE REGISTRO DE ATIVIDADES FÍSICAS AQUI
  ret <- button "Voltar"
  return ("Usuario: " <> (T.pack $ show uid), tabelaUsuariosRequest <$ ret)

editarUsuariosPage :: 
  ( DomBuilder t m
  , Prerender js t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m) => Int -> Workflow t m T.Text
editarUsuariosPage uid = Workflow $ do
  btnAuxMostrarUsuario <- button "Mostrar Usuário"
  u :: Dynamic t (Event t (Maybe Usuario)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getUsuarioRequest uid) <$> btnAuxMostrarUsuario))
  mdyn <- return (switchDyn u)
  dynE <- return ((fromMaybe (Usuario 0 "")) <$> mdyn)
  inputNome <- inputElement $ def & inputElementConfig_setValue .~ (fmap usuarioNome dynE)
  let usuario = fmap (\n -> Usuario 0 n) (_inputElement_value inputNome)
  submitBtn <- button "Editar Usuário"
  let usuarioEvt = tag (current usuario) submitBtn
  _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_EditarUsuario :/ uid) <$> usuarioEvt)) 
  return ("Usuario: " <> (T.pack $ show uid), tabelaUsuariosRequest <$ submitBtn)

apagarUsuariosPage ::
  ( DomBuilder t m
  , Prerender js t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m) => Int -> Workflow t m T.Text
apagarUsuariosPage uid = Workflow $ do
  btnAuxMostrarUsuario <- button "Mostrar Usuario"
  u :: Dynamic t (Event t (Maybe Usuario)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getUsuarioRequest uid) <$> btnAuxMostrarUsuario))
  mdyn <- holdDyn Nothing (switchDyn u)
  dynU <- return ((fromMaybe (Usuario 0 "")) <$> mdyn)
  el "div" $ do
    el "div" (dynText $ fmap usuarioNome dynU)
  submitBtn <- button "Apagar Usuário e Voltar"
  let usuarioEvt = tag (current dynU) submitBtn
  _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_ApagarUsuario :/ uid) <$> usuarioEvt))
  return ("Usuario: " <> (T.pack $ show uid), tabelaUsuariosRequest <$ submitBtn)

homePageRequest :: 
  ( DomBuilder t m
  , Prerender js t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m) => m ()
homePageRequest = do
    r <- workflow tabelaUsuariosRequest
    el "div" (dynText r)

  -- NovoUsuarioPage

novoUsuarioPageRequest :: (DomBuilder t m, Prerender js t m) => m ()
novoUsuarioPageRequest = do
  elAttr "div" ("class" =: "criar-usuario") $ do
    inputNome <- inputElement def
    let usuario = fmap (\n -> Usuario 0 n) (_inputElement_value inputNome)
    (submitBtn,_) <- el' "button" (text "Criar Usuário")
    let click = domEvent Click submitBtn
    let usuarioEvt = tag (current usuario) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_InserirUsuario :/ ()) <$> usuarioEvt))
    return ()

-- GERENCIAMENTO DE PÁGINAS

menuClick :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
menuClick p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)

menu :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menu = do
    evs <- el "ul" $ do
        p1 <- menuClick HomePage "Home"
        p2 <- menuClick NovoUsuarioPage "Novo Usuário"
        return (leftmost [p1,p2])
    holdDyn HomePage evs

currentPage :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m, MonadFix m) => Pagina -> m ()
currentPage p = 
    case p of
         HomePage        -> homePageRequest
         NovoUsuarioPage -> novoUsuarioPageRequest

mainPage :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m, MonadFix m) => m ()
mainPage = do
    page <- elAttr "header" ("class" =: "menu") menu
    dyn_ $ currentPage <$> page

-- GERAÇÃO DO FRONTEND

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "RAF"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" (text "Registro de Atividades Físicas")
      mainPage
      return ()
  }