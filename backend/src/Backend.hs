{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Common.Api
import Data.Aeson.Text

-- MIGRATIONS

migrationUsuario :: Query
migrationUsuario = "CREATE TABLE IF NOT EXISTS usuario\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

-- migrationExercicio :: Query
-- migrationExercicio = "CREATE TABLE IF NOT EXISTS exercicio\
--   \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

-- migrationRegistro :: Query
-- migrationRegistro = "CREATE TABLE IF NOT EXISTS registro\
--   \ (id          SERIAL PRIMARY KEY,\ 
--   \  usuarioId   INT NOT NULL,\
--   \  exercicioId INT NOT NULL,\
--   \  series      INT,\
--   \  repeticoes  INT,\
--   \  duracao     INT,\
--   \  ano         INT NOT NULL,\
--   \  mes         INT NOT NULL,\
--   \  dia         INT NOT NULL,\
--   \  FOREIGN KEY (usuarioId) REFERENCES usuario (id),\
--   \  FOREIGN KEY (exercicioId) REFERENCES exercicio (id)\
--   \ )"

-- INFORMAÇÕES DO BANCO DE DADOS

getConn :: ConnectInfo
getConn  = ConnectInfo 
  "" -- maquina (host)
  5432 -- porta
  "" -- usuario
  "" -- senha
  "" -- banco

-- CONEXÃO COM O BANCO DE DADOS

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    dbcon <- connect getConn
    serve $ do
      \case
        -- ROTAS

        BackendRoute_InserirUsuario :/ () -> method POST $ do
          novoUsuario <- A.decode <$> readRequestBody 2000
          case novoUsuario of
            Just nU -> do
              liftIO $ do
                execute_ dbcon migrationUsuario
                execute dbcon "INSERT INTO usuario (nome) VALUES (?)" [usuarioNome nU]
              modifyResponse $ setResponseStatus 200 "OK"    
            _ -> modifyResponse $ setResponseStatus 500 "Erro"

        BackendRoute_ListarUsuarios :/ () -> method GET $ do
          res :: [Usuario] <- liftIO $ do
            execute_ dbcon migrationUsuario
            query_ dbcon "SELECT * from usuario"
          modifyResponse $ setResponseStatus 200 "OK"
          writeLazyText (encodeToLazyText res)
    
        BackendRoute_BuscarUsuario :/ uid -> method GET $ do 
          res :: [Usuario] <- liftIO $ do
            execute_ dbcon migrationUsuario
            query dbcon "SELECT * from usuario where id=?" (Only (uid :: Int))
          if res /= [] then do
            modifyResponse $ setResponseStatus 200 "OK"   
            writeLazyText (encodeToLazyText (Prelude.head res))
          else 
            modifyResponse $ setResponseStatus 404 "NOT FOUND"

        BackendRoute_EditarUsuario :/ uid -> method POST $ do
          alteracaoUsuario <- A.decode <$> readRequestBody 2000
          case alteracaoUsuario of
            Just aU -> do
              liftIO $ do
                execute_ dbcon migrationUsuario
                execute dbcon "UPDATE usuario SET nome = ? WHERE id = ?" (usuarioNome aU, uid)
              modifyResponse $ setResponseStatus 200 "OK"
            Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"

        BackendRoute_ApagarUsuario :/ uid -> method POST $ do 
          res :: [Usuario] <- liftIO $ do
            execute_ dbcon migrationUsuario
            query dbcon "SELECT * from usuario where id=?" (Only (uid :: Int))
          if res /= [] then do
            liftIO $ do
              execute_ dbcon migrationUsuario
              execute dbcon "DELETE from usuario where id=?" (Only (uid :: Int))
            modifyResponse $ setResponseStatus 200 "OK"   
          else
            modifyResponse $ setResponseStatus 404 "NOT FOUND"  

        -- QUALQUER ROTA NÃO MAPEADA: 
        _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }