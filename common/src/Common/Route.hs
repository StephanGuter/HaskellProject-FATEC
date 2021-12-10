{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Route where

import Data.Text (Text, unpack)
import Data.Function -- Para o operador &
import Data.Functor.Identity
import Obelisk.Route
import Obelisk.Route.TH

-- ROTAS DO BACKEND

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()

  BackendRoute_InserirUsuario :: BackendRoute ()
  BackendRoute_ListarUsuarios :: BackendRoute ()
  BackendRoute_BuscarUsuario  :: BackendRoute Int
  BackendRoute_EditarUsuario  :: BackendRoute Int
  BackendRoute_ApagarUsuario  :: BackendRoute Int

  -- BackendRoute_InserirExercicio :: BackendRoute ()
  -- BackendRoute_ListarExercicios :: BackendRoute ()
  -- BackendRoute_BuscarExercicio  :: BackendRoute Int
  -- BackendRoute_EditarExercicio  :: BackendRoute Int
  -- BackendRoute_ApagarExercicio  :: BackendRoute Int

  -- BackendRoute_InserirRegistro :: BackendRoute ()
  -- BackendRoute_ListarRegistros :: BackendRoute ()
  -- BackendRoute_BuscarRegistro  :: BackendRoute Int
  -- BackendRoute_EditarRegistro  :: BackendRoute Int
  -- BackendRoute_ApagarRegistro  :: BackendRoute Int

-- TOTAS DO FRONTEND

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()

-- VERIFICA SE A ROTA ESTÁ CORRETA

checFullREnc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & \case
  Left err -> error $ unpack err
  Right encoder -> encoder  

-- CODIFICADOR DAS ROTAS

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing          -> PathSegment "missing" $ unitEncoder mempty

      BackendRoute_InserirUsuario   -> PathSegment "inserirUsuario" $ unitEncoder mempty
      BackendRoute_ListarUsuarios   -> PathSegment "listarUsuarios" $ unitEncoder mempty
      BackendRoute_BuscarUsuario    -> PathSegment "buscarUsuario" readShowEncoder
      BackendRoute_EditarUsuario    -> PathSegment "editarUsuario" readShowEncoder
      BackendRoute_ApagarUsuario    -> PathSegment "apagarUsuario" readShowEncoder)

      -- BackendRoute_InserirExercicio -> PathSegment "inserirExercicio" $ unitEncoder mempty
      -- BackendRoute_ListarExercicios -> PathSegment "listarExercicios" $ unitEncoder mempty
      -- BackendRoute_BuscarExercicio  -> PathSegment "buscarExercicio" readShowEncoder
      -- BackendRoute_EditarExercicio  -> PathSegment "editarExercicio" readShowEncoder
      -- BackendRoute_ApagarExercicio  -> PathSegment "apagarExercicio" readShowEncoder

      -- BackendRoute_InserirRegistro  -> PathSegment "inserirRegistro" $ unitEncoder mempty
      -- BackendRoute_ListarRegistros  -> PathSegment "listarRegistros" $ unitEncoder mempty
      -- BackendRoute_BuscarRegistro   -> PathSegment "buscarRegistro" readShowEncoder
      -- BackendRoute_EditarRegistro   -> PathSegment "editarRegistro" readShowEncoder
      -- BackendRoute_ApagarRegistro   -> PathSegment "apagarRegistro" readShowEncoder)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]