{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

data Usuario = Usuario {
    usuarioId :: Int,
    usuarioNome :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- data Exercicio = Exercicio {
--     exercicioId   :: Int,
--     exercicioNome :: Text
-- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- data Registro = Registro {
--     registroId          :: Int,
--     registroUsuarioId   :: Int,
--     registroExercicioId :: Int,
--     registroSeries      :: Int,
--     registroRepeticoes  :: Int,
--     registroDuracao     :: Int,
--     registroAno         :: Int,
--     registroMes         :: Int,
--     registroDia         :: Int
-- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)