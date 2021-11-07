{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

nav :: DomBuilder t m => m ()
nav = do
  el "header" $ do
    elAttr "a" ("href" =: "#") (text "Início")
    el "nav" $ do
      el "ul" $ do
        el "li" $ do
          elAttr "a" ("href" =: "https://kenshin.fandom.com/wiki/Uonuma_Usui") (text "Sobre")
        el "li" $ do
          elAttr "a" ("href" =: "https://kenshin.fandom.com/wiki/J%C5%ABppongatana") (text "Jūppongatana")



frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Uonuma Usui"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      nav
      el "p" $ text "Enquanto não pegar o jeito, vai ser nos..."
      elAttr "img" ("src" =: static @"Usui_Uonuma.jpg" <> "title" =: "Olhos do coração") blank
      return ()
  }
