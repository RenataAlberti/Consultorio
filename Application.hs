{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

import Home
import Handlers.Usuario
import Handlers.Medico

import Database.Persist.Postgresql

mkYesodDispatch "App" resourcesApp
