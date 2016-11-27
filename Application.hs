{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

import Add
import Home
import Handlers.Consulta
import Handlers.Especialidade
import Handlers.Medico
import Handlers.Paciente
import Handlers.Padrao

mkYesodDispatch "App" resourcesApp
