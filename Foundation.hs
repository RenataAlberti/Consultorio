{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod
import Data.Text
import Yesod.Static
import Data.Time(Day)
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

staticFiles "static"

data App = App {getStatic :: Static, connPool :: ConnectionPool }

-- Paciente tem responsavel? Tem tabela responsavel?
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Usuario
    nome            Text
    email           Text
    senha           Text
|]

mkYesodData "App" $(parseRoutesFile "routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where


instance YesodPersist App where
   type YesodPersistBackend App = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage