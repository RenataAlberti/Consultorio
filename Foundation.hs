{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod
import Data.Text
import Yesod.Static
import Data.Time.Calendar
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

data App = App {getStatic :: Static, connPool :: ConnectionPool }

-- Paciente tem responsavel? Tem tabela responsavel?
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Usuario
    nome            Text
    email           Text
    senha           Text
    UniqueEmail email
    deriving Show
    
Medico
    nome            Text
    crm             Text
    especialidade   Text
    UniqueCrm crm
    deriving Show
    
Paciente
    nome            Text
    nascimento      Day
    cpf             Text
    endereco        Text
    numero          Int
    bairro          Text
    cidade          Text
    uf              Text sqltype=char(2)
    telefone        Int
    celular         Int
    email           Text
    UniqueCpf cpf
    deriving Show

Consulta
    dtconsulta      Day
    pacid           PacienteId
    medid           MedicoId
    obsconsulta     Text
    deriving Show
    
|]

staticFiles "static"

mkYesodData "App" $(parseRoutesFile "routes")

instance YesodPersist App where
   type YesodPersistBackend App = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod App where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
