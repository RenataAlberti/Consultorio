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
Paciente
    nome            Text
    dtNasc          Day sqltype=date
    rg              Text
    endereco        Text
    numero          Int
    bairro          Text
    cidade          Text
    sgUF            Text sqltype=char(2)
    telefone        Int
    celular         Int
    email           Text
    deriving Show
    
Especialidade
    nome    Text
    deriving Show
    
Medico
    nome            Text
    crm             Int
    idEsp           EspecialidadeId
    deriving Show
    
Consulta
    idPac           PacienteId
    idMed           MedicoId
    dtConsulta      Day sqltype=date
    obsConsulta     Text
    deriving Show
    
Retorno
    idCon           ConsultaId
    idMed           MedicoId
    dtRetorno       Day sqltype=date
    obsRetorno      Text
    deriving Show
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