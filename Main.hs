{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Yesod.Static
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql

-- String de conexao
-- Necessario: dbname | host | user | password | port="5432"
connStr :: ConnectionString
connStr = "dbname=ded3ia8hf1459f host=ec2-23-23-208-32.compute-1.amazonaws.com user=redzjfefujrzlt password=kejjLnv9C9miSkMlUmmuYjVCNf port=5432"

main :: IO ()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
        runSqlPersistMPool (runMigration migrateAll) pool
        static@(Static settings) <- static "static"
        warp 8080 (App static pool)
