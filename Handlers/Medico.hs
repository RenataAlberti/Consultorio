{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Medico where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

formMedico :: Form Medico
formMedico = renderDivs $ Medico
        <$> areq textField   "nome"          Nothing
        <*> areq textField   "crm"           Nothing
        <*> areq textField   "especialidade" Nothing


getMedicoR :: Handler Html
getMedicoR = do
    (widget,enctype) <- generateFormPost formMedico
    defaultLayout $ do
        [whamlet|
            <form action=@{MedicoR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
        
        
getListMedicoR :: Handler Html
getListMedicoR = do
            medicos <- runDB $ selectList [] [Asc MedicoNome]
            defaultLayout $(whamletFile "templates/listar_medico.hamlet")
            
        
postDelMedicoR :: MedicoId -> Handler Html
postDelMedicoR medid = do 
                runDB $ delete medid
                redirect ListMedicoR
         


postMedicoR :: Handler Html
postMedicoR = do
                ((result, _), _) <- runFormPost formMedico
                case result of
                    FormSuccess medico -> do
                       unicoCrm <- runDB $ getBy $ UniqueCrm (medicoCrm medico)
                       case unicoCrm of
                           Just _ -> redirect MedicoR
                           Nothing -> do 
                              medid <- runDB $ insert medico 
                              redirect (ListMedicoR)
                    _ -> redirect HomeR
