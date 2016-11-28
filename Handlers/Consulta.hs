{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Consulta where

import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Time.Calendar
import Yesod.Form.Fields
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist
  
formConsulta :: Form Consulta
formConsulta = renderDivs $ Consulta
        <$> areq dayField  "Data da Consulta "          Nothing
        <*> areq (selectField listpac) "Paciente"       Nothing
        <*> areq (selectField listmed) "Medico"         Nothing
        <*> areq textField  "Observação da consulta"    Nothing

listmed = do
       entidades <- runDB $ selectList [] [Asc MedicoNome] 
       optionsPairs $ fmap (\ent -> (medicoNome $ entityVal ent, entityKey ent)) entidades

listpac = do
       entidades <- runDB $ selectList [] [Asc PacienteNome] 
       optionsPairs $ fmap (\ent -> (pacienteNome $ entityVal ent, entityKey ent)) entidades

getConsultaR :: Handler Html
getConsultaR = do
    (widget,enctype) <- generateFormPost formConsulta
    defaultLayout $ do
        [whamlet|
            <form action=@{ConsultaR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
        
        
getListConsultaR :: Handler Html
getListConsultaR = do
            consultas <- runDB $ selectList [] [Asc ConsultaObsconsulta]
            defaultLayout $(whamletFile "templates/listar_consulta.hamlet")
            
        
postDelConsultaR :: ConsultaId -> Handler Html
postDelConsultaR conid = do 
                runDB $ delete conid
                redirect ListConsultaR
         

postConsultaR :: Handler Html
postConsultaR = do
                ((result, _), _) <- runFormPost formConsulta
                case result of
                    FormSuccess consulta -> do
                              conid <- runDB $ insert consulta 
                              redirect (ListConsultaR)
                    _ -> redirect HomeR
