{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Paciente where

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
  
formPaciente :: Form Paciente
formPaciente = renderDivs $ Paciente
        <$> areq textField  "nome"          Nothing
        <*> areq dayField   "nascimento"    Nothing
        <*> areq textField  "cpf"           Nothing
        <*> areq textField  "endereco"      Nothing
        <*> areq intField   "numero"        Nothing
        <*> areq textField  "bairro"        Nothing
        <*> areq textField  "cidade"        Nothing
        <*> areq textField FieldSettings{fsId=Just "uf", fsTooltip= Nothing, fsLabel="Sigla", fsName= Nothing, fsAttrs=[("maxlength","2")]} Nothing
        <*> areq intField   "telefone"      Nothing
        <*> areq intField   "celular"       Nothing
        <*> areq emailField "email"         Nothing
        

getPacienteR :: Handler Html
getPacienteR = do
    (widget,enctype) <- generateFormPost formPaciente
    defaultLayout $ do
        [whamlet|
            <form action=@{PacienteR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
        
        
getListPacienteR :: Handler Html
getListPacienteR = do
            pacientes <- runDB $ selectList [] [Asc PacienteNome]
            defaultLayout $(whamletFile "templates/listar_paciente.hamlet")
            
        
postDelPacienteR :: PacienteId -> Handler Html
postDelPacienteR pacid = do 
                runDB $ delete pacid
                redirect ListPacienteR
         

postPacienteR :: Handler Html
postPacienteR = do
                ((result, _), _) <- runFormPost formPaciente
                case result of
                    FormSuccess paciente -> do
                       unicoCpf <- runDB $ getBy $ UniqueCpf (pacienteCpf paciente)
                       case unicoCpf of
                           Just _ -> redirect PacienteR
                           Nothing -> do 
                              pacid <- runDB $ insert paciente 
                              redirect (ListPacienteR)
                    _ -> redirect HomeR
