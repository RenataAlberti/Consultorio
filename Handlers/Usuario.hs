{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Usuario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario
        <$> areq textField   "nome"       Nothing
        <*> areq emailField  "email"      Nothing
        <*> areq passwordField "password" Nothing


getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,enctype) <- generateFormPost formUsuario
    defaultLayout $ do
        [whamlet|
            <form action=@{UsuarioR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
        
        
getListUsuarioR :: Handler Html
getListUsuarioR = do
            usuarios <- runDB $ selectList [] [Asc UsuarioNome]
            defaultLayout $(whamletFile "templates/listar_usuario.hamlet")
            
        
postDelUsuarioR :: UsuarioId -> Handler Html
postDelUsuarioR userid = do 
                runDB $ delete userid
                redirect ListUsuarioR
         


postUsuarioR :: Handler Html
postUsuarioR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                       unicoEmail <- runDB $ getBy $ UniqueEmail (usuarioEmail usuario)
                       case unicoEmail of
                           Just _ -> redirect UsuarioR
                           Nothing -> do 
                              uid <- runDB $ insert usuario 
                              redirect (ListUsuarioR)
                    _ -> redirect HomeR
