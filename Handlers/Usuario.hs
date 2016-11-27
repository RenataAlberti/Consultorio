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

postUsuarioR :: Handler Html
postUsuarioR = do
        ((result,_),_)<- runFormPost formUsuario
        case result of
            FormSuccess usuario -> do
                vid <- runDB $ insert usuario
                defaultLayout [whamlet|
                    <h1> Usuario #{fromSqlKey vid} cadastro!
                |]
            _ -> redirect HomeR

getListUsuarioR :: Handler Html
getListUsuarioR = undefined