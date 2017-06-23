{-# LANGUAGE OverloadedStrings #-}
module Web.Forms.Register where

import Web.Forms.Common

import Text.Blaze.Html (Html)
import Text.Digestive
import Text.Digestive.Bootstrap
import qualified Data.Text as T

data RegisterRequest
   = RegisterRequest
   { rr_username :: T.Text
   , rr_password :: T.Text
   , rr_passwordConfirm :: T.Text
   , rr_email :: T.Text
   } deriving (Show)

registerForm :: Monad m => Form Html m RegisterRequest
registerForm =
    RegisterRequest <$> "name" .: usernameFormlet Nothing
                    <*> "password1" .: passwordFormlet Nothing
                    <*> "password2" .: passwordFormlet Nothing
                    <*> "email" .: emailFormlet Nothing

registerFormSpec :: FormMeta
registerFormSpec =
    FormMeta
    { fm_method = POST
    , fm_target = "/register"
    , fm_components =
        [ FCSection
            FormSection
            { fs_title = Nothing
            , fs_help = Nothing
            , fs_elements =
                [ FormElement "name" (Just "Username") (Just "Username") InputText
                , FormElement "email" (Just "Email") (Just "Email") InputText
                , FormElement "password1" (Just "Password") (Just "Password") InputPassword
                , FormElement "password2" (Just "Repeat Password") (Just "Repeat Password") InputPassword
                ]
            }
        ]
    , fm_submitValue = "Register"
    }
