{-# LANGUAGE OverloadedStrings #-}
module Web.Forms.Post where

import Model.CoreTypes

import Data.Time
import Text.Blaze.Html (Html)
import Text.Digestive hiding (Post)
import Text.Digestive.Bootstrap

postForm :: Monad m => UTCTime -> Form Html m Post
postForm now =
    Post <$> "title" .: text Nothing
         <*> "date" .: stringRead "Couldn't parse as UTCTime" (Just now)
         <*> "content" .: text Nothing

postFormSpec :: FormMeta
postFormSpec =
    FormMeta
    { fm_method = POST
    , fm_target = "/write"
    , fm_components =
        [ FCSection
            FormSection
            { fs_title = Nothing
            , fs_help = Nothing
            , fs_elements =
                [ FormElement "title" (Just "Title") (Just "Title") InputText
                , FormElement "date" (Just "Date") Nothing InputText
                , FormElement "content" (Just "Content") (Just "Content") $ InputTextArea (Just 30) (Just 10)
                ]
            }
        ]
    , fm_submitValue = "Publish"
    }
