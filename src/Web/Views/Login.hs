{-# LANGUAGE OverloadedStrings #-}
module Web.Views.Login where

import Data.Monoid
import Text.Blaze.XHtml5 ((!))
import qualified Data.Text as T
import qualified Text.Blaze.Bootstrap as H
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

loginView :: Maybe T.Text -> H.Html -> H.Html
loginView mError ct =
    H.div ! A.class_ "panel panel-info" ! A.style "margin-top: 30px;" $
     do H.div ! A.class_ "panel-heading" $
         H.div ! A.class_ "panel-title" $ "Login"
        H.div ! A.class_ "panel-body" $
         do case mError of
              Just errMsg ->
                  H.alertBox H.BootAlertDanger (H.toHtml errMsg)
              Nothing -> mempty
            H.div ct
