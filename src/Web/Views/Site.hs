{-# LANGUAGE OverloadedStrings #-}
module Web.Views.Site where

import Model.CoreTypes

import Data.Monoid
import Text.Blaze.XHtml5 ((!))
import qualified Data.Text as T
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

data SiteView
   = SiteView
   { sv_blogName :: T.Text
   , sv_blogDesc :: T.Text
   , sv_user :: Maybe User
   }

siteView :: SiteView -> H.Html -> H.Html
siteView sv body =
    H.html $
    do H.head $
        do H.title (H.toHtml $ sv_blogName sv)
           H.meta ! A.charset "utf-8"
           H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
           H.link ! A.href "/css/bootstrap.min.css" ! A.rel "stylesheet"
           H.link ! A.href "/css/blog.css" ! A.rel "stylesheet"
       H.body $
        do H.div ! A.class_ "blog-masthead" $
            H.div ! A.class_ "container" $
             H.nav ! A.class_ "blog-nav" $
              do H.a ! A.class_ "blog-nav-item" ! A.href "/" $ "Home"
                 H.a ! A.class_ "blog-nav-item" ! A.href "/about" $ "About"
                 case sv_user sv of
                   Nothing -> H.a ! A.class_ "blog-nav-item" ! A.href "/login" $ "Login"
                   Just _ -> H.a ! A.class_ "blog-nav-item" ! A.href "/logout" $ "Logout"
           H.div ! A.class_ "container" $ body
           H.div ! A.class_ "blog-footer" $
            do H.p $
                do H.span "Blog template built for "
                   H.a ! A.href "http://getbootstrap.com" $ "Bootstrap"
                   H.span " by "
                   H.a ! A.href "https://twitter.com/mdo" $ "@mdo"
               H.p $
                H.a ! A.href "#" $ "Back to top"
           H.script ! A.href "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js" $ mempty
           H.script ! A.href "/js/bootstrap.min.js" $ mempty
