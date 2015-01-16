{-# LANGUAGE OverloadedStrings #-}
module Web.Views.Home where

import Web.Views.Site (SiteView(..))

import Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

homeView :: SiteView -> H.Html
homeView sv =
    do H.div ! A.class_ "blog-header" $
        do H.h1 ! A.class_ "blog-title" $ H.toHtml $ sv_blogName sv
           H.p ! A.class_ "lead blog-description" $ H.toHtml $ sv_blogDesc sv
       H.div ! A.class_ "row" $
        do H.div ! A.class_ "col-sm-8 blog-main" $
            do H.div ! A.class_ "blog-post" $
                do H.h2 ! A.class_ "blog-post-title" $ "Sample blog post"
                   H.p ! A.class_ "blog-post-meta" $ "Meta line"
                   H.p "Some random post...."
           H.div ! A.class_ "col-sm-3 col-sm-offset-1 blog-sidebar" $
            do H.div ! A.class_ "sidebar-module sidebar-module-inset" $
                do H.h4 "About"
                   H.p "This blog is very cool and written in Haskell."
               H.div ! A.class_ "sidebar-module" $
                do H.h4 "Archives"
                   H.ol ! A.class_ "list-unstyled" $
                     do H.li $ H.a ! A.href "#" $ "Fooo"
