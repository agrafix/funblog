{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Blog where

import Model.CoreTypes
import Model.ResponseTypes
import Web.Actions.User
import Web.Forms.Login
import Web.Forms.Register
import Web.Forms.Post
import Web.Utils
import Web.Views.Home
import Web.Views.Site

import Control.Monad.Logger
import Data.Monoid
import Database.Persist.Sqlite hiding (get)
import Network.Wai.Middleware.Static
import Text.Blaze.Html (Html, toHtml)
import Text.Digestive.Bootstrap (renderForm)
import Web.Spock.Digestive
import Web.Spock.Safe hiding (SessionId)
import qualified Data.Configurator as C
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as Http

type SessionVal = Maybe SessionId
type BlogApp = SpockM SqlBackend SessionVal BlogState ()
type BlogAction a = SpockAction SqlBackend SessionVal BlogState a

data BlogState
   = BlogState
   { bs_cfg :: BlogCfg
   }

data BlogCfg
   = BlogCfg
   { bcfg_db   :: T.Text
   , bcfg_port :: Int
   , bcfg_name :: T.Text
   , bcfg_desc :: T.Text
   }

parseConfig :: FilePath -> IO BlogCfg
parseConfig cfgFile =
    do cfg <- C.load [C.Required cfgFile]
       db <- C.require cfg "db"
       port <- C.require cfg "port"
       name <- C.require cfg "blogName"
       desc <- C.require cfg "blogDescription"
       return (BlogCfg db port name desc)

runBlog :: BlogCfg -> IO ()
runBlog bcfg =
    do pool <- runNoLoggingT $ createSqlitePool (bcfg_db bcfg) 5
       runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
       runSpock (bcfg_port bcfg) $ spock sessCfg (PCPool pool) (BlogState bcfg) blogApp
    where
      sessCfg =
          SessionCfg
          { sc_cookieName = "funblog"
          , sc_sessionTTL = 5 * 60 * 50
          , sc_sessionIdEntropy = 40
          , sc_emptySession = Nothing
          , sc_persistCfg = Nothing
          , sc_sessionExpandTTL = True
          }

mkSite :: (SiteView -> Html) -> BlogAction a
mkSite content =
    maybeUser $ \mUser ->
    do blogSt <- getState
       let cfg = bs_cfg blogSt
           sv =
               SiteView
               { sv_blogName = bcfg_name cfg
               , sv_blogDesc = bcfg_desc cfg
               , sv_user = fmap snd mUser
               }
       blaze $ siteView sv (content sv)

mkSite' :: Html -> BlogAction a
mkSite' content = mkSite (const content)

getpost url action =
    do get url action
       post url action

blogApp :: BlogApp
blogApp =
    do middleware (staticPolicy (addBase "static"))
       get "/" $
           mkSite (\sv -> homeView sv)
       get "/about" $
           mkSite mempty
       get "/manage" $ requireUser $ requireRights [userIsAdmin] $ \_ ->
           mkSite mempty
       getpost "/write" $ requireUser $ \_ ->
           do f <- runForm "writePost" postForm
              let formView mErr view =
                      panelWithErrorView "Write a Post" mErr $ (renderForm postFormSpec view)
              case f of
                (view, Nothing) ->
                    mkSite' (formView Nothing view)
                (view, Just newPost) ->
                    error "Not implemented"
       getpost "/login" $
           do f <- runForm "loginForm" loginForm
              let formView mErr view =
                      panelWithErrorView "Login" mErr $ (renderForm loginFormSpec view)
              case f of -- (View, Maybe LoginRequest)
                (view, Nothing) ->
                    mkSite' (formView Nothing view)
                (view, Just loginReq) ->
                    do loginRes <-
                           runSQL $ loginUser (lr_user loginReq) (lr_password loginReq)
                       case loginRes of
                         Just userId ->
                             do sid <- runSQL $ createSession userId
                                writeSession (Just sid)
                                redirect "/"
                         Nothing ->
                             mkSite' (formView (Just "Invalid login credentials!") view)
       getpost "/register" $
            do f <- runForm "registerForm" registerForm
               let formView mErr view =
                       panelWithErrorView "Register" mErr $ renderForm registerFormSpec view
               case f of
                 (view, Nothing) ->
                     mkSite' (formView Nothing view)
                 (view, Just registerReq) ->
                     if rr_password registerReq /= rr_passwordConfirm registerReq
                     then mkSite' (formView (Just "Passwords do not match") view)
                     else do registerRes <-
                                 runSQL $ registerUser (rr_username registerReq) (rr_email registerReq) (rr_password registerReq)
                             case registerRes of
                               CommonError errMsg ->
                                   mkSite' (formView (Just errMsg) view)
                               CommonSuccess _ ->
                                   mkSite' (panelWithErrorView "Register - Success!" Nothing $ "Great! You may now login.")
       get "/logout" $ requireUser $ \(userId, _) ->
           do runSQL $ killSessions userId
              writeSession Nothing
              redirect "/"

requireRights :: [User -> Bool] -> ((UserId, User) -> BlogAction a) -> (UserId, User) -> BlogAction a
requireRights rights action u@(_, user) =
    if or $ map (\f -> f user) rights
    then action u
    else noAccessPage "You don't have enough rights, sorry."

noAccessPage :: T.Text -> BlogAction a
noAccessPage msg =
    do setStatus Http.status403
       prefResp <- preferredFormat
       case prefResp of
         PrefJSON ->
             json (CommonError msg)
         _ ->
             mkSite' (panelWithErrorView "No Access" Nothing (toHtml msg))

maybeUser :: (Maybe (UserId, User) -> BlogAction a) -> BlogAction a
maybeUser action =
    do sess <- readSession
       case sess of
         Nothing ->
             action Nothing
         Just sid ->
             do mUser <- runSQL $ loadUser sid
                action mUser

requireUser :: ((UserId, User) -> BlogAction a) -> BlogAction a
requireUser action =
    maybeUser $ \mUser ->
    case mUser of
      Nothing ->
          noAccessPage "Please login first"
      Just userTuple ->
          action userTuple
