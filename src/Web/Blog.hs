{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Blog where

import Model.CoreTypes
import Model.ResponseTypes
import Web.Actions.User
import Web.Forms.Login
import Web.Forms.Post
import Web.Forms.Register
import Web.Utils
import Web.Views.Home
import Web.Views.Site

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans
import Data.HVect
import Data.Time
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
type BlogApp ctx = SpockCtxM ctx SqlBackend SessionVal BlogState ()
type BlogAction ctx a = SpockActionCtx ctx SqlBackend SessionVal BlogState a

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
       runSpock (bcfg_port bcfg) $ spock (spockCfg pool) blogApp
    where
      spockCfg pool =
          defaultSpockCfg Nothing (PCPool pool) (BlogState bcfg)

mkSite :: (SiteView -> Html) -> BlogAction ctx a
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

mkSite' :: Html -> BlogAction ctx a
mkSite' content = mkSite (const content)

blogApp :: BlogApp ()
blogApp =
    prehook baseHook $
    do middleware (staticPolicy (addBase "static"))
       get "/" $
           do allPosts <- runSQL $ selectList [] [Desc PostDate]
              mkSite (homeView allPosts)
       get "/about" $
           mkSite mempty
       prehook guestOnlyHook $
               do getpost "/register" registerAction
                  getpost "/login" loginAction
       prehook authHook $
               do get "/logout" logoutAction
                  prehook authorHook $
                      getpost "/write" writeAction
                  prehook adminHook $
                      get "/manage" manageAction

loginAction :: (ListContains n IsGuest xs, NotInList (UserId, User) xs ~ 'True) => BlogAction (HVect xs) a
loginAction =
    do f <- runForm "loginForm" loginForm
       let formView mErr view =
               panelWithErrorView "Login" mErr $ renderForm loginFormSpec view
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

registerAction :: (ListContains n IsGuest xs, NotInList (UserId, User) xs ~ 'True) => BlogAction (HVect xs) a
registerAction =
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
                           mkSite' (panelWithErrorView "Register - Success!" Nothing "Great! You may now login.")

manageAction :: ListContains n IsAdmin xs => BlogAction (HVect xs) a
manageAction = mkSite mempty

writeAction :: (ListContains n IsAuthor xs, ListContains m (UserId, User) xs) => BlogAction (HVect xs) a
writeAction =
    do now <- liftIO getCurrentTime
       f <- runForm "writePost" (postForm now)
       let formView mErr view =
               panelWithErrorView "Write a Post" mErr $ renderForm postFormSpec view
       case f of
         (view, Nothing) ->
             mkSite' (formView Nothing view)
         (_, Just newPost) ->
             do _ <- runSQL $ insert newPost
                mkSite' (panelWithErrorView "Post - Success!" Nothing "Thanks for the post! You can now see it on the home page")

logoutAction :: ListContains n (UserId, User) xs => BlogAction (HVect xs) a
logoutAction =
    do (userId, _ :: User) <- liftM findFirst getContext
       runSQL $ killSessions userId
       writeSession Nothing
       redirect "/"

baseHook :: BlogAction () (HVect '[])
baseHook = return HNil

authHook :: BlogAction (HVect xs) (HVect ((UserId, User) ': xs))
authHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing ->
             noAccessPage "Unknown user. Login first!"
         Just val ->
             return (val :&: oldCtx)

data IsAdmin = IsAdmin

adminHook :: ListContains n (UserId, User) xs => BlogAction (HVect xs) (HVect (IsAdmin ': xs))
adminHook =
    do (_ :: UserId, user) <- liftM findFirst getContext
       oldCtx <- getContext
       if userIsAdmin user then return (IsAdmin :&: oldCtx) else noAccessPage "You don't have enough rights, sorry"

data IsAuthor = IsAuthor

authorHook :: ListContains n (UserId, User) xs => BlogAction (HVect xs) (HVect (IsAuthor ': xs))
authorHook =
    do (_ :: UserId, user) <- liftM findFirst getContext
       oldCtx <- getContext
       if userIsAuthor user then return (IsAuthor :&: oldCtx) else noAccessPage "You don't have enough rights, sorry"

data IsGuest = IsGuest

guestOnlyHook :: BlogAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing -> return (IsGuest :&: oldCtx)
         Just _ -> redirect "/"

noAccessPage :: T.Text -> BlogAction ctx a
noAccessPage msg =
    do setStatus Http.status403
       prefResp <- preferredFormat
       case prefResp of
         PrefJSON ->
             json (CommonError msg)
         _ ->
             mkSite' (panelWithErrorView "No Access" Nothing (toHtml msg))

maybeUser :: (Maybe (UserId, User) -> BlogAction ctx a) -> BlogAction ctx a
maybeUser action =
    do sess <- readSession
       case sess of
         Nothing ->
             action Nothing
         Just sid ->
             do mUser <- runSQL $ loadUser sid
                action mUser
