{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Blog where

import Model.CoreTypes
import Model.ResponseTypes
import Web.Actions.User
import Web.Forms.Login
import Web.Utils
import Web.Views.Home
import Web.Views.Login
import Web.Views.Site

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Monoid
import Database.Persist.Sqlite hiding (get)
import Network.Wai.Middleware.Static
import Text.Blaze.Html (Html)
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
          }

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn

mkSite :: Maybe User -> (SiteView -> Html) -> BlogAction ()
mkSite mUser content =
    do blogSt <- getState
       let cfg = bs_cfg blogSt
           sv =
               SiteView
               { sv_blogName = bcfg_name cfg
               , sv_blogDesc = bcfg_desc cfg
               , sv_user = mUser
               }
       blaze $ siteView sv (content sv)

mkSite' :: Maybe User -> Html -> BlogAction ()
mkSite' mUser content = mkSite mUser (const content)

getpost url action =
    do get url action
       post url action

blogApp :: BlogApp
blogApp =
    do middleware (staticPolicy (addBase "static"))
       get "/" $
           mkSite Nothing (\sv -> homeView sv)
       getpost "/login" $
           do f <- runForm "loginForm" loginForm
              case f of
                (view, Nothing) ->
                    mkSite' Nothing (loginView Nothing $ renderForm loginFormSpec view)
                (view, Just loginReq) ->
                    do loginRes <- runSQL $ loginUser (lr_user loginReq) (lr_password loginReq)
                       case loginRes of
                         Just userId ->
                             do sid <- runSQL $ createSession userId
                                writeSession (Just sid)
                                redirect "/"
                         Nothing ->
                             mkSite' Nothing (loginView (Just "Invalid login credentials!") $ renderForm loginFormSpec view)
       get "/about" $
           mkSite Nothing mempty
       post "/register" $
            do Just username <- param "username"
               Just email <- param "email"
               Just password <- param "password"
               (runSQL $ registerUser username email password) >>= json
       get "/logout" $ requireUser $ \(userId, _) ->
           do runSQL $ killSessions userId
              writeSession Nothing
              redirect "/"

requireUser :: ((UserId, User) -> BlogAction a) -> BlogAction a
requireUser action =
    do sess <- readSession
       case sess of
         Nothing ->
             do setStatus Http.status403
                json (CommonError "Not logged in")
         Just sid ->
             do mUser <- runSQL $ loadUser sid
                case mUser of
                  Nothing ->
                      do setStatus Http.status403
                         json (CommonError "Invalid user")
                  Just userTuple ->
                      action userTuple
