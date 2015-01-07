{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Blog where

import Model.CoreTypes
import Model.ResponseTypes
import Web.Actions.User

import Text.HSmarty
import Control.Concurrent.STM
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Database.Persist.Sqlite hiding (get)
import Web.Spock.Safe hiding (SessionId)
import Network.Wai.Middleware.Static

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Configurator as C
import qualified Data.HashMap.Strict as HM

type SessionVal = Maybe SessionId
type BlogApp = SpockM SqlBackend SessionVal BlogState ()
type BlogAction a = SpockAction SqlBackend SessionVal BlogState a

data BlogState
   = BlogState
   { bs_cfg :: BlogCfg
   , bs_templates :: TVar (HM.HashMap FilePath SmartyCtx)
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
       newHm <- newTVarIO HM.empty
       runSpock (bcfg_port bcfg) $ spock sessCfg (PCPool pool) (BlogState bcfg newHm) blogApp
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

runTpl :: Maybe User -> FilePath -> ParamMap -> BlogAction ()
runTpl mUser fp m =
    do blogSt <- getState
       let bcfg = bs_cfg blogSt
           coreHM =
               HM.fromList [ ("blogName", mkParam (bcfg_name bcfg))
                           , ("blogDesc", mkParam (bcfg_desc bcfg))
                           , ("user", mkParam mUser)
                           ]
       ctxHm <-
           liftIO $ atomically $ readTVar (bs_templates blogSt)
       renderCtx <-
           case HM.lookup fp ctxHm of
             Nothing ->
                 liftIO $
                 do newCtx <- prepareTemplate fp
                    atomically $ modifyTVar (bs_templates blogSt) (HM.insert fp newCtx)
                    return newCtx
             Just ctx ->
                 return ctx
       res <- liftIO $ applyTemplate renderCtx (coreHM `HM.union` m)
       case res of
         Left err ->
             do setStatus Http.status403
                liftIO $ putStrLn $ "Template Error: " ++ show err
                text "Internal Server Error!"
         Right h ->
             html h

blogApp :: BlogApp
blogApp =
    do middleware (staticPolicy (addBase "static"))
       get "/" $
           runTpl Nothing "templates/main.tpl" HM.empty
       get "/login" $
           runTpl Nothing "templates/login.tpl" $ HM.fromList [("error", mkParam (Nothing :: Maybe String))]
       post "/register" $
            do Just username <- param "username"
               Just email <- param "email"
               Just password <- param "password"
               (runSQL $ registerUser username email password) >>= json
       post "/login" $
            do Just username <- param "username"
               Just password <- param "password"
               loginRes <- runSQL $ loginUser username password
               case loginRes of
                 Just userId ->
                     do sid <- runSQL $ createSession userId
                        writeSession (Just sid)
                        json (CommonSuccess "Login okay!")
                 Nothing ->
                     json (CommonError "Login failed.")
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
