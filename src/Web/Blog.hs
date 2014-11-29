{-# LANGUAGE OverloadedStrings #-}
module Web.Blog where

import Model.CoreTypes
import Model.ResponseTypes
import Web.Actions.User

import Text.HSmarty
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Database.Persist.Sqlite hiding (get)
import Web.Spock
import Web.Spock.Auth
import Network.Wai.Middleware.Static

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Configurator as C
import qualified Data.HashMap.Strict as HM

type BlogApp = SpockM Connection (VisitorSession () SessionId) BlogCfg ()
type BlogAction a = SpockAction Connection (VisitorSession () SessionId) BlogCfg a

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
    do pool <- createSqlitePool (bcfg_db bcfg) 5
       runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
       spock (bcfg_port bcfg) sessCfg (PCConduitPool pool) bcfg blogApp
    where
      sessCfg =
          authSessCfg (AuthCfg (5 * 60 * 60) ())

runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn

runTpl :: FilePath -> ParamMap -> BlogAction ()
runTpl fp m =
    do bcfg <- getState
       let coreHM =
               HM.fromList [ ("blogName", mkParam (bcfg_name bcfg))
                           , ("blogDesc", mkParam (bcfg_desc bcfg))
                           ]
       res <- liftIO $ renderTemplate fp (coreHM `HM.union` m)
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
           runTpl "templates/main.tpl" HM.empty
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
                        markAsLoggedIn sid
                        json (CommonSuccess "Login okay!")
                 Nothing ->
                     json (CommonError "Login failed.")
       userR GET [] "/logout" $ \(userId, _) ->
           do runSQL $ killSessions userId
              markAsGuest

userR =
    userRoute http403 (runSQL . loadUser) (checkRights)
    where
      checkRights :: (UserId, User) -> [UserRights] -> BlogAction Bool
      checkRights (_, user) rightList =
          if "admin" `elem` rightList
          then return $ userIsAdmin user
          else if "author" `elem` rightList
               then return $ or [ userIsAdmin user
                                , userIsAuthor user
                                ]
               else return True
      http403 ty =
          do setStatus Http.status403
             let txt =
                     case ty of
                       NotEnoughRights ->
                           "Not enough rights to view the page"
                       NotLoggedIn ->
                           "You need to be logged in to view the page"
                       NotValidUser ->
                           "No valid user found"
             json (CommonError txt)
