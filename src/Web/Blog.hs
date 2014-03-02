{-# LANGUAGE OverloadedStrings #-}
module Web.Blog where

import Model.CoreTypes
import Model.ResponseTypes
import Web.Actions.User

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.Postgresql
import Web.Spock

import qualified Network.HTTP.Types.Status as Http
import qualified Data.Configurator as C

type BlogApp = SpockM Connection SessionId () ()
type BlogAction a = SpockAction Connection SessionId () a

parseConfig :: FilePath -> IO (ConnectionString, Int)
parseConfig cfgFile =
    do cfg <- C.load [C.Required "blog.cfg"]
       connStr <- C.require cfg "pgString"
       port <- C.require cfg "port"
       return (connStr, port)

runBlog :: ConnectionString -> Int -> IO ()
runBlog connStr port =
    do pool <- createPostgresqlPool connStr 5
       runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
       spock port sessCfg (PCConduitPool pool) () blogApp
    where
      sessCfg =
          SessionCfg "funblog" (5 * 60 * 60) 40

runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn

blogApp :: BlogApp
blogApp =
    do post "/register" $
            do username <- param "username"
               email <- param "email"
               password <- param "password"
               (runSQL $ registerUser username email password) >>= json
       post "/login" $
            do username <- param "username"
               password <- param "password"
               loginRes <- runSQL $ loginUser username password
               case loginRes of
                 Just userId ->
                     do sid <- runSQL $ createSession userId
                        authedUser userId (const sid)
                        json (CommonSuccess "Login okay!")
                 Nothing ->
                     json (CommonError "Login failed.")
       userR GET [] "/logout" $ \(userId, _) ->
           do runSQL $ killSessions userId
              unauthCurrent

userR =
    authed http403 (runSQL . loadUser) (checkRights)
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
          do status Http.status403
             let txt =
                     case ty of
                       NotEnoughRights ->
                           "Not enough rights to view the page"
                       NotLoggedIn ->
                           "You need to be logged in to view the page"
                       NoSession ->
                           "No valid session found"
             json (CommonError txt)
