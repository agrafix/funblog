{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.CoreTypes where

import Database.Persist.TH
import Data.Time

import qualified Data.Text as T
import qualified Data.ByteString as BS

share [mkPersist sqlSettings, mkMigrate "migrateCore"] [persistLowerCase|
Session
     validUntil UTCTime
     userId UserId
     deriving Show
User
     name T.Text
     password BS.ByteString
     salt BS.ByteString
     email T.Text
     isAuthor Bool
     isAdmin Bool
     UniqueUsername name
     UniqueEmail email
     deriving Show
Post
     title T.Text
     date UTCTime
     content T.Text
     deriving Show
PostTags
     postId PostId
     tagId TagId
     UniquePostTag postId tagId
     deriving Show
Tag
     tag T.Text
     UniqueTag tag
     deriving Show
Comment
     author T.Text
     email T.Text Maybe
     content T.Text
     post PostId
     deriving Show
|]
