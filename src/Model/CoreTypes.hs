{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Model.CoreTypes where

import Database.Persist.TH
import Data.Time

import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateCore"] [persistLowerCase|
Session
     validUntil UTCTime
     userId UserId
     deriving Show
User json
     name T.Text
     password T.Text
     salt T.Text
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
