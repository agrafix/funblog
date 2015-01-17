{-# LANGUAGE OverloadedStrings #-}
module Web.Forms.Common where

import Data.Maybe
import qualified Text.Blaze.Html as H
import Text.Digestive
import qualified Data.Text as T

minMaxLen :: (Int, Int) -> T.Text -> Result H.Html T.Text
minMaxLen (minLen, maxLen) t =
    if len >= minLen && len <= maxLen
    then Success stripped
    else Error $ H.toHtml $ "Must be longer than " ++ show minLen ++ " and shorter than " ++ show maxLen ++ " characters"
    where
      stripped = T.strip t
      len = T.length stripped

usernameFormlet :: Monad m => Maybe T.Text -> Form H.Html m T.Text
usernameFormlet mTxt =
    validate (minMaxLen (3, 12)) (text mTxt)

passwordFormlet :: Monad m => Maybe T.Text -> Form H.Html m T.Text
passwordFormlet mTxt =
    validate (minMaxLen (6, 40)) (text mTxt)

emailFormlet :: Monad m => Maybe T.Text -> Form H.Html m T.Text
emailFormlet mTxt =
    check "Not a valid email address" (isJust . T.find (== '@')) $
    validate (minMaxLen(4, 50)) (text mTxt)
