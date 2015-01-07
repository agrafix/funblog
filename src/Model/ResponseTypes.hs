{-# LANGUAGE TemplateHaskell #-}
module Model.ResponseTypes where

import Data.Aeson.TH
import qualified Data.Text as T

dropLbl :: Int -> Options
dropLbl i =
    defaultOptions { fieldLabelModifier = drop i }

data CommonResponse
   = CommonError T.Text
   | CommonSuccess T.Text
   deriving (Show, Eq)


$(deriveJSON defaultOptions ''CommonResponse)
