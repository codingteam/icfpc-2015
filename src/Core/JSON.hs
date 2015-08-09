{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Core.JSON where

import Core.GameModel

import Data.Aeson
import GHC.Generics (Generic)
import Data.Aeson.Types
import Data.Char
import Data.List.Split

jsonModifier :: String -> (String -> String)
jsonModifier prefix = modify
  where modify s = let wop = splitOn prefix s !! 1 in
          toLower (head wop) : tail wop

instance FromJSON Cell where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = id
    }

instance FromJSON Unit where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = id
    }

-- instance FromJSON Unit where
--   parseJSON unit = do
    

instance FromJSON GameInput where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = jsonModifier "gamei"
    }

instance ToJSON GameOutput where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = jsonModifier "gameo"
    }
