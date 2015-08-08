{-# LANGUAGE DeriveDataTypeable, DeriveGeneric  #-}

module Core.GameModel where

import Data.HashSet
import Data.Aeson
import GHC.Generics (Generic)
import Data.Aeson.Types

data Cell = Cell {
    x :: Int
  , y :: Int
  } deriving (Show, Eq, Generic)

data Unit = Unit {
    members :: [Cell]
  , pivot :: Cell
  } deriving (Show, Eq, Generic)

data Board = Board {
    boardFilled :: [Cell]
  , boardWidth  :: Int
  , boardHeight :: Int
  } deriving (Show, Eq)

data GameState = GameState {
    board  :: Board
  , cunit  :: Unit -- current unit
  , runits :: [Unit] -- rest units
  , score  :: Int
  } deriving (Show, Eq)

data GameInput = GameInput {
    gameiId           :: Int
  , gameiUnits        :: [Unit]
  , gameiWidth        :: Int
  , gameiHeight       :: Int
  , gameiFilled       :: [Cell]
  , gameiSourceLength :: Int
  , gameiSourceSeeds  :: [Int]
  } deriving (Show, Generic) 

data GameOutput = GameOutput {
    gameoProblemId :: Int
  , gameoSeed      :: Int
  , gameoTag       :: String
  , gameoCommands  :: [Char]
  } deriving (Show, Generic)
             
