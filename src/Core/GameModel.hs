{-# LANGUAGE DeriveDataTypeable, DeriveGeneric  #-}

module Core.GameModel where

import GHC.Generics (Generic)
import Data.Vector

data Cell = Cell {
    x :: Int
  , y :: Int
  } deriving (Show, Eq, Generic, Ord)

data Unit = Unit {
    members :: Vector Cell
  , pivot :: Cell
  } deriving (Show, Eq, Generic)

data Field = Field { filled :: Bool, cell :: Cell } deriving (Show, Eq)

data Board = Board {
    boardFields :: Vector (Vector Field)
  , boardWidth  :: Int
  , boardHeight :: Int
  } deriving (Show, Eq)

data GameState = GameState {
    gamesBoard  :: Board
  , gamesUnit   :: Unit -- current unit
  , gamesUnits  :: [Unit] -- rest units
  , gamesScore  :: Int
  , gamesEnded  :: Bool
  } deriving (Show, Eq)

data MoveDirection = E | W | SE | SW deriving (Show, Eq, Ord)
data TurnDirection = CW | CCW deriving (Show, Eq, Ord)
data Command = Move MoveDirection
             | Turn TurnDirection
             | Ignore
             | Invalid -- facing this command game ends and score becomes 0
             deriving (Show, Eq, Ord)

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

             
