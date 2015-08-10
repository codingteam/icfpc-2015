{-# LANGUAGE DeriveDataTypeable, DeriveGeneric  #-}

module Core.GameModel where

import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.HashSet as H
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)

data Cell = Cell {
    x :: Int
  , y :: Int
  } deriving (Show, Eq, Generic, Ord)
instance Hashable Cell

data Unit = Unit {
    members :: H.HashSet Cell
  , pivot :: Cell
  } deriving (Show, Eq, Generic)

data Field = Field { filled :: Bool, cell :: Cell } deriving (Show, Eq)

data Board = Board {
    filledCells  :: H.HashSet Cell
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


overCells :: (Cell -> Cell) -> Unit -> Unit
overCells f u = Unit { members = H.map f (members u), pivot = f (pivot u) }

translateUnit :: (Int, Int) -> Unit -> Unit
translateUnit delta = overCells (translateCell delta)

translateCell :: (Int, Int) -> Cell -> Cell
translateCell (deltaX, deltaY) c = Cell { x = x c + deltaX, y = y c + deltaY }

rotateUnit :: Int -> Unit -> Unit
rotateUnit angleCCW
  | angleCCW < 0   = vmirrorUnit . rotateUnit (abs angleCCW) . vmirrorUnit
  | angleCCW == 0  = id
  | otherwise      = \unit -> (overCells $ rotateCell (pivot unit) angleCCW) $ unit

rotateCell :: Cell -> Int -> Cell -> Cell
rotateCell pivot angleCCW cell= Cell {x = ru, y = rv}
  where
    u = x cell - x pivot
    v = y cell - y pivot

    xx = u - half v
    yy = v
    zz = 0 - xx - yy

    (rx, ry, rz) = iterate transform (xx, yy, zz) !! angleCCW

    transform (x, y, z) = (x', y', z') where
      x' = x + y + 0
      y' = 0 + y + z
      z' = x + 0 + z

    ru = x pivot + rx + half ry
    rv = y pivot + ry

    half arg | y pivot `mod` 2 == 0   = arg `div` 2
             | otherwise              = (arg + 1) `div` 2

vmirrorUnit :: Unit -> Unit
vmirrorUnit = overCells vmirrorCell

vmirrorCell :: Cell -> Cell
vmirrorCell c = Cell {x = x c, y = 0 - y c}


unitBottom :: Unit -> Int
unitBottom u = maximum . map y . H.toList . members $ u
