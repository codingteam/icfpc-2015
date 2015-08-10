{-# LANGUAGE NamedFieldPuns #-}

module Core.GameMechanics where

import Core.GameModel

import qualified Data.Vector as V
import qualified Data.HashSet as H
import qualified Data.HashMap.Strict as M
import Data.Monoid
import Data.List (sort, nub)
import Data.Maybe (isJust)
import Control.Applicative ((<*>), (<$>), pure)

initBoard :: GameInput -> Board
initBoard g = Board { boardWidth = w, boardHeight = h, filledCells = cells }
  where
    w = gameiWidth g
    h = gameiHeight g
    cells = H.fromList $ gameiFilled g


shiftSpawnedUnit :: Unit -> Int -> Unit
shiftSpawnedUnit unit boardW =
  let ms    = members unit
      pvt   = pivot unit
      msx   = H.map (\(Cell x _) -> x) ms
      uminX = minimum (H.toList msx)
      umaxX = maximum (H.toList msx)
      uw    = umaxX - uminX + 1
      shift = (boardW - uw) `div` 2
  in
   unit {
     members = H.map (\(Cell x y) -> Cell (x + shift) y) ms,
     pivot   = Cell ((x pvt) + shift) (y pvt)
     }


--
-- 02:01:02< jjourdan> there are 3 kinds of errors :
-- 02:01:17< jjourdan> 1- too long chain of commands
-- 02:01:23< jjourdan> 2- invalid command
-- 02:01:33< jjourdan> 3- repeating configuration
--
-- Notice!
-- hitmyself will end the game if the result is True

    
hitTest :: Unit -> Board -> Bool
hitTest unit board = any isFilledOnBoard (H.toList (members unit))
  where
    isFilledOnBoard :: Cell -> Bool
    isFilledOnBoard c
      | x c < 0      = True
      | y c < 0      = True
      | x c >= boardWidth board = True
      | y c >= boardHeight board = True
      | otherwise = not . H.member c . filledCells $ board
