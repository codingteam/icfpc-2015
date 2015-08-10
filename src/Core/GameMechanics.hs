{-# LANGUAGE NamedFieldPuns #-}

module Core.GameMechanics where

import Core.GameModel

import qualified Data.Vector as V
import qualified Data.HashSet as H
import Data.Monoid
import Data.List (sort, nub)
import Data.Maybe (isJust)
import Control.Applicative ((<*>), (<$>), pure)

initBoard :: GameInput -> Board
initBoard g = let w  = 2 + gameiWidth g
                  h  = 2 + gameiHeight g
                  filled = gameiFilled g
                  vv = V.fromList [ vn
                                  | y <- [-1..(h - 2)],
                                    vn <- [ V.fromList [ Field (isFilled (w - 2, h - 2) (x, y)) (Cell x y) | x <- [-1..(w - 2)] ] ]
                                  ]
              in
               Board (applyFilled vv [ (yy, cells)
                                     | yy <- nub (map (\cell -> y cell) filled),
                                       cells <- [ filter (\(Cell _ y) -> y == yy) filled ] ]) w h
  where
    isFilled (w, h) (x, y) | x < 0 || y < 0 || x >= w || y >= h = True
                           | otherwise                      = False
                                                              
    applyFilled :: V.Vector (V.Vector Field) -> [(Int, [Cell])] -> V.Vector (V.Vector Field)
    applyFilled vec []   = vec
    applyFilled vec ((y,cells):cs) =
      flip applyFilled cs $ vec V.// [(y + 1, (vec V.! (y + 1) V.// map (\(Cell x y) -> (x + 1, Field True (Cell x y))) cells))]


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

    
flattenBoard :: Board -> [Cell]
flattenBoard = (map cell) . concat . (map V.toList) . V.toList . boardFields

hitTest :: Unit -> Board -> Bool
hitTest unit board = any isFilledOnBoard (H.toList (members unit))
  where
    isFilledOnBoard :: Cell -> Bool
    isFilledOnBoard c
      | x c < 0      = True
      | y c < 0      = True
      | x c >= boardWidth board = True
      | y c >= boardHeight board = True
      | otherwise = not . null $ filter (==c) (flattenBoard board)


-- hit :: Board -> (Unit, UnitStates) -> Bool
-- hit board ustates = isJust $ hitmyself <$> hitwall (board, ustates)
--   where
--     hitmyself :: (Unit, UnitStates) -> Bool
--     hitmyself (unit, [])   = False
--     hitmyself (unit, s:ss) | (pivot unit) == (pivot s) &&
--                              hitMembers (members unit) (members s) = True
--                            | otherwise = hitmyself (unit, ss)
--       where
--         hitMembers pm sm = sort (V.toList pm) == sort (V.toList sm)


--     hitwall :: (Board, (Unit, UnitStates)) -> Maybe (Unit, UnitStates)
--     hitwall (board, (unit, states)) =
--       case hit' (boardFields board) (members unit) of
--         True  -> Just (unit, states)
--         _     -> Nothing
--       where
--         hit' :: V.Vector (V.Vector Field) -> V.Vector Cell -> Bool
--         hit' fields members = _hit fields members (V.length members)
--           where
--             _hit fields members 0 = False
--             _hit fields members n =
--               let mcell = members V.! (n - 1)
--                   field = fields V.! y mcell V.! x mcell
--               in
--                filled field
