{-# LANGUAGE NamedFieldPuns #-}

module Core.GameMechanics where

import Core.GameModel

import qualified Data.Vector as V
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


--
-- 02:01:02< jjourdan> there are 3 kinds of errors :
-- 02:01:17< jjourdan> 1- too long chain of commands
-- 02:01:23< jjourdan> 2- invalid command
-- 02:01:33< jjourdan> 3- repeating configuration
--
-- Notice!
-- hitmyself will end the game if the result is True
    
hitConfigs :: (Unit, UnitStates) -> Bool
hitConfigs (unit, [])   = False
hitConfigs (unit, s:ss) | (pivot unit) == (pivot s) &&
                          hitMembers (members unit) (members s) = True
                        | otherwise = hitConfigs (unit, ss)
  where
    hitMembers pm sm = sort (V.toList pm) == sort (V.toList sm)

    
hitWall :: Unit -> Board -> Bool
hitWall unit board = hit' (boardFields board) (members unit)
  where
    hit' :: V.Vector (V.Vector Field) -> V.Vector Cell -> Bool
    hit' fields members = _hit fields members (V.length members)
      where
        _hit fields members 0 = False
        _hit fields members n =
          let mcell = members V.! (n - 1)
              field = fields V.! (y mcell) V.! (x mcell)
          in
           case filled field of
             False -> _hit fields members (n - 1)
             _     -> True
    

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
