{-# LANGUAGE NamedFieldPuns #-}

module Core.GameMechanics where

import Core.GameModel

import qualified Data.Vector as V
import Data.List (sort, nub)
import Data.Maybe (isJust)
import Control.Applicative ((<*>), (<$>), pure)

type UnitStates = [Unit]

runCommand :: Command -> GameState -> GameState
runCommand = undefined

initBoard :: GameInput -> Board
initBoard g = let w  = 2 + gameiWidth g
                  h  = 2 + gameiHeight g
                  filled = gameiFilled g
                  vv = V.fromList [ vn
                                  | y <- [-1..(h - 1)],
                                    vn <- [ V.fromList [ Field (isFilled (w - 2, h - 2) (x, y)) (Cell x y) | x <- [-1..(w - 1)] ] ]
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
    applyFilled vec cs =
      let y     = fst (head cs)
          cells = snd (head cs)
      in
       flip applyFilled cs $ vec V.// [(y, (vec V.! y V.// map (\(Cell x y) -> (x, Field True (Cell x y))) cells))]
               
move :: (Unit, UnitStates) -> MoveDirection -> (Unit, UnitStates)
move (unit, states) d =
  (Unit {
    members = V.map (translate d) (members unit),
    pivot   = translate d (pivot unit)
    },
   states)
  where
    translate :: MoveDirection -> Cell -> Cell
    translate E  (Cell x y) = Cell (x + 1) y
    translate W  (Cell x y) = Cell (x - 1) y
    translate SE (Cell x y) = Cell (x + deltaX y + 1) (y + 1)
    translate SW (Cell x y) = Cell (x + deltaX y) (y + 1)

    deltaX y | even y    = 0
             | otherwise = -1

-- TODO:
-- port the code from there: https://github.com/ForNeVeR/icfpc-2015/blob/b53b2675f37e888689181f8e9540b805120c3ab6/src/main/scala/ru/org/codingteam/icfpc/Emulator.scala#L225
--
turn :: (Unit, UnitStates) -> TurnDirection -> (Unit, UnitStates)
turn (unit, state) d = (rotate d unit, state)
  where
    rotate :: TurnDirection -> Unit -> Unit
    rotate d u = u { members = V.map (_rotate (cc d) u) (members u) }
      where
        cc :: TurnDirection -> Double
        cc CW  = -1.0
        cc CCW = 1.0

    _rotate :: Double -> Unit -> Cell -> Cell
    _rotate cc unit (Cell cx cy) =
      let dcx   = fromIntegral cx 
          dcy   = fromIntegral cy
          pvt   = pivot unit
          pvtX  = fromIntegral (x pvt)
          pvtY  = fromIntegral (y pvt)
          v     = cc * (dcy - pvtY)
          u     = dcx - pvtX
          -- frndr = if even cy then ceiling else floor
          
          xx = u - fromIntegral (frndr cy (v / 2.0))
          yy = v
          zz = 0.0 - dcx - dcy

          (rx, ry, rz) = (xx + yy + 0.0, -- x
                          0.0 + yy + zz, -- y
                          xx + 0.0 + zz) -- z
      in
       Cell (round (pvtX + rx + fromIntegral (frndr cy (ry / 2.0)))) (round (pvtY + cc + ry))
      where
        frndr :: (RealFrac a, Integral b) => Int -> (a -> b)
        frndr y | even y    = ceiling
                | otherwise = floor

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
    
hitWall :: (Board, (Unit, UnitStates)) -> Bool
hitWall (board, (unit, states)) = hit' (boardFields board) (members unit)
  where
    hit' :: V.Vector (V.Vector Field) -> V.Vector Cell -> Bool
    hit' fields members = _hit fields members (V.length members)
      where
        _hit fields members 0 = False
        _hit fields members n =
          let mcell = members V.! (n - 1)
              field = fields V.! y mcell V.! x mcell
          in
           filled field


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
