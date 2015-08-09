{-# LANGUAGE NamedFieldPuns #-}

module Core.GameMechanics where

import Core.GameModel

import Data.Vector as V
import Data.List (sort)
import Data.Maybe (isJust)
import Control.Applicative ((<*>), (<$>), pure)

type UnitStates = [Unit]

runCommand :: Char -> GameState -> GameState
runCommand = undefined

move :: (Unit, UnitStates) -> MoveDirection -> (Unit, UnitStates)
move (unit, states) d =
  (Unit {
    members = V.map (translate d) (members unit),
    pivot   = (translate d) (pivot unit)
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

turn :: (Unit, UnitStates) -> TurnDirection -> (Unit, UnitStates)
turn = undefined

hit :: Board -> (Unit, UnitStates) -> Bool
hit board ustates = isJust $ hitmyself <$> hitwall (board, ustates)
  where
    hitmyself :: (Unit, UnitStates) -> Bool
    hitmyself (unit, [])   = False
    hitmyself (unit, s:ss) | (pivot unit) == (pivot s) &&
                             hitMembers (members unit) (members s) = True
                           | otherwise = hitmyself (unit, ss)
      where
        hitMembers pm sm = sort (V.toList pm) == sort (V.toList sm)


    hitwall :: (Board, (Unit, UnitStates)) -> Maybe (Unit, UnitStates)
    hitwall (board, (unit, states)) =
      case hit' (boardFields board) (members unit) of
        True  -> Just (unit, states)
        _     -> Nothing
      where
        hit' :: Vector (Vector Field) -> Vector Cell -> Bool
        hit' fields members = _hit fields members (V.length members)
          where
            _hit fields members 0 = False
            _hit fields members n =
              let mcell = members V.! (n - 1)
                  field = fields V.! y mcell V.! x mcell
              in
               filled field
