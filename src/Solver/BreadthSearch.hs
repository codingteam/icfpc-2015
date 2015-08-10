{-# LANGUAGE DeriveGeneric, TypeOperators, KindSignatures, DataKinds #-}

module Solver.BreadthSearch where

import qualified Data.HashMap.Lazy as H
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Data.List (findIndices)
import Data.Function (on)

import Control.Applicative
import Control.Arrow

import Core

type CommandHistory = [Command]

data PhaseState = PhaseState {
	offsetX :: !Int,
	offsetY :: !Int,
	rotation :: !Int,
	symmetry :: !Int
}	deriving (Eq, Generic, Show)
instance Hashable PhaseState


unitSymmetry :: Unit -> Int
unitSymmetry u = findIndices (==u) (rotateUnit <$> [0..] <*> pure u) !! 1


type PossibleMoves = H.HashMap PhaseState CommandHistory
calcUnitMoves :: Board -> Unit -> [(Unit, CommandHistory)]
calcUnitMoves board unit = map (unitPhase unit *** id) $ H.toList $ snd $ go (start, start)
	where
		start = H.singleton (PhaseState 0 0 0 (unitSymmetry unit)) []

		go :: (PossibleMoves, PossibleMoves) -> (PossibleMoves, PossibleMoves)
		go (prevmoves, allmoves) = if H.null prevmoves then (prevmoves, allmoves) else go newstate
			where
				moves = H.filterWithKey (\k _ -> not $ H.member k allmoves)
					  $ H.filterWithKey (\k _ -> not $ hitTest (unitPhase unit k) board)
					  $ H.fromList $ concatMap nextStates $ H.toList prevmoves

				newstate = (moves, H.union allmoves moves)

		nextStates :: (PhaseState, CommandHistory) -> [(PhaseState, CommandHistory)]
		nextStates (s, h) = zip states histories
			where
				cmds = [Move E, Move W, Move SE, Move SW, Turn CW, Turn CCW]
				states = phaseCmd (y(pivot unit)`mod`2) <$> cmds <*> pure s
				histories = (:h) <$> cmds

type Parity = Int

phaseCmd :: Parity -> Command -> PhaseState -> PhaseState
phaseCmd _ (Move E) s = s { offsetX = offsetX s - 1 }
phaseCmd _ (Move W) s = s { offsetX = offsetX s + 1 }
phaseCmd p (Move SE) s = s { offsetX = offsetX s     - ((p+offsetY s)`mod`2), offsetY = 1 + offsetY s}
phaseCmd p (Move SW) s = s { offsetX = offsetX s + 1 - ((p+offsetY s)`mod`2), offsetY = 1 + offsetY s}
phaseCmd _ (Turn CW) s = s { rotation = (rotation s - 1) `mod` symmetry s }
phaseCmd _ (Turn CCW) s = s { rotation = (rotation s + 1) `mod` symmetry s }

unitPhase :: Unit -> PhaseState -> Unit
unitPhase u s = rotateUnit (rotation s) . translateUnit (offsetX s, offsetY s) $ u

