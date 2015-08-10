{-# LANGUAGE DeriveGeneric, TypeOperators, KindSignatures, DataKinds #-}

module Solver.BreadthSearch where

import qualified Data.HashMap.Lazy as H
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Data.List (findIndices)

import Control.Monad.State
import Control.Applicative

import Core

type CommandHistory = [Command]

data PhaseState = PhaseState {
	offsetX :: !Int,
	offsetY :: !Int,
	rotation :: !Int,
	symmetry :: !Int
}	deriving (Eq, Generic)
instance Hashable PhaseState


unitSymmetry :: Unit -> Int
unitSymmetry u = findIndices (==u) (rotateUnit <$> [0..] <*> pure u) !! 1


type PossibleMoves = H.HashMap PhaseState CommandHistory
calcUnitMoves :: Board -> Unit -> PossibleMoves
calcUnitMoves board unit = snd $ execState go (start, start)
	where
		start = H.singleton (PhaseState 0 0 0 (unitSymmetry unit)) []

		go :: State (PossibleMoves, PossibleMoves) ()
		go = do
			(prevmoves, allmoves) <- get

			let moves = H.filterWithKey (\k _ -> not $ H.member k allmoves)
			          $ H.filterWithKey (\k _ -> not $ hitTest (unitPhase k unit) board)
				      $ H.fromList $ concatMap nextStates $ H.toList prevmoves

			put (moves, H.union allmoves moves)

			if H.null moves then return () else go

		nextStates :: (PhaseState, CommandHistory) -> [(PhaseState, CommandHistory)]
		nextStates (s, h) = zip states histories
			where
				cmds = [Move E, Move W, Move SE, Move SW, Turn CW, Turn CCW]
				states = phaseCmd <$> cmds <*> pure s
				histories = (:h) <$> cmds


phaseCmd :: Command -> PhaseState -> PhaseState
phaseCmd (Move E) s = s { offsetX = offsetX s - 1 }
phaseCmd (Move W) s = s { offsetX = offsetX s + 1 }
phaseCmd (Move SE) s = s { offsetX = offsetX s - 1, offsetY = 1 + offsetY s}
phaseCmd (Move SW) s = s { offsetX = offsetX s + 1, offsetY = 1 + offsetY s}
phaseCmd (Turn CW) s = s { rotation = (rotation s - 1) `mod` symmetry s }
phaseCmd (Turn CCW) s = s { rotation = (rotation s + 1) `mod` symmetry s }

unitPhase :: PhaseState -> Unit -> Unit
unitPhase s = rotateUnit (rotation s) . translateUnit (offsetX s, offsetY s)

