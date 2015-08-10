
module Main where

import Core.JSON
import Core.GameModel
import Core.GameMechanics
import Solver.Solver

import qualified Data.Vector as V
import qualified Data.HashSet as H
import qualified Data.HashMap.Lazy as M
import Data.ByteString.Lazy.Char8 (pack, unpack, empty)
import Data.Aeson (encode, decode, eitherDecode)
import Data.Maybe
import Data.List
import Data.Function (on)
import Options.Applicative
import Control.Applicative
import Text.Printf (printf)
                                 
loadProblem :: String -> IO (Maybe GameInput)
loadProblem file = do
  json <- readFile file
  return (decode (pack json) :: Maybe GameInput)

data SimOptions = SimOptions {
    file :: String
  , step :: Maybe String
  }

options :: Parser SimOptions
options = SimOptions
          <$> strOption ( short 'f' <> metavar "FILENAME" <> help "File with a problem" )
          <*> optional (strOption ( short 'i' <> metavar "STEP" <> help "Run until N step" ))
  
-- Here we go!
runOpts :: SimOptions -> IO ()
runOpts (SimOptions file step) = do
  maybeGameInput <- loadProblem file

  case maybeGameInput of
    Nothing -> fail "Failed to load problem file"
    Just g -> do
      let units = gameiUnits g
          gs    = GameState { gamesBoard = initBoard g
                            , gamesUnit  = head units
                            , gamesUnits = tail units
                            , gamesScore = 0
                            , gamesEnded = False
                            }
      
      renderASCII gs -- initial rendering
      loopUntilTheEndOfTheWorld gs
        
      where
        loopUntilTheEndOfTheWorld :: GameState -> IO ()
        loopUntilTheEndOfTheWorld gs = do
          let unit    = gamesUnit gs
              board   = gamesBoard gs
              units   = gamesUnits gs
              shifted = shiftSpawnedUnit (gamesUnit gs) (boardWidth board)

          let moves = calcUnitMoves (gamesBoard gs) shifted


          case sortBy (flip compare `on` unitBottom . fst) moves of
            []      -> undefined -- no moves
            (u,h):_ -> print ("Move: " ++ show (u,h))

          --loopUntilTheEndOfTheWorld gs
          

-- Just for a debug
-- Didn't get why it doesn't print '0'
renderASCII :: GameState -> IO ()
renderASCII gs = do
  let board  = gamesBoard gs
      unit   = gamesUnit gs
      width  = boardWidth board
      height = boardHeight board
      filled = filledCells board

  printf "Size: %dx%d\n\n" width height

  putStr "   "
  for [1..width] $ \x -> printf "%3.d" (x - 1)
  putStr "\n\n"
  
  for [0..height-1] $ \y -> do
             printf "%-3.d" y
             if even y then putChar ' ' else return ()
             for [0..width-1] $ \x -> 
                printf " %c " (if H.member (Cell x y) filled then '@' else '.')
             putStr "\n"

  putStr "\n     ------------------------------------ \n\n"

for = flip mapM_

main :: IO ()
main = execParser opts >>= runOpts
  where
    opts = info (helper <*> options) (   fullDesc
                                      <> progDesc "The Old One's Simulator"
                                      <> header "simulator - simulate it!" )
