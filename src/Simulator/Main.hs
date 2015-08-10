
module Main where

import Core.JSON
import Core.GameModel
import Core.GameMechanics

import qualified Data.Vector as V
import qualified Data.HashSet as H
import Data.ByteString.Lazy.Char8 (pack, unpack, empty)
import Data.Aeson (encode, decode, eitherDecode)
import Data.Maybe
import Options.Applicative
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

          print unit
          print shifted
          
          let ngs     =
                if hitTest shifted board
                then gs { gamesEnded = True }
                else gs { gamesUnit = shifted, gamesUnits = tail units }
          print (gamesEnded ngs)
          
          case gamesEnded ngs of
            False -> do
              renderASCII ngs
              loopUntilTheEndOfTheWorld ngs
            _     -> return ()
          
    Nothing -> fail "Failed to load problem file"

-- Just for a debug
-- Didn't get why it doesn't print '0'
renderASCII :: GameState -> IO ()
renderASCII gs = do
  let board  = gamesBoard gs
      unit   = gamesUnit gs
      width  = boardWidth board - 2
      height = boardHeight board - 2
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
