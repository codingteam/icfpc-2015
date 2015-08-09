
module Main where

import Core.JSON
import Core.GameModel
import Core.GameMechanics

import qualified Data.Vector as V
import Data.ByteString.Lazy.Char8 (pack, unpack, empty)
import Data.Aeson (encode, decode, eitherDecode)
import Data.Maybe
import Options.Applicative

simulate :: [Char] -> Maybe Int -> GameInput -> [GameState]
simulate commands index ginput = [GameState {}]
                                 
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
    Just g ->
      let board = initBoard g in
      printBoard board
    Nothing -> fail "Failed to load problem file"

printBoard :: Board -> IO ()
printBoard board =
  V.mapM_ (\v ->
          V.mapM_ (\f -> let c = if filled f then 'X' else '@' in putChar c) v
          >> putStr "\n") $ boardFields board
       

main :: IO ()
main = execParser opts >>= runOpts
  where
    opts = info (helper <*> options) (   fullDesc
                                      <> progDesc "The Old One's Simulator"
                                      <> header "simulator - simulate it!" )
