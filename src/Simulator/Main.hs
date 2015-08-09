
module Main where

import Core.JSON
import Core.GameModel
import Core.GameMechanics

import qualified Data.Vector as V
import Data.ByteString.Lazy.Char8 (pack, unpack, empty)
import Data.Aeson (encode, decode, eitherDecode)
import Data.Maybe
import Options.Applicative
import Text.Printf (printf)

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

-- Just for a debug
-- Didn't get why it doesn't print '0'
printBoard :: Board -> IO ()
printBoard board = do
  let width  = boardWidth board - 2
      height = boardHeight board - 2
      fields = boardFields board

  printf "Size: %dx%d\n\n" width height

  let v0 = fields V.! 0
      
  putStr "   "
  imapM_ (\x _ -> printf "%3.d" (x - 1)) $ V.slice 0 (V.length v0) v0
  putStr "\n\n"
  
  imapM_ (\y v -> printf "%-3.d" (y - 1) >>
          V.mapM_ (\f -> printf " %c " (if filled f then '@' else '.')) v
          >> putStr "\n") $ V.slice 0 (V.length fields) fields

  putStr "\n     ------------------------------------ \n\n"
    
  where
    imapM_ :: Monad m => (Int -> a -> m b) -> V.Vector a -> m ()
    imapM_ fn v = mapM_ (uncurry fn) $ zip [0 .. V.length v] (V.toList v)
  

main :: IO ()
main = execParser opts >>= runOpts
  where
    opts = info (helper <*> options) (   fullDesc
                                      <> progDesc "The Old One's Simulator"
                                      <> header "simulator - simulate it!" )
