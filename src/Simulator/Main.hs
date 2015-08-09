
module Main where

import Core.JSON
import Core.GameModel
import Core.GameMechanics

import Data.ByteString.Lazy.Char8 (pack, unpack, empty)
import Data.Aeson (encode, decode, eitherDecode)
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Maybe

data Flag = File String
        --  | Index (Maybe String)
          deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['f'] [] (ReqArg File "filename") "Simulate using a file contains JSON encoded input"
  --, Option ['i'] [] (OptArg Index "command_index") "Execute number of commands up to the index"
  ]

simulate :: [Char] -> GameInput -> [GameState]
simulate commands ginput = [GameState {}]
  
loadProblem :: String -> IO (Maybe GameInput)
loadProblem file = do
  json <- readFile file
  return (decode (pack json) :: Maybe GameInput)

main :: IO ()
main = do
  args <- getArgs

  opts <- case getOpt RequireOrder options args of
    ([],[],errs) -> fail $ usageInfo "Usage: simulator [OPTIONS]" options
    (o,n,  []) -> return (o,n)
    

  let file = head $ map (\(File s) -> s) $ fst opts
      -- index = flip filter (fst opts) $ \flag -> case flag of
      --   Index _ -> True
      --   _       -> False
      commands = head $ snd opts
  
  ginput <- loadProblem file
  case ginput of
    Just g -> print (simulate commands g)
    _      -> fail "Error: Couldn't load a problem"
  
  -- let gi = GameInput {
  --         gameiId = 1
  --       , gameiUnits = [Unit { members = [Cell { x = 1, y = 1 }], pivot = Cell { x = 0, y = 0} }]
  --       , gameiWidth = 5
  --       , gameiHeight = 10
  --       , gameiFilled = []
  --       , gameiSourceLength = 1
  --       , gameiSourceSeeds = [12, 9120]
  --       }

  -- tests
  
  -- let gi = "{ \"id\": 1, \"units\": [{ \"members\": [{ \"x\": 1, \"y\": 1}], \"pivot\": { \"x\": 0, \"y\": 0} }], \"width\": 5, \"height\": 10, \"filled\": [], \"sourceLength\": 1, \"sourceSeeds\": [] }"

  -- print (case (eitherDecode (pack gi) :: Either String GameInput) of
  --           Left  e -> error e
  --           Right g -> g)
  
  -- let go = GameOutput {
  --         gameoProblemId = 1
  --       , gameoSeed = 12
  --       , gameoTag = "asdf"
  --       , gameoCommands = ['c','b','a','!']
  --       }

  
  -- putStrLn $ unpack $ encode go


  
  
