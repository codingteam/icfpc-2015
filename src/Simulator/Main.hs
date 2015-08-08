
module Main where

import Core.JSON
import Core.GameModel

import Data.ByteString.Lazy.Char8 (pack, unpack, empty)
import Data.Aeson (encode, decode, eitherDecode)

main :: IO ()
main = do
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
  
  let gi = "{ \"id\": 1, \"units\": [{ \"members\": [{ \"x\": 1, \"y\": 1}], \"pivot\": { \"x\": 0, \"y\": 0} }], \"width\": 5, \"height\": 10, \"filled\": [], \"sourceLength\": 1, \"sourceSeeds\": [] }"

  print (case (eitherDecode (pack gi) :: Either String GameInput) of
            Left  e -> error e
            Right g -> g)
  
  let go = GameOutput {
          gameoProblemId = 1
        , gameoSeed = 12
        , gameoTag = "asdf"
        , gameoCommands = ['c','b','a','!']
        }

  
  putStrLn $ unpack $ encode go

