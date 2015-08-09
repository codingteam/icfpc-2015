
module Core.Commands where

-- TODO: support phrases of power

import Core.GameModel

import Data.Map as Map hiding (map)

dict :: Map Command String
dict = Map.fromList [(Move W,   "p'!.03"),
                     (Move E,   "bcefy2"),
                     (Move SW,  "aghij4"),
                     (Move SE,  "lmno 5"),
                     (Turn CW,  "dqrvz1"),
                     (Turn CCW, "kstuwx")]

undict :: Map Char Command
undict = Map.fromList $ concat $ map fn (Map.toList dict)
  where
    fn :: (Command, String) -> [(Char, Command)]
    fn (com, letters) = map (\letter -> (letter, com)) letters

-- decode :: String -> [Command]
-- decode cmds 
