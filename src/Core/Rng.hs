module Core.Rng
       (
         HexRndGen(..)
       , randomlist
       , randomlistR
       ) where

import Data.Int (Int32)
import Data.Bits

import Control.Applicative ((<$>))

import System.Random (Random(..), RandomGen(..))

import Test.QuickCheck hiding ((.&.)) -- I just like the face


data HexRndGen = HexRndGen !Int32 deriving (Show, Eq, Bounded)

_multiplier = 1103515245
_increment = 12345

instance RandomGen HexRndGen where
    genRange _ = (0, 0x7FFF)

    next (HexRndGen s) = (fromEnum outbits, s')
        where
            s' = HexRndGen (s * _multiplier + _increment)

            outbits = (s .&. 0x7FFF0000) `shiftR` 16

    split (HexRndGen s) = undefined


randomlist :: HexRndGen -> [Int]
randomlist g = n:randomlist g'
	where
		(n, g') = next g

randomlistR :: HexRndGen -> Int -> [Int]
randomlistR g modulus = (`mod` modulus) <$> randomlist g


test0 = (take 10 $ randomlist $ HexRndGen 17) == [0,24107,16552,12125,9427,13152,21440,3383,6873,16117::Int]
