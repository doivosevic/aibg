module Combinations where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

twoCombinations :: [a] -> [[a]]
twoCombinations list = [[v Vec.! i, v Vec.! j] | i <- [0..len - 1], j <- [i + 1..len - 1]]
    where v = Vec.fromList list
          len = Vec.length v
