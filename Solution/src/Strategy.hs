{-# LANGUAGE RecordWildCards #-}
module Strategy where

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Int

newtype Moves = Moves [(Int, Int)]

data CellState = Empty | Ally | Enemy deriving (Eq, Ord, Read, Show)

data GameState = GameState { field :: Vector CellState
                           , ammo  :: Int }
                           deriving (Eq, Ord, Show)

gridHeight, gridWidth :: Int
(gridHeight, gridWidth) = (24, 24)

at :: GameState -> (Int, Int) -> CellState
(st@GameState{..}) `at` (x, y)
    |    x >= 0 && x < gridWidth
      && y >= 0 && y < gridHeight = field Vec.! (x + y * gridHeight)
    | otherwise                   = st `at` (x `mod` gridWidth, y `mod` gridHeight)

coords :: Int -> (Int, Int)
coords n = let (d, r) = n `divMod` gridWidth in (r, d)

neighbors :: GameState -> (Int, Int) -> Int
neighbors = undefined

logic :: (Int, Int) -> CellState -> GameState -> CellState
logic (x, y) st gs = case st of
    Empty | ns == 3            -> Ally
          | ns == -3           -> Enemy
    Ally  | ns `elem` [2, 3]   -> Ally
          | otherwise          -> Empty
    Enemy | ns `elem` [-2, -3] -> Enemy
          | otherwise          -> Empty
    where ns = neighbors gs (x, y)

advance :: GameState -> GameState
advance gs = gs { field = Vec.imap (\i st -> logic (coords i) st gs) (field gs) }

allMoves :: GameState -> Moves
allMoves = undefined

kCombinations :: Moves -> [Moves]
kCombinations = undefined

decide :: GameState -> Moves
decide gs
    where allMoves
