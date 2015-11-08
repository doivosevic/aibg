{-# LANGUAGE RecordWildCards #-}
module Strategy where

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Int
import Data.Ord
import Data.List
import System.IO.Unsafe

newtype Moves = Moves [(Int, Int)]

data CellState = Empty | Ally | Enemy deriving (Eq, Ord, Read, Show)

data GameState = GameState { field :: Vector CellState
                           , ammo  :: Int }
                           deriving (Eq, Ord, Show)

gridHeight, gridWidth :: Int
(gridHeight, gridWidth) = (24, 24)

gridIdx :: [(Int, Int)]
gridIdx = [(x, y) | x <- [0..gridWidth - 1], y <- [0..gridHeight - 1]]

oneOffset :: [(Int, Int)]
oneOffset = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

twoOffset :: [(Int, Int)]
twoOffset = [(x, y) | x <- [-2..2], y <- [-2..2], x /= 0 || y /= 0]

at :: GameState -> (Int, Int) -> CellState
(st@GameState{..}) `at` (x, y) = field Vec.! idx (x, y)

coords :: Int -> (Int, Int)
coords n = let (d, r) = n `divMod` gridWidth in (r, d)

idx :: (Int, Int) -> Int
idx (x, y)
    |    x >= 0 && x < gridWidth
      && y >= 0 && y < gridHeight = x + y * gridWidth
    | otherwise                   = idx (x `mod` gridWidth, y `mod` gridHeight)

numCell :: CellState -> Int
numCell Empty = 0
numCell Ally  = 1
numCell Enemy = -1

neighbors :: GameState -> (Int, Int) -> Int
neighbors gs (x, y) = s
    where s = sum $ map (\(i, j) -> numCell $ gs `at` (x + i, y + j)) oneOffset

legal :: GameState -> (Int, Int) -> Bool
legal gs (x, y) = gs `at` (x, y) == Empty && close
    where all2Neigh = map (\(i, j) -> gs `at` (x + i, y + j)) oneOffset
          close     = Ally `elem` all2Neigh

logic :: (Int, Int) -> CellState -> GameState -> CellState
logic (x, y) st gs = case st of
    Empty | ns == 3            -> Ally
          | ns == -3           -> Enemy
          | otherwise          -> Empty
    Ally  | ns `elem` [2, 3]   -> Ally
          | otherwise          -> Empty
    Enemy | ns `elem` [-2, -3] -> Enemy
          | otherwise          -> Empty
    where ns = neighbors gs (x, y)

advance :: GameState -> GameState
advance gs = gs { field = Vec.imap (\i st -> logic (coords i) st gs) (field gs) }

allMoves :: GameState -> Moves
allMoves gs = Moves $ filter (legal gs) gridIdx

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
    where subsequencesBySize [] = [[[]]]
          subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                                      in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

kCombinations :: Int -> Moves -> [Moves]
kCombinations k (Moves mvs) = map Moves $ subsequencesOfSize k mvs

evaluate :: GameState -> Int
evaluate gs = sum $ map (numCell . (gs `at`)) gridIdx

goodness :: GameState -> Moves -> Int
goodness (gs@GameState{..}) (Moves mvs) = evaluate $ advance $ gs { field = newField }
    where newField = field Vec.// map (\(x, y) -> (idx (x, y), Ally)) mvs

decide :: GameState -> Moves
decide gs = maximumBy (comparing (goodness gs)) allCombos
    where allCombos = kCombinations 2 $ allMoves gs
