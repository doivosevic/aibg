{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Strategy where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Int
import Data.Ord
import Data.List
import Hilbert
import Combinations
import Settings

newtype Moves = Moves [(Int8, Int8)]

type CellState = Int8

empty, ally, enemy :: Int8
(empty, ally, enemy) = (0, 1, 2)

data GameState = GameState { field :: Vector CellState
                           , ammo  :: Int
                           , iterIndex :: Int }
                           deriving (Eq, Ord, Show)

gridIdx :: [(Int8, Int8)]
gridIdx = map (\(x, y) -> (fromIntegral x, fromIntegral y)) grid

oneOffset :: [(Int8, Int8)]
oneOffset = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

twoOffset :: [(Int8, Int8)]
twoOffset = [(x, y) | x <- [-2..2], y <- [-2..2], x /= 0 || y /= 0]

at :: GameState -> (Int8, Int8) -> CellState
(st@GameState{..}) `at` (x, y) = field Vec.! idx (x, y)

coords :: Int -> (Int8, Int8)
coords n = let (d, r) = n `divMod` fromIntegral gridWidth in (fromIntegral r, fromIntegral d)

idx :: (Int8, Int8) -> Int
idx (x, y)
    |    x >= 0 && x < gridWidth
      && y >= 0 && y < gridHeight = fromIntegral x + fromIntegral y * fromIntegral gridWidth
    | otherwise                   = idx (x `mod` gridWidth, y `mod` gridHeight)

numCell :: CellState -> Int
numCell c | c == empty = 0
          | c == ally  = 1
          | c == enemy = -1

neighbors :: GameState -> (Int8, Int8) -> Int
neighbors gs (x, y) = s
    where s = sum $ map (\(i, j) -> numCell $ gs `at` (x + i, y + j)) oneOffset

smart :: GameState -> (Int8, Int8) -> Bool
smart gs (x, y) = gs `at` (x, y) == empty && close
    where all1Neigh = map (\(i, j) -> gs `at` (x + i, y + j)) oneOffset
          close     = (allies - enemies) `elem` [2, 3]
          allies    = length (filter (== ally) all1Neigh)
          enemies   = length (filter (== enemy) all1Neigh)

legal :: GameState -> (Int8, Int8) -> Bool
legal gs (x, y) = gs `at` (x, y) == empty && close
    where all1Neigh = map (\(i, j) -> gs `at` (x + i, y + j)) oneOffset
          close     = ally `elem` all1Neigh

logic :: (Int8, Int8) -> CellState -> GameState -> CellState
logic (x, y) st gs = case () of
    _ | st == empty && ns == 3            -> ally
      | st == empty && ns == -3           -> enemy
      | st == empty                       -> empty
      | st == ally && ns `elem` [2, 3]    -> ally
      | st == enemy && ns `elem` [-2, -3] -> enemy
      | otherwise                         -> empty
      where ns = neighbors gs (x, y)

advance :: GameState -> GameState
advance gs = gs { field = Vec.imap (\i st -> logic (coords i) st gs) (field gs) }

allMoves :: GameState -> [(Int8, Int8)]
allMoves gs = moves -- ++ movesR ++ movesD
    where moves = filter (legal gs) gridIdx

smartMoves :: GameState -> [(Int8, Int8)]
smartMoves gs = moves
    where moves = filter (smart gs) gridIdx

windows :: Int -> [a] -> [[a]]
windows n xs = take (length xs - n + 1) $ map (take n) $ tails xs

moveWindows :: Int -> [(Int8, Int8)] -> [Moves]
moveWindows k mvs = map Moves $ windows k mvs

evaluate :: GameState -> Int
evaluate gs = won * 5 + available
    where won = sum $ map (numCell . (gs `at`)) gridIdx
          available = fromIntegral (length (smartMoves gs))

goodness :: GameState -> Moves -> Int
goodness (gs@GameState{..}) (Moves mvs) = evaluate oneStep + evaluate twoStep + evaluate threeStep
    where newField = field Vec.// map (\(x, y) -> (idx (x, y), ally)) mvs
          oneStep = advance $ gs { field = newField }
          twoStep = advance oneStep
          threeStep = advance twoStep

decide :: GameState -> Moves
decide gs = snd $ maximumBy (comparing fst) eval
    where mvs       = allMoves gs
          allCombos = moveWindows 3 mvs -- ++ bestOf
          sorted1   = sortBy (flip $ comparing (goodness gs . Moves . return)) mvs
          bestOf    = map Moves $ twoCombinations $ take 20 sorted1
          eval      = map (\combo -> let g = goodness gs combo in g `seq` (g, combo)) allCombos
