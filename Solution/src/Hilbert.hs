module Hilbert where

import Data.Bits
import Data.List
import Settings
import Data.Int

grid :: [(Int, Int)]
grid = nub $ map (scale . d2xy 32) [0..32 * 32 - 1]

gridRight :: [(Int, Int)]
gridRight = map (shiftHilbert (3, 0)) grid

gridDown :: [(Int, Int)]
gridDown = map (shiftHilbert (0, 3)) grid

shiftHilbert :: (Int, Int) -> (Int, Int) -> (Int, Int)
shiftHilbert (dx, dy) (x, y) = ((x + dx) `mod` fromIntegral gridWidth, (y + dy) `mod` fromIntegral gridHeight)

scale :: (Int, Int) -> (Int, Int)
scale (x, y) = (floor (fromIntegral x / 32 * 24), floor (fromIntegral y / 32 * 24))

d2xy :: Int -> Int -> (Int, Int)
d2xy n d = iter (0, 0, 1, d, 0, 0)
    where iter (rx, ry, s, t, x, y)
              | s >= n = (x, y)
              | otherwise = iter (rx', ry', s * 2, t `div` 4 , x' + s * rx', y' + s * ry')
              where (x', y') = rot s rx' ry' (x, y)
                    rx' = 1 .&. (t `div` 2)
                    ry' = 1 .&. (t `xor` rx')

rot :: Int -> Int -> Int -> (Int, Int) -> (Int, Int)
rot n rx ry (x, y) | ry /= 0   = (x, y)
                   | otherwise = (y', x')
                   where x' = if rx == 1 then n - 1 - x else x
                         y' = if rx == 1 then n - 1 - y else y
