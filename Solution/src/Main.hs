{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Main where

import qualified Data.Aeson                 as Aes
import qualified Data.Aeson.TH              as Aes
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import Control.Monad
import System.IO (hFlush, stdout)
import Strategy (Moves(..), GameState(GameState), CellState(..), decide, empty, ally, enemy)
import qualified Data.Vector.Unboxed as Vec

data ServerInfo = ServerInfo { field                   :: [String]
                             , cellsRemaining          :: Int
                             , cellGainPerTurn         :: Int
                             , maxCellCapacity         :: Int
                             , maxColonisationDistance :: Int
                             , currIteration           :: Int
                             , maxGameIterations       :: Int
                             , timeGainPerTurn         :: Int
                             , timeLeftForMove         :: Int }
                             deriving (Eq, Ord, Show, Read)

Aes.deriveJSON Aes.defaultOptions ''ServerInfo

data Move = Move { cells :: [(Int, Int)] }
                 deriving (Eq, Ord, Show, Read)

Aes.deriveJSON Aes.defaultOptions ''Move

leftest :: ServerInfo -> (Int, Int)
leftest ServerInfo{..} = maximum [(x, y) | x <- [0..23], y <- [0..23], field !! y !! x == '#']

makeMove :: ServerInfo -> Move
makeMove = movesToMove . decide . serverInfoToGameState

movesToMove :: Moves -> Move
movesToMove (Moves mvs) = Move $ map (\(x, y) -> (y, x)) mvs

charToCell :: Char -> CellState
charToCell '.' = empty
charToCell '#' = ally
charToCell 'O' = enemy
charToCell _   = error "Bad cell"

serverInfoToGameState :: ServerInfo -> GameState
serverInfoToGameState ServerInfo{..} = GameState (Vec.fromList $ map charToCell $ concat field)
                                                 cellsRemaining
                                                 currIteration

main :: IO ()
main = forever $ do
    parseRes <- Aes.decodeStrict <$> BS.getLine
    case parseRes of
        Just servInfo -> do
            let move = Aes.encode $ makeMove servInfo
            LazyBS.putStr move
            putStr "\n"
            hFlush stdout
        Nothing       -> error "Server sent malformed JSON"
