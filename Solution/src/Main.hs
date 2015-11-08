{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Main where

import qualified Data.Aeson                 as Aes
import qualified Data.Aeson.TH              as Aes
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import Control.Monad

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
leftest ServerInfo{..} = minimum [(x, y) | x <- [0..23], y <- [0..23], field !! y !! x == '#']

makeMove :: ServerInfo -> Move
makeMove si = Move [(x - 1, y - 1)]
    where (x, y) = leftest si

main :: IO ()
main = forever $ do
    parseRes <- Aes.decodeStrict <$> BS.getLine
    case parseRes of
        Just servInfo -> LazyBS.putStrLn $ Aes.encode $ makeMove servInfo
        Nothing       -> error "Server sent malformed JSON"
