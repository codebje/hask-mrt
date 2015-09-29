{-|
Module      : Main
Description : MRT Export Information Format parser
License     : BSD3
Stability   : Experimental

MRT is a library for parsing Multi-Threaded Routing Toolkit (MRT) export
files, of the kind you might find on the RouteViews archive.
-}
module Main where

import qualified Codec.Compression.BZip as BZ
import           Control.Monad          (liftM)
import qualified Data.ByteString.Lazy   as BL
import           Net.MRT

testStream :: IO BL.ByteString
testStream = liftM BZ.decompress $ BL.readFile "rib.20150922.1200.bz2"

sample :: IO [MRTMessage]
sample = do
    input <- testStream
    return $ take 10 $ readMessages input

main :: IO ()
main = sample >>= mapM_ print
