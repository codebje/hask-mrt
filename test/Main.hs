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
import           Data.Binary.MRT
import qualified Data.ByteString.Lazy   as BL
import           System.Environment

loadStream :: FilePath -> IO BL.ByteString
loadStream = liftM BZ.decompress . BL.readFile

main :: IO ()
main = do
  args <- getArgs
  mapM_ ((mapM_ print =<<) . (liftM readMessages <$> loadStream)) args
