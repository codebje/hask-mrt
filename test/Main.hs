{-|
Module      : Main
Description : MRT Export Information Format parser
License     : BSD3
Stability   : Experimental

MRT is a library for parsing Multi-Threaded Routing Toolkit (MRT) export
files, of the kind you might find on the RouteViews archive.
-}
module Main where

import Data.Maybe (listToMaybe)
import qualified Codec.Compression.BZip as BZ
import           Control.Monad          (liftM)
import           Data.Binary.MRT
import qualified Data.ByteString.Lazy   as BL
import           System.Environment

loadStream :: FilePath -> IO BL.ByteString
loadStream = liftM BZ.decompress . BL.readFile

printRib :: IPRange -> RIBEntry -> IO ()
printRib ip rib = do
    let path = listToMaybe $ filter isPath $ getBGPAttributes rib
    putStrLn $ foldl (++) "" [show ip, "|", show path]
    --let attrs = filter (getBGPAttributes rib
  where
    isPath x = case x of {ASPath _ -> True; _ -> False}

printEntry :: MRTMessage -> IO ()
printEntry msg = case getRecord msg of
    (TableDumpV2 _ ip ribs) -> mapM_ (printRib ip) ribs
    _                       -> return ()

main :: IO ()
main = do
  args <- getArgs
  mapM_ ((mapM_ printEntry =<<) . (liftM readMessages <$> loadStream)) args
