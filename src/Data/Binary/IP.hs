{-|
Module      : Data.Binary.IP
Description : IP address binary (de)serialisation
License     : BSD3
Stability   : Experimental

MRT is a library for parsing Multi-Threaded Routing Toolkit (MRT) export
files, of the kind you might find on the RouteViews archive.
-}

module Data.Binary.IP
  ( getIPv4
  , getIPv6)
  where

import           Control.Monad (liftM)
import           Data.IP
import           Data.Binary.Get
import qualified Data.ByteString as BS

getIPv4 :: Get IPv4
getIPv4 = liftM (toIPv4 . map fromIntegral . BS.unpack) (getByteString 4)

getIPv6 :: Get IPv6
getIPv6 = liftM (toIPv6b . map fromIntegral . BS.unpack) (getByteString 16)
