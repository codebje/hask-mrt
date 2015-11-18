{-|
Module      : Data.Network.IP
Description : Binary instances for Data.IP
License     : BSD3
Stability   : Experimental

MRT is a library for parsing Multi-Threaded Routing Toolkit (MRT) export
files, of the kind you might find on the RouteViews archive.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Binary.IP ( IPv4, IPv6 )
  where

import           Control.Monad   (liftM)
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString as BS
import           Data.IP

instance Binary IPv4 where
    get = liftM (toIPv4 . map fromIntegral . BS.unpack) (getByteString 4)
    put _ = error "not yet implemented"

instance Binary IPv6 where
    get = liftM (toIPv6b . map fromIntegral . BS.unpack) (getByteString 16)
    put _ = error "not yet implemented"
