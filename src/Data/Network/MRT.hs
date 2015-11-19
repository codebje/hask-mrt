{-|
Module      : Data.Network.MRT
Description : Multi-Threaded Routing Toolkit Export Information Format types
License     : BSD3
Stability   : Experimental

MRT is a library for parsing Multi-Threaded Routing Toolkit (MRT) export
files, of the kind you might find on the RouteViews archive.
-}

{-# LANGUAGE LambdaCase #-}

module Data.Network.MRT
    ( Timestamp
    , ASNumber
    , ASPathSegment
    , BGPAttributeValue ( Origin
                        , ASPath
                        , NextHop
                        , LocalPref
                        , AtomicAggregate
                        , UnknownAttribute)
    , BGPAttributeFlags (isOptional, isTransitive, isPartial, isExtLength)
    , BGPAttribute (BGPAttribute)
    , RIBEntry
    , getPeerIndex
    , getOriginationTime
    , getBGPAttributes
    , MRTRecord (TableDumpV2, Other)
    , getSequenceNo
    , getPrefix
    , getRIBEntries
    , getSkippedBytes
    , MRTMessage
    , getMessageTimestamp
    , getRecord
    , readMessages
    -- re-export IPs
    , IPRange
    ) where

import           Control.Monad        (liftM, replicateM)
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.IP
import           Data.Maybe           (listToMaybe)
import           Data.Network.BGP

-- |The `Timestamp` type alias represents a BGP timestamp attribute,
-- recorded as seconds since the Unix epoch.
type Timestamp  = Word32

-- instance Show BGPAttributeFlags where
--     show (BGPAttributeFlags o t p e) = map snd $ filter fst $ zip [o, t, p, e] "OTPE"

data RIBEntry = RIBEntry
    { getPeerIndex       :: Word16
    , getOriginationTime :: Timestamp
    , getBGPAttributes   :: [BGPAttribute] }
    deriving (Show)

data MRTRecord = TableDumpV2 { getSequenceNo :: Word32
                             , getPrefix     :: IPRange
                             , getRIBEntries :: [RIBEntry] }
               | Other { getTypeCode     :: Word16
                       , getSubType      :: Word16
                       , getSkippedBytes :: Word32 }
    deriving (Show)

data MRTMessage = MRTMessage
    { getMessageTimestamp :: Timestamp
    , getRecord           :: MRTRecord }
    deriving (Show)

getIPRange :: (Addr a) => (AddrRange a -> IPRange) -> ([Int] -> a) -> Int -> Get IPRange
getIPRange toRange toAddr bits = do
    maskLength <- liftM fromIntegral getWord8
    let dataLength = (maskLength - 1) `div` 8 + 1
    bytes <- liftM (take bits . (++ replicate bits 0) . BS.unpack) (getByteString dataLength)
    let address = toAddr (map fromIntegral bytes)
    let range = makeAddrRange address maskLength
    return $ toRange range

getIPv4Range :: Get IPRange
getIPv4Range = getIPRange IPv4Range toIPv4 4

getIPv6Range :: Get IPRange
getIPv6Range = getIPRange IPv6Range toIPv6b 16


getBytes16be :: Get BL.ByteString
getBytes16be = getWord16be >>= getLazyByteString . fromIntegral

getBytes32be :: Get BL.ByteString
getBytes32be = getWord32be >>= getLazyByteString . fromIntegral

getAttributes :: Get [BGPAttribute]
getAttributes = do
    empty <- isEmpty
    if empty
        then return []
        else (:) <$> get <*> getAttributes

getRIBEntry :: Get RIBEntry
getRIBEntry = RIBEntry
          <$> getWord16be
          <*> getWord32be
          <*> liftM (runGet getAttributes) getBytes16be

getTimes :: (Integral a) => a -> Get b -> Get [b]
getTimes = replicateM . fromIntegral

readPayload :: Word16 -> Word16 -> BL.ByteString -> MRTRecord
readPayload 13 2 d = flip runGet d $ TableDumpV2 <$> getWord32be
                                                 <*> getIPv4Range
                                                 <*> (getWord16be >>= flip getTimes getRIBEntry)
readPayload 13 4 d = flip runGet d $ TableDumpV2 <$> getWord32be
                                                 <*> getIPv6Range
                                                 <*> (getWord16be >>= flip getTimes getRIBEntry)
readPayload t s d  = Other t s $ fromIntegral (BL.length d)

readMessage :: Get MRTMessage
readMessage = do
    timestamp'  <- getWord32be
    payload'    <- readPayload <$> getWord16be <*> getWord16be <*> getBytes32be
    return $ MRTMessage timestamp' payload'

readMessages :: BL.ByteString -> [MRTMessage]
readMessages input = more (BL.toChunks input)
  where
    more = go (runGetIncremental readMessage)
    go :: Decoder MRTMessage -> [BS.ByteString] -> [MRTMessage]
    go (Done r _ m) i = m : case i of { [] -> []; _ -> more $ r : i }
    go (Partial k)  i = go (k . listToMaybe $ i) (drop 1 i)
    go (Fail _ o s) _ = error (s ++ " at " ++ show o)
