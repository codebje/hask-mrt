{-|
Module      : MRT
Description : MRT Export Information Format parser
License     : BSD3
Stability   : Experimental

MRT is a library for parsing Multi-Threaded Routing Toolkit (MRT) export
files, of the kind you might find on the RouteViews archive.
-}
module Net.MRT
    ( Timestamp
    , ASNumber
    , ASPathSegment
    , RIBEntry
    , getPeerIndex
    , getOriginationTime
    , getBGPAttributes
    , MRTRecord
    , getSequenceNo
    , getPrefix
    , getRIBEntries
    , getSkippedBytes
    , MRTMessage
    , getMessageTimestamp
    , getRecord
    , BGPAttributeFlags
    , readMessages
    ) where

import           Control.Monad        (liftM, replicateM)
import           Data.Binary
import qualified Data.Binary.Bits.Get as BG
import           Data.Binary.Get
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.IP
import           Data.Maybe           (listToMaybe)

-- |The `Timestamp` type alias represents a BGP timestamp attribute,
-- recorded as seconds since the Unix epoch.
type Timestamp  = Word32

data RIBEntry = RIBEntry
    { getPeerIndex       :: Word16
    , getOriginationTime :: Timestamp
    , getBGPAttributes   :: [BGPAttribute] }
    deriving (Show)

data MRTRecord = TableDumpV2
    { getSequenceNo :: Word32
    , getPrefix     :: IPRange
    , getRIBEntries :: [RIBEntry] }
               | Other { getSkippedBytes :: Word32 }
    deriving (Show)

data MRTMessage = MRTMessage
    { getMessageTimestamp :: Timestamp
    , getRecord           :: MRTRecord }
    deriving (Show)

-- feh.

type ASNumber   = Word32

data Origin = IGP | EGP | INCOMPLETE deriving (Show, Eq, Enum)

data ASPathSegment = Sequence [ASNumber]
                   | Set [ASNumber]
                   | ConfedSequence [ASNumber]
                   | ConfedSet [ASNumber]
    deriving (Show)

data Community = NO_EXPORT
               | NO_ADVERTISE
               | NO_EXPORT_SUBCONFED
               | COMMUNITY Word32
   deriving (Show)

data BGPAttribute = Origin Origin
                  | ASPath [ASPathSegment]
                  | LocalPref Word16
                  | AtomicAggregate
                  | Aggregator ASNumber IPv4
                  | MultipathReach -- contents discarded
                  | Communities [Community]
                  | UnknownAttribute Word8 BS.ByteString
    deriving (Show)

data BGPAttributeFlags = BGPAttributeFlags
    { isOptional   :: Bool
    , isTransitive :: Bool
    , isPartial    :: Bool
    , isExtLength  :: Bool }

instance Show BGPAttributeFlags where
    show (BGPAttributeFlags o t p e) = map snd $ filter fst $ zip [o, t, p, e] "OTPE"

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

getIPv4 :: Get IPv4
getIPv4 = liftM (toIPv4 . map fromIntegral . BS.unpack) (getByteString 4)


getAttrFlags :: Get BGPAttributeFlags
getAttrFlags = BG.runBitGet $
    BG.block (BGPAttributeFlags <$> BG.bool <*> BG.bool <*> BG.bool <*> BG.bool)

getCommunity :: Get Community
getCommunity = do
    community <- getWord32be
    case community of
        0xFFFFFF01 -> return NO_EXPORT
        0xFFFFFF02 -> return NO_ADVERTISE
        0xFFFFFF03 -> return NO_EXPORT_SUBCONFED
        _          -> return (COMMUNITY community)

getCommunities :: Get [Community]
getCommunities = do
    empty <- isEmpty
    if empty
        then return []
        else do community   <- getCommunity
                communities <- getCommunities
                return (community:communities)

attributeReader :: Word8 -> BS.ByteString -> BGPAttribute
attributeReader 1  = Origin . toEnum . fromIntegral . BS.head
attributeReader 2  = const (ASPath [])
attributeReader 5  = LocalPref . BS.foldl (\t v -> t * 256 + fromIntegral v) 0
attributeReader 6  = const AtomicAggregate
attributeReader 7  = runGet (Aggregator <$> getWord32be <*> getIPv4) . BL.fromStrict
attributeReader 8  = runGet (Communities <$> getCommunities) . BL.fromStrict
attributeReader 14 = const MultipathReach
attributeReader t  = UnknownAttribute t

getAttribute :: Get BGPAttribute
getAttribute = do
    flags <- getAttrFlags
    atype <- getWord8
    size  <- if isExtLength flags then getWord16be else liftM fromIntegral getWord8
    bytes <- getByteString (fromIntegral size)
    return $ attributeReader atype bytes

-- feh.

getBytes8 :: Get BL.ByteString
getBytes8 = getWord8 >>= getLazyByteString . fromIntegral

getBytes16be :: Get BL.ByteString
getBytes16be = getWord16be >>= getLazyByteString . fromIntegral

getBytes32be :: Get BL.ByteString
getBytes32be = getWord32be >>= getLazyByteString . fromIntegral

getAttributes :: Get [BGPAttribute]
getAttributes = do
    empty <- isEmpty
    if empty
        then return []
        else do attr <- getAttribute
                attrs <- getAttributes
                return (attr:attrs)

getRIBEntry :: Get RIBEntry
getRIBEntry = RIBEntry
          <$> getWord16be
          <*> getWord32be
          <*> liftM (runGet getAttributes) getBytes16be

getTimes :: (Integral a) => a -> Get b -> Get [b]
getTimes = replicateM . fromIntegral

readPayload :: Word16 -> Word16 -> BL.ByteString -> MRTRecord
readPayload 13 4 d = flip runGet d $ TableDumpV2 <$> getWord32be
                                                 <*> getIPv6Range
                                                 <*> (getWord16be >>= flip getTimes getRIBEntry)
readPayload _ _ d  = Other $ fromIntegral (BL.length d)

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
