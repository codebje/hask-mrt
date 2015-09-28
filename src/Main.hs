module Main where

import qualified Codec.Compression.BZip as BZ
import           Control.Monad          (liftM, replicateM)
import           Data.Binary
import qualified Data.Binary.Bits.Get   as BG
import           Data.Binary.Get
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString        as BS
import           Data.Maybe             (listToMaybe)
import           Data.IP

import Debug.Trace

type Timestamp  = Word32
type ASNumber   = Word32

data Origin = IGP | EGP | INCOMPLETE deriving (Show, Eq, Enum)

data PathSegmentType = SEQUENCE | SET | CONFED_SEQUENCE | CONFED_SET
    deriving (Show, Eq)

instance Enum PathSegmentType where
    fromEnum SEQUENCE           = 1
    fromEnum SET                = 2
    fromEnum CONFED_SEQUENCE    = 3
    fromEnum CONFED_SET         = 4
    toEnum 1                    = SEQUENCE
    toEnum 2                    = SET
    toEnum 3                    = CONFED_SEQUENCE
    toEnum 4                    = CONFED_SET
    toEnum _                    = undefined

data PathSegment = PathSegment
    { segmentType       :: PathSegmentType
    , sequence          :: [ASNumber] }
    deriving (Show)

data Community = NO_EXPORT
               | NO_ADVERTISE
               | NO_EXPORT_SUBCONFED
               | COMMUNITY Word32
               deriving (Show)

data BGPAttribute = ORIGIN Origin
                  | AS_PATH [PathSegment]
                  | LOCAL_PREF Word16
                  | ATOMIC_AGGREGATE
                  | AGGREGATOR ASNumber IPv4
                  | MP_REACH_NLRI -- contents discarded
                  | COMMUNITIES [Community]
                  | UnknownAttribute Word8 BS.ByteString
    deriving (Show)

data RIBEntry = RIBEntry
    { peerIndex         :: Word16
    , originationTime   :: Timestamp
    , attributes        :: [BGPAttribute] }
    deriving (Show)

data MRTRecord = TableDumpV2
    { sequenceNo   :: Word32
    , prefix       :: IPRange
    , entries      :: [RIBEntry] }
               | Other { skippedBytes :: Word32 }
    deriving (Show)

data MRTMessage = MRTMessage
    { timestamp :: Timestamp
    , payload   :: MRTRecord }
    deriving (Show)

data BGPAttributeFlags = BGPAttributeFlags
    { isOptional   :: Bool
    , isTransitive :: Bool
    , isPartial    :: Bool
    , isExtLength  :: Bool }

instance Show BGPAttributeFlags where
    show (BGPAttributeFlags o t p e) = map snd $ filter fst $ zipWith (,) [o, t, p, e] "OTPE"

getBytes8 :: Get BL.ByteString
getBytes8 = getWord8 >>= getLazyByteString . fromIntegral

getBytes16be :: Get BL.ByteString
getBytes16be = getWord16be >>= getLazyByteString . fromIntegral

getBytes32be :: Get BL.ByteString
getBytes32be = getWord32be >>= getLazyByteString . fromIntegral

getIPRange :: (Addr a) => (AddrRange a -> IPRange) -> ([Int] -> a) -> Int -> Get IPRange
getIPRange toRange toAddr bits = do
    maskLength <- getWord8 >>= return . fromIntegral
    let dataLength = (maskLength - 1) `div` 8 + 1
    bytes <- getByteString dataLength >>= return . (take bits) . (++ (replicate bits 0)) . BS.unpack
    let address = toAddr (map fromIntegral bytes)
    let range = makeAddrRange address maskLength
    return $ toRange range

getIPv4Range :: Get IPRange
getIPv4Range = getIPRange IPv4Range toIPv4 4

getIPv6Range :: Get IPRange
getIPv6Range = getIPRange IPv6Range toIPv6b 16

getIPv4 :: Get IPv4
getIPv4 = getByteString 4 >>= return . toIPv4 . (map fromIntegral) . BS.unpack

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
attributeReader 1  = ORIGIN . toEnum . fromIntegral . BS.head
attributeReader 2  = const (AS_PATH [])
attributeReader 5  = LOCAL_PREF . BS.foldl (\t v -> t * 256 + fromIntegral v) 0
attributeReader 6  = const ATOMIC_AGGREGATE
attributeReader 7  = runGet (AGGREGATOR <$> getWord32be <*> getIPv4) . BL.fromStrict
attributeReader 8  = runGet (COMMUNITIES <$> getCommunities) . BL.fromStrict
attributeReader 14 = const MP_REACH_NLRI
attributeReader t  = UnknownAttribute t

getAttribute :: Get BGPAttribute
getAttribute = do
    flags <- getAttrFlags
    atype <- getWord8
    size  <- if (isExtLength flags) then getWord16be else getWord8 >>= return . fromIntegral
    bytes <- getByteString (fromIntegral size)
    return $ attributeReader atype bytes

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
          <*> (getBytes16be >>= return . runGet getAttributes)

getRIBEntries :: Get [RIBEntry]
getRIBEntries = do
    count <- getWord16be
    replicateM (fromIntegral count) getRIBEntry

readPayload :: Word16 -> Word16 -> BL.ByteString -> MRTRecord
readPayload 13 4 d = (flip runGet) d $ do
    TableDumpV2 <$> getWord32be
                <*> getIPv6Range
                <*> getRIBEntries
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
    go (Done r _ m) i = m : case i of { [] -> []; _ -> (more $ r : i) }
    go (Partial k)  i = go (k . listToMaybe $ i) (drop 1 i)
    go (Fail _ o s) _ = error (s ++ " at " ++ (show o))

testStream :: IO BL.ByteString
testStream = liftM BZ.decompress $ BL.readFile "rib.20150922.1200.bz2"

sample :: IO [MRTMessage]
sample = do
    input <- testStream
    return $ take 10 $ readMessages input

main :: IO ()
main = putStrLn "hello world"
