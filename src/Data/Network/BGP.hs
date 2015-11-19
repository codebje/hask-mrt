{-|
Module      : Data.Network.BGP
Description : Border Gateway Protocol v4 types
License     : BSD3
Stability   : Experimental

MRT is a library for parsing Multi-Threaded Routing Toolkit (MRT) export
files, of the kind you might find on the RouteViews archive.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Network.BGP (
    ASNumber
  , Origin (IGP, EGP, INCOMPLETE)
  , ASPath (getASPathSegments)
  , ASPathSegment (ASSet, ASSequence)
  , BGPAttributeValue ( Origin
                      , ASPath
                      , NextHop
                      , LocalPref
                      , AtomicAggregate
                      , UnknownAttribute )
  , BGPAttributeFlags (isOptional, isTransitive, isPartial, isExtLength)
  , BGPAttribute (BGPAttribute)
) where

import           Control.Monad        (liftM)
import           Data.Binary
import qualified Data.Binary.Bits.Get as BG
import           Data.Binary.Get
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Proxy


-- ** Fundamental data types

-- |2-byte AS number formats in BGP are unsupported
type ASNumber = Word32

-- ** BGP Attributes

-- | ORIGIN is a well-known mandatory attribute that defines the
--   origin of the path information.
--
--  See: RFC 4271, Section 5.1.1
data Origin = IGP | EGP | INCOMPLETE deriving (Read, Show, Eq, Enum)

instance Binary Origin where
    get = toEnum . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . fromEnum

-- | AS_PATH is a well-known mandatory attribute that is composed
--   of a sequence of AS path segments.
newtype ASPath = MkPath { getASPathSegments :: [ASPathSegment] }
    deriving (Read, Show)

instance Binary ASPath where
    get = MkPath <$> (getWord8 >>= getTimes)
    put (MkPath ps) = putTimes (Proxy :: Proxy Word8) ps

-- | A path segment is either an AS Set or an AS Sequence.
--
--  See RFC 4271, Section 5.1.2
data ASPathSegment = ASSet [ASNumber]
                   | ASSequence [ASNumber]
    deriving (Read, Show)

instance Binary ASPathSegment where
    get = do
        segType <- getWord8
        ases <- getTimes segType
        case segType of
            1 -> return $ ASSet ases
            2 -> return $ ASSequence ases
            _ -> fail $ "Unknown segment type " ++ show segType
    put (ASSequence as) = putTimes (Proxy :: Proxy Word8) as
    put (ASSet as) = putTimes (Proxy :: Proxy Word8) as

-- | A BGP attribute consist of flags, then one of t
data BGPAttributeValue = Origin Origin
                       | ASPath ASPath
                       | NextHop BS.ByteString
                       | LocalPref Word32
                       | AtomicAggregate
                       | UnknownAttribute Word8 BS.ByteString
    deriving (Read, Show)

data BGPAttributeFlags = BGPAttributeFlags
    { isOptional   :: Bool
    , isTransitive :: Bool
    , isPartial    :: Bool
    , isExtLength  :: Bool }
    deriving (Read, Show)

data BGPAttribute = BGPAttribute BGPAttributeFlags BGPAttributeValue
    deriving (Read, Show)

instance Binary BGPAttributeFlags where
    get = BG.runBitGet $
        BG.block (BGPAttributeFlags <$> BG.bool <*> BG.bool <*> BG.bool <*> BG.bool)
    put = undefined

instance Binary BGPAttribute where
    get = do
        flags <- get
        atype <- getWord8
        size  <- if isExtLength flags then getWord16be else liftM fromIntegral getWord8
        bytes <- getByteString (fromIntegral size)
        return . BGPAttribute flags $ attributeReader atype bytes
      where
        attributeReader 1  = Origin . toEnum . fromIntegral . BS.head
        attributeReader 2  = ASPath . runGet get . BL.fromStrict
        attributeReader 3  = NextHop
        attributeReader 5  = LocalPref . runGet getWord32be . BL.fromStrict
        attributeReader 6  = const AtomicAggregate
        attributeReader t  = UnknownAttribute t
    put = undefined

------- utility functions --------

getTimes :: (Integral n, Binary a) => n -> Get [a]
getTimes = sequenceA . flip replicate get . fromIntegral

putTimes :: (Num n, Binary n, Binary a) => Proxy n -> [a] -> Put
putTimes p as = put (asProxyTypeOf (fromIntegral $ length as) p) *> mapM_ put as
