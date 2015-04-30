module Data.Bitcoin.Types ( TransactionId
                          , BlockHash
                          , PrivateKey
                          , Address
                          , Account
                          , Btc
                          , VarInt (..) ) where

import Control.Applicative ((<$>))
import Data.Word ( Word64 )

import Data.Binary ( Binary, get, put )
import Data.Binary.Get ( getByteString
                       , getWord8
                       , getWord16le
                       , getWord32le
                       , getWord64le )

import Data.Binary.Put ( putByteString
                       , putWord8
                       , putWord16le
                       , putWord32le
                       , putWord64le )

import           Data.Fixed
import qualified Data.HexString    as HS
import qualified Data.Base58String as B58S
import qualified Data.Text         as T

-- | Per Bitcoin documentation, an identifier used to uniquely identify a
--   particular transaction; specifically, the sha256d hash of the transaction.
type TransactionId = HS.HexString

-- | Per Bitcoin document, an identifier used to uniquely identify a block
--   by its header.
type BlockHash = HS.HexString

-- | A base58 private key to sign transactions
type PrivateKey = B58S.Base58String

-- | Per Bitcoin documentation, an identifier used to uniquely identify a
--   particular address.
type Address = B58S.Base58String

-- | A wallet account can be any easy to remember string.
type Account = T.Text

-- | The smallest unit of payment possible in Bitcoin is a Satoshi
data Satoshi = Satoshi

-- | We describe BTC in terms of Satoshi, where one BTC equals 10^8 Satoshis.
instance HasResolution Satoshi where
  resolution _ = 10 ^ ( 8 :: Integer )

-- | A single Bitcoin, which represents 10^8 Satoshis.
type Btc = Fixed Satoshi



-- | Data type representing a variable length integer. The 'VarInt' type
--   usually precedes an array or a string that can vary in length.
newtype VarInt = VarInt { getVarInt :: Word64 }
    deriving (Eq, Show, Read)

instance Binary VarInt where

    get = VarInt <$> ( getWord8 >>= go )
      where
        go 0xff = getWord64le
        go 0xfe = fromIntegral <$> getWord32le
        go 0xfd = fromIntegral <$> getWord16le
        go x    = fromIntegral <$> return x

    put (VarInt x)
        | x < 0xfd =
            putWord8 $ fromIntegral x
        | x <= 0xffff = do
            putWord8 0xfd
            putWord16le $ fromIntegral x
        | x <= 0xffffffff = do
            putWord8 0xfe
            putWord32le $ fromIntegral x
        | otherwise = do
            putWord8 0xff
            putWord64le x
