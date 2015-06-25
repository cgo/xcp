module Network.Xcp
    (-- * Types and Classes
     XcpCommandId(..)
    ,pid
    ,ToByteString
    ,XcpError(..)
     -- * XCP packets
    ,xcpGet
    ,xcpSet
    ,xcpConnect
    ,xcpDisconnect
     -- * XCP Results and Conversion
    ,XcpResult(..)
    ,byteStringToResult
    ,fromBytes
    ,bytesToString) where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_)
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.List (intersperse)
import Data.Monoid
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Numeric
import System.IO.Unsafe

-- | XCP commands from the standard.
-- Note: Not all of these are implemented and a few from the
-- XCP reference are missing. Please add them as needed if you like.
data XcpCommandId = ConnectXcp
                | DisconnectXcp
                | GetStatus
                | Synch
                | GetCommModeInfo
                | GetId
                | SetRequest
                | GetSeed
                | Unlock
                | SetMta
                | Upload
                | ShortUpload
                | BuildChecksum
                | TransportLayerCmd
                | UserCmd
                | Download
                | DownloadNext
                | DownloadMax
                | ShortDownload
                | ModifyBits

pid :: XcpCommandId -> Word8
pid ConnectXcp = 0xFF
pid DisconnectXcp = 0xFE
pid GetStatus = 0xFD
pid Synch = 0xFC
pid GetCommModeInfo = 0xFB
pid GetId = 0xFA
pid SetRequest = 0xF9
pid GetSeed = 0xF8
pid Unlock = 0xF7
pid SetMta = 0xF6
pid Upload = 0xF5
pid ShortUpload = 0xF4
pid BuildChecksum = 0xF3
pid TransportLayerCmd = 0xF2
pid UserCmd = 0xF1
pid Download = 0xF0
pid DownloadNext = 0xEF
pid DownloadMax = 0xEE
pid ShortDownload = 0xED
pid ModifyBits = 0xEC

-- | Class for types that can be converted to a lazy 'ByteString' for transmission.
class Storable a => ToByteString a where
    toByteString :: a -> L.ByteString

instance ToByteString Word8 where
    toByteString = L.singleton

instance ToByteString Word16 where
    toByteString = toLazyByteString . word16LE

instance ToByteString Word32 where
    toByteString = toLazyByteString . word32LE

instance ToByteString Float where
    toByteString = toLazyByteString . floatLE

instance ToByteString Int8 where
    toByteString = toLazyByteString . int8

instance ToByteString Int16 where
    toByteString = toLazyByteString . int16LE

instance ToByteString Int32 where
    toByteString = toLazyByteString . int32LE


-- | Show a string of bytes.
bytesToString :: [Word8] -> String
bytesToString a = concat $ intersperse " " $ map (flip showHex "") a


-- | Create an XCP packet to get a number of bytes from an address.
xcpGet :: Word32       -- ^ Memory address
       -> Word8        -- ^ Number of bytes to retrieve
       -> L.ByteString -- ^ The XCP packet
xcpGet addr sz = L.pack (pid ShortUpload : sz : 0 : 0 : []) `mappend` toByteString addr


-- | Create an XCP packet to set a number of bytes at an address.
xcpSet :: ToByteString a =>
          Word32       -- ^ Memory address
       -> a            -- ^ Value to set at the given address
       -> L.ByteString -- ^ The XCP packet
xcpSet addr a = L.pack (pid ShortDownload : sz : 0 : 0 : []) `mappend` toByteString addr `mappend` toByteString a
    where sz = fromIntegral $ sizeOf a


-- | The XCP /connect/ packet.
xcpConnect :: L.ByteString
xcpConnect = L.pack $ [pid ConnectXcp, 0]


-- | The XCP /disconnect/ packet.
xcpDisconnect :: L.ByteString
xcpDisconnect = L.pack $ [pid DisconnectXcp, 0]


-- | Generate a value from a list of bytes by poking them into memory.
fromBytes :: Storable a =>
             a        -- ^ Just a dummy to fix the type of the return value.
          -> [Word8]  -- ^ The bytes that make up the return value.
          -> Maybe a  -- ^ If the number of bytes are sufficient, the first /sizeOf a/ bytes from the list of bytes are poked into the returned value.
fromBytes dummy b = unsafePerformIO $ alloca $ \p ->
                    zipWithM_ (\a n -> pokeByteOff p n a) b [0..sz-1] >>
                    if (sz <= length b) 
                    then Just <$> peek p
                    else return Nothing
    where sz = sizeOf dummy
          
              

-- | Result structure for the result of XCP commands, received from a slave.
data XcpResult = XcpResult { xcpResultPayload :: B.ByteString
                           , xcpResultSize    :: Word16
                           , xcpResultCounter :: Word16 }
               | XcpErr { xcpErr :: XcpError
                        , xcpErrPayload :: B.ByteString
                        , xcpErrSize :: Word16
                        , xcpErrCounter :: Word16 }

data XcpError = GenericError Word8 -- No error codes implemented yet.


-- | Convert a result XCP message received from a slave into a 'XcpResult'.
byteStringToResult :: B.ByteString  -- ^ The XCP message as received from the slave.
                   -> XcpResult     -- ^ The decoded result.
byteStringToResult bs | B.null bs = XcpErr (GenericError 0) mempty 0 0
byteStringToResult bs | B.length bs < 5 = XcpErr (GenericError 0) mempty 0 0
byteStringToResult bs | otherwise =
    let (sizeLow:sizeHigh:ctrLow:ctrHigh:a:as) = B.unpack bs
        sz = ((fromIntegral sizeLow) .|. ((fromIntegral sizeHigh) `shiftL` 8))
        ctr = ((fromIntegral ctrLow) .|. ((fromIntegral ctrHigh) `shiftL` 8)) in
    case a of
      0xFF -> XcpResult (B.pack as) sz ctr
      0xFE -> XcpErr (GenericError (head as)) (B.pack (tail as)) sz ctr
      otherwise -> XcpErr (GenericError 0) mempty sz ctr
