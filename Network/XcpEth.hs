{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
-- |This module is the one to use when you want to use XCP over ethernet.
-- Usage goes somewhat like this:
--
-- @
--   import Network.XcpEth
--   import Data.Int
-- 
--   main = do
--      am <- loadAddressMap "myAddresses"
--      a <- runXcpEth $ do
--             connect "192.168.0.1" 12345 "192.168.0.2" 12345
--             a <- getVariable (0::Float) "myOwnVariable"
--             setVariable "myOtherVariable" (42::Int8)
--             return a
--             disconnect
--      putStrLn $ "Received " ++ show a
-- @
--
-- If you want to add other transport layer protocols,
-- just look at 'Xcp' and add the parts of the XCP message
-- that are specific to your transport layer protocol to the XCP packets
-- you get from 'Xcp'.

module Network.XcpEth
    (
     -- * XcpEth Monad
     XcpEth
    ,runXcpEth
     -- * XcpEth operations
    ,connect
    ,disconnect
    ,setVariable
    ,getVariable
    ,logString
     -- * Reading strings with 'XcpCommand's
    ,readCommands
     -- * Name to address maps and operations
    ,AddressMap
    ,loadAddressMap
    ,setAddressMap
    ,module Network
    ,module Network.Udp
    ,throwError
    ,ToByteString
    -- * Other Types
    ,IPAddress) where


import Network (PortNumber, Socket)
import Network.Udp
import Network.Socket (SockAddr(..), inet_addr, close)
import Network.Socket.ByteString
import Network.Xcp
import Control.Applicative
import Control.Monad (when, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import Data.Int
import qualified Data.Map as M
import Foreign.Storable

type IPAddress = String

type AddressMap = M.Map String Word32

loadAddressMapIO :: FilePath -> IO AddressMap
loadAddressMapIO fp = do
  f <- readFile fp
  let a l = let (addr:name:_) = words l in (name, read ('0':'x':addr))
  return $ M.fromList $ map a (lines f)

         
-- | Sets the address map to use by subsequent actions.
setAddressMap :: AddressMap -> XcpEth ()
setAddressMap am = XcpEth $ modify $ \s -> s { xcpStateConfig = (xcpStateConfig s) { xcpConfigAddressMap = am } }

                   
-- | Reads a mapping from memory addresses to names from a simple text file.
-- Each line is expected of the form
-- /address/ /name/.
loadAddressMap :: FilePath -> XcpEth ()
loadAddressMap fp = XcpEth (liftIO (loadAddressMapIO fp)) >>= setAddressMap

                    
-- | Commands for running lists of commands.
data XcpCommand = ReadNames FilePath
                | Connect IPAddress Int IPAddress Int
                | Disconnect 
                | SetInt8 String Int8
                | SetUInt8 String Word8
                | SetInt16 String Int16
                | SetUInt16 String Word16
                | SetInt32 String Int32
                | SetUInt32 String Word32
                | SetFloat String Float
                | GetInt8 String
                | GetUInt8 String
                | GetInt16 String
                | GetUInt16 String
                | GetInt32 String
                | GetUInt32 String
                | GetFloat String  deriving (Show, Read)


compileCommand :: XcpCommand -> XcpEth ()
compileCommand a = case a of
                     ReadNames fp     -> loadAddressMap fp
                     Connect myIp myPort destIp destPort -> connect myIp (fromIntegral myPort) destIp (fromIntegral destPort)
                     Disconnect       -> disconnect
                     SetInt8  name a  -> setVariable name a
                     SetUInt8 name a  -> setVariable name a
                     SetInt16  name a -> setVariable name a
                     SetUInt16 name a -> setVariable name a
                     SetInt32  name a -> setVariable name a
                     SetUInt32 name a -> setVariable name a
                     SetFloat name a  -> setVariable name a
                     GetInt8 name     -> getAndPrint (0::Int8) name
                     GetUInt8 name    -> getAndPrint (0::Word8) name
                     GetInt16 name    -> getAndPrint (0::Int16) name
                     GetUInt16 name   -> getAndPrint (0::Word16) name
                     GetInt32 name    -> getAndPrint (0::Int32) name
                     GetUInt32 name   -> getAndPrint (0::Word32) name
                     GetFloat name    -> getAndPrint (0::Float) name


readCommand :: String -> XcpEth ()
readCommand s = compileCommand $ read s

readCommands :: [String] -> XcpEth ()
readCommands = mapM_ readCommand


-- | Gets a variable from the connected slave and prints the result to the console.
getAndPrint :: (ToByteString a, Show a) => a -> String -> XcpEth ()
getAndPrint dummy name = do
  a <- getVariable dummy name
  XcpEth $ liftIO $ putStrLn $ name ++ " = " ++ show a


data XcpConfig = XcpConfig { xcpConfigMyIP :: IPAddress
                           , xcpConfigMyPort :: PortNumber
                           , xcpConfigTargetIP :: IPAddress
                           , xcpConfigTargetPort :: PortNumber
                           , xcpConfigAddressMap :: AddressMap }


data XcpState = XcpState { xcpStateMasterCounter :: Word16
                         , xcpStateSlaveCounter :: Word16
                         , xcpStateSocket :: Maybe Socket
                         , xcpStateConfig :: XcpConfig }
                         

-- | The XcpEth monad. It is used to encapsulate sending commands
-- from the host to the slave and receiving results.
newtype XcpEth a = XcpEth { unXcpEth :: RWST () [String] XcpState (ExceptT String IO) a } deriving (Monad, Applicative, Functor)


defaultXcpConfig = XcpConfig "192.168.1.1" 21845 "192.168.1.2" 21845 M.empty

-- | Run an action and return either an error message, or the resulting value and
-- log strings, if any.
runXcpEth :: XcpEth a -> IO (Either String (a,[String]))
runXcpEth act = let s = XcpState 0 0 Nothing defaultXcpConfig in
                         runExceptT (runRWST (unXcpEth act) () s >>= \(a,_,w) -> return (a,w)) 

-- Event and Service from the slave to the master are not implemented here.


-- | Wrap a bytestring containing an XCP packet in more information to yield
-- a XCP message that can be sent to the slave over UDP.
wrapXcpEth :: Word16           -- ^ Message counter
           -> LB.ByteString     -- ^ XCP packet
           -> XcpEth B.ByteString -- ^ Returns the XCP message with ethernet head and tail.
wrapXcpEth ctr bs = do
  let len = LB.length bs
  when (len > 2^16-1) $ throwError "wrapXcpEth: XCP packet is too long."
  let xcpMessage = word16LE (fromIntegral len)
                  `mappend` word16LE ctr
                  `mappend` lazyByteString bs
  return . LB.toStrict . toLazyByteString $ xcpMessage


-- | Increment the host message counter.
incCtr :: XcpEth ()
incCtr = XcpEth . modify $ \s -> s { xcpStateMasterCounter = xcpStateMasterCounter s + 1 }
         

-- | Send the given XCP packet and receive the result from the slave.
-- The packet is wrapped using 'wrapXcpEth'.
sendXcp :: LB.ByteString -> XcpEth XcpResult
sendXcp bs = do
  st@(XcpState ctr slaveCtr msock cfg) <- XcpEth get
  let XcpConfig ip port targetIP targetPort _ = cfg

  sock <- maybe
          (throwError "sendXcp: Not connected.")
          (return)
          msock

  xcpMessage <- wrapXcpEth ctr bs   
  targetHostAddr <- XcpEth . liftIO $ inet_addr targetIP
  let targetAddr = SockAddrInet targetPort targetHostAddr

  -- XcpEth $ tell [bytesToString (B.unpack xcpMessage)]
  n <- XcpEth . liftIO $ sendTo sock xcpMessage targetAddr
  incCtr

  (res, addr) <- XcpEth . liftIO $ recvFrom sock (1024 * 10)
  -- XcpEth $ tell [bytesToString (B.unpack res)]
  return $ byteStringToResult res


-- | Connect to the given slave IP and portnumber, and send
-- an XCP /connect/ packet.
connect :: IPAddress  -- ^ Host IP (this computer).
        -> PortNumber -- ^ Host port number.
        -> IPAddress  -- ^ Slave IP address.
        -> PortNumber -- ^ Slave port number.
        -> XcpEth ()
connect myIp myPort destIp destPort = do
  conf <- XcpEth $ gets xcpStateConfig
  let conf' = XcpConfig myIp myPort destIp destPort (xcpConfigAddressMap conf)
  mySocket <- XcpEth $ liftIO $ udpSocket myIp myPort
  XcpEth $ modify $ \s -> s { xcpStateConfig = conf', xcpStateSocket = Just mySocket }
  -- XcpEth $ tell $ [myIp ++ " -> " ++ destIp]
  res <- sendXcp xcpConnect
  case res of
    XcpResult _ _ _ -> XcpEth $ tell ["connect ok"]
    XcpErr _ _ _ _ -> XcpEth $ tell ["connect failed"]


-- | Sends a /disconnect/ XCP packet to the slave and closes the UDP socket.
disconnect :: XcpEth ()
disconnect = do
  res <- sendXcp xcpDisconnect
  case res of
    XcpResult _ _ _ -> do XcpEth $ tell ["disconnect ok"]
                          ms <- XcpEth $ gets xcpStateSocket
                          maybe (return ()) (\s -> XcpEth . liftIO $ close s) ms
                          XcpEth $ modify $ \s -> s { xcpStateSocket = Nothing }
    XcpErr _ _ _ _ -> XcpEth $ tell ["disconnect failed"]


-- | Stores a log message in the internal log.
logString :: String -> XcpEth ()
logString a = XcpEth $ tell [a]


-- | Set a variable in the slave memory.
setVariable :: ToByteString a =>
               String    -- ^ Name of the variable to set.
            -> a         -- ^ Value to set. Must be of the correct type, otherwise funny things may happen on the slave.
            -> XcpEth ()
setVariable name value = do
  addr <- address name
  res <- sendXcp $ xcpSet addr value
  case res of
    XcpErr _ _ _ _ -> throwError "get failed."
    XcpResult _ _ _ -> return ()


-- | Look up the slave memory address in the internal 'AddressMap', given the variable's name.
address :: String        -- ^ Name of the variable.
        -> XcpEth Word32
address name = do
  am <- xcpConfigAddressMap <$> XcpEth (gets xcpStateConfig)
  maybe (throwError $ "Could not find name " ++ name) (return) $ M.lookup name am


-- | Get the value of a variable in the slave memory.
getVariable :: ToByteString a =>
               a         -- ^ Just a dummy to fix the type of the variable to retrieve.
            -> String    -- ^ Name of the variable.
            -> XcpEth a  -- ^ Returns the retrieved value received from the slave.
getVariable dummy name = do
  addr <- address name
  let sz = sizeOf dummy
  res <- sendXcp $ xcpGet addr $ fromIntegral sz
  case res of
    XcpErr _ _ _ _ -> throwError "get failed."
    XcpResult payload _ _ -> let ma = fromBytes dummy (B.unpack payload)
                             in maybe (throwError "get failed: could not convert result.")
                                      return
                                      ma

throwError :: forall a. String -> XcpEth a
throwError s = XcpEth $ lift $ throwE s 
