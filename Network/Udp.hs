module Network.Udp
    ( withUdpDo
    , udpSocket ) where


import Network
import Network.Socket

-- type IPAddress = String
-- data UdpConfig = UdpConfig { udpcIP   :: IPAddress
--                            , udpcPort :: PortNumber }
-- data UdpState = UdpState
-- newtype Udp m a = Udp { runUdp :: RWST UdpConfig [String] UdpState m a } deriving Monad


-- | The same as 'withSocketsDo', since some operating systems require
-- some initialisation when using sockets.
withUdpDo = withSocketsDo

-- | Creates a socket with the given local IP address and port.
udpSocket :: String     -- ^ Local IP address in the form "a.b.c.d".
          -> PortNumber -- ^ Port number
          -> IO Socket  -- ^ The socket tp use with UDP.
udpSocket localAddress p =
    inet_addr localAddress >>= \haddr ->
    let addr = SockAddrInet p haddr in socket AF_INET Datagram defaultProtocol >>= \s ->
    bind s addr >> return s

         
    
