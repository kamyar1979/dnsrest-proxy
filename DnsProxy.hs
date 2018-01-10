{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveGeneric #-}

import Prelude hiding (takeWhile)
import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Default (Default(def))
import Data.IP (IPv4, toIPv4)
import Data.List (partition)
import Data.Maybe
import System.Environment (getArgs)
import System.Timeout (timeout)
import Network.Socket.ByteString (sendAll, sendAllTo, recvFrom)
import Network.Socket hiding (recvFrom)
import Network.DNS
import Network.DNS.Resolver (Resolver)
import GHC.Generics
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Network.Wreq hiding (header)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J

type Host = (Domain, IPv4)

data HttpDnsResponse  = HttpDnsResponse { name::String, value::String, ttl::Int} deriving (Show, Generic)

instance FromJSON HttpDnsResponse


data Conf = Conf
  { bufSize     :: Int
  , timeOut     :: Int
  }

instance Default Conf where
    def = Conf
      { bufSize     = 512
      , timeOut     = 10 * 1000 * 1000
      }

toEither :: a -> Maybe b -> Either a b
toEither a = maybe (Left a) Right

{--
 - Parse request and compose response.
 -}
handlePacket :: Conf -> Socket -> SockAddr -> B.ByteString -> IO ()
handlePacket conf@Conf{..} sock addr s =
    either
    (putStrLn . ("decode fail:"++))
    (\req -> do
        resolveUsingHttp req >>=
            either
            putStrLn
            (\rsp -> let packet = encode rsp
                     in  timeout timeOut (sendAllTo sock packet addr) >>=
                         maybe (putStrLn "send response timeout") return
            )
    )
    (decode s)

run :: Conf -> IO ()
run conf = withSocketsDo $ do
    addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
    addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
    sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
    bind sock (addrAddress addrinfo)
    forever $ do
        (s, addr) <- recvFrom sock (bufSize conf)
        forkIO $ handlePacket conf sock addr s


resolveUsingHttp :: DNSMessage -> IO (Either String DNSMessage)
resolveUsingHttp req = do
    res <- get ("https://dns-api.org/" ++ (show $ qtype $ (question req)!!0) ++ "/" ++ (B.unpack $ qname $ (question req)!!0))
    case J.decode (res ^. responseBody) :: Maybe [HttpDnsResponse] of
        Nothing -> return $ Left ("Unable to resolve domain:  " ++ (B.unpack $ qname q))
        Just resLst -> return $ Right $ responseA ident q (map (read.value) resLst)
        where 
            ident = identifier . header $ req
            q = (question req)!!0
    

main :: IO ()
main = run def
