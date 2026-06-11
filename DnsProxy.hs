{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Control.Exception (SomeException, try)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Maybe
import System.Environment (getArgs)
import System.Exit (die)
import System.Timeout (timeout)
import Network.Socket.ByteString (sendAllTo, recvFrom)
import Network.Socket hiding (recvFrom)
import Network.DNS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import DnsProxyCore


{--
 - Parse request and compose response.
 -}
handlePacket :: Conf -> HTTP.Manager -> Socket -> SockAddr -> B.ByteString -> IO ()
handlePacket conf@Conf{..} manager sock addr s =
    either
    (putStrLn . ("decode fail:"++) . show)
    (\req -> do
        resolveUsingDoh conf manager req >>=
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
    manager <- HTTP.newManager HTTPS.tlsManagerSettings
    addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
    addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
    sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
    bind sock (addrAddress addrinfo)
    forever $ do
        (s, addr) <- recvFrom sock (bufSize conf)
        forkIO $ handlePacket conf manager sock addr s

resolveUsingDoh :: Conf -> HTTP.Manager -> DNSMessage -> IO (Either String DNSMessage)
resolveUsingDoh Conf{..} manager req = do
    httpReq <- makeDohRequest dohUrl req
    res <- try (HTTP.httpLbs httpReq manager) :: IO (Either SomeException (HTTP.Response BL.ByteString))
    case res of
        Left err -> return $ Left ("DoH request failed: " ++ show err)
        Right rsp -> return $ either (Left . ("DoH response decode failed: "++) . show) (Right . restoreResponseId req)
                   $ decode (BL.toStrict $ HTTP.responseBody rsp)

makeDohRequest :: String -> DNSMessage -> IO HTTP.Request
makeDohRequest url req = do
    httpReq <- HTTP.parseRequest url
    return httpReq
      { HTTP.method = "POST"
      , HTTP.requestBody = HTTP.RequestBodyLBS . BL.fromStrict . encode $ prepareDohRequest req
      , HTTP.requestHeaders =
          [ ("Accept", "application/dns-message")
          , ("Content-Type", "application/dns-message")
          ]
      }

main :: IO ()
main = do
    args <- getArgs
    either die run $ configFromArgs args
