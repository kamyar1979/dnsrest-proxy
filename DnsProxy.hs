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
import Data.Tuple
import GHC.Word
import Data.List

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

responseNS :: Identifier -> Question -> [Domain] -> DNSMessage
responseNS ident q domains =
  let hd = header defaultResponse
      dom = qname q
      an = ResourceRecord dom NS classIN 300 . RD_NS <$> domains
  in  defaultResponse {
          header = hd { identifier=ident }
        , question = [q]
        , answer = an
      }

responseMX :: Identifier -> Question -> [(Word16, Domain)] -> DNSMessage
responseMX ident q dps =
  let hd = header defaultResponse
      dom = qname q
      an = ResourceRecord dom MX classIN 300 . uncurry RD_MX <$> dps
  in  defaultResponse {
          header = hd { identifier=ident }
        , question = [q]
        , answer = an
      }
      
readMxItem :: String -> (Word16, Domain)
readMxItem val = (read $ takeWhile (/= ' ') val, B.pack $ dropWhile (== ' ') $ dropWhile (/= ' ') val)

resolveUsingHttp :: DNSMessage -> IO (Either String DNSMessage)
resolveUsingHttp req = do
    res <- get ("https://dns-api.org/" ++ (show $ qtype $ quest) ++ "/" ++ (B.unpack $ qname $ quest))
    case J.decode (res ^. responseBody) of
        Nothing -> return $ Left ("Unable to resolve domain:  " ++ (B.unpack $ qname quest))
        Just resLst -> case qtype $ quest of
            A -> return $ Right $ responseA ident quest (map (read.value) resLst)
            NS -> return $ Right $ responseNS ident quest (map (B.pack.value) resLst)
            MX -> return $ Right $ responseMX ident quest (map (readMxItem.value) resLst)
    where 
        ident = identifier . header $ req
        quest = (question req)!!0
        
    
        
    

main :: IO ()
main = run def
