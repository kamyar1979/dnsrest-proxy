module DnsProxyCore
  ( Conf(..)
  , defaultConf
  , configFromArgs
  , prepareDohRequest
  , restoreResponseId
  ) where

import Network.DNS

data Conf = Conf
  { bufSize     :: Int
  , timeOut     :: Int
  , dohUrl      :: String
  } deriving (Eq, Show)

defaultConf :: Conf
defaultConf = Conf
  { bufSize     = 512
  , timeOut     = 10 * 1000 * 1000
  , dohUrl      = "https://cloudflare-dns.com/dns-query"
  }

configFromArgs :: [String] -> Either String Conf
configFromArgs [] = Right defaultConf
configFromArgs [url] = Right defaultConf { dohUrl = url }
configFromArgs _ = Left "Usage: DnsProxy [DOH_URL]"

prepareDohRequest :: DNSMessage -> DNSMessage
prepareDohRequest req = req { header = (header req) { identifier = 0 } }

restoreResponseId :: DNSMessage -> DNSMessage -> DNSMessage
restoreResponseId req rsp =
  rsp { header = (header rsp) { identifier = identifier $ header req } }
