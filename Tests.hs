module Main where

import DnsProxyCore
import Network.DNS
import System.Exit (exitFailure)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual label expected actual =
  if expected == actual
     then pure True
     else do
       putStrLn $ "FAIL: " ++ label
       putStrLn $ "  expected: " ++ show expected
       putStrLn $ "  actual:   " ++ show actual
       pure False

messageWithId :: Identifier -> DNSMessage
messageWithId ident =
  defaultResponse { header = (header defaultResponse) { identifier = ident } }

tests :: [IO Bool]
tests =
  [ assertEqual "empty args use default config"
      (Right defaultConf)
      (configFromArgs [])
  , assertEqual "single arg overrides DoH URL"
      (Right defaultConf { dohUrl = "https://dns.google/dns-query" })
      (configFromArgs ["https://dns.google/dns-query"])
  , assertEqual "multiple args return usage error"
      (Left "Usage: DnsProxy [DOH_URL]")
      (configFromArgs ["https://dns.google/dns-query", "extra"])
  , assertEqual "DoH request ID is normalized to zero"
      0
      (identifier . header . prepareDohRequest $ messageWithId 1234)
  , assertEqual "DoH response ID is restored from the original request"
      1234
      (identifier . header $ restoreResponseId (messageWithId 1234) (messageWithId 0))
  ]

main :: IO ()
main = do
  results <- sequence tests
  if and results
     then putStrLn $ "PASS: " ++ show (length tests) ++ " tests"
     else exitFailure
