{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Servant.Client
import Servant.API
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)
import System.Process (withCreateProcess, proc, CreateProcess)
import System.IO.Silently (silence)

import Toxiproxy

withToxiproxyServer :: IO a -> IO a
withToxiproxyServer f =
  silence $
    withCreateProcess server $ \_ _ _ _ -> threadDelay 100000 >> f
  where
    server :: CreateProcess
    server = proc "toxiproxy-server" []

run :: ClientM a -> IO (Either ServantError a)
run f = do
  manager <- newManager defaultManagerSettings
  runClientM f (ClientEnv manager toxiproxyUrl)

main :: IO ()
main = hspec $
  describe "Toxiproxy" $ do
    it "get version" $
      withToxiproxyServer $
        run getVersion `shouldReturn` Right "git-fe6bf4f"
    it "post reset" $
      withToxiproxyServer $
        run postReset `shouldReturn` Right NoContent
    it "create, update, get and delete a proxy" $
      withToxiproxyServer $ do
        let name = "myProxy"
        let proxy = Proxy
              { proxyName     = name
              , proxyListen   = "127.0.0.1:4444"
              , proxyUpstream = "127.0.0.1:4445"
              , proxyEnabled  = False
              , proxyToxics   = []
              }
        run (createProxy proxy) `shouldReturn` Right proxy
        run getProxies `shouldReturn` Right (Map.fromList [(name, proxy)])
        run (getProxy name) `shouldReturn` Right proxy
        let enabled = proxy { proxyEnabled  = True }
        run (updateProxy name enabled) `shouldReturn` Right enabled
        run (deleteProxy name) `shouldReturn` Right NoContent
        run getProxies `shouldReturn` Right Map.empty
    it "populate proxies" $
      withToxiproxyServer $ do
        let proxy1 = Proxy
              { proxyName     = "myProxy"
              , proxyListen   = "127.0.0.1:4444"
              , proxyUpstream = "127.0.0.1:4445"
              , proxyEnabled  = False
              , proxyToxics   = []
              }
        let proxy2 = Proxy
              { proxyName     = "myOtherProxy"
              , proxyListen   = "127.0.0.1:4446"
              , proxyUpstream = "127.0.0.1:4447"
              , proxyEnabled  = False
              , proxyToxics   = []
              }
        run (populate [proxy1, proxy2]) `shouldReturn` Right (Populate [proxy1, proxy2])
    it "create get, update and delete toxic" $
      withToxiproxyServer $ do
        let name = "myProxy"
        let toxicName = "latency"
        let toxic = Toxic
              { toxicName       = toxicName
              , toxicType       = toxicName
              , toxicStream     = "upstream"
              , toxicToxicity   = 1
              , toxicAttributes = Map.fromList [("latency", 1000), ("jitter", 0)]
              }
        let proxy = Proxy
              { proxyName     = name
              , proxyListen   = "127.0.0.1:4444"
              , proxyUpstream = "127.0.0.1:4445"
              , proxyEnabled  = False
              , proxyToxics   = []
              }
        let proxyWithToxic = proxy { proxyToxics = [toxic] }
        let updatedToxic =
              toxic { toxicAttributes = Map.fromList [("latency", 1000), ("jitter", 0)] }
        run (createProxy proxy) `shouldReturn` Right proxy
        run (createToxic name toxic) `shouldReturn` Right toxic
        run (getToxics name) `shouldReturn` Right [toxic]
        run (getProxy name) `shouldReturn` Right proxyWithToxic
        run (updateToxic name toxicName updatedToxic) `shouldReturn` Right updatedToxic
        run (deleteToxic name toxicName) `shouldReturn` Right NoContent
        run (getToxics name) `shouldReturn` Right []
