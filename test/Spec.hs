{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Servant.Client
import Servant.API
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Map.Strict as Map

import Toxiproxy

toxiproxyUrl = BaseUrl Http "localhost" 8474 ""

run f = do
  manager <- newManager defaultManagerSettings
  runClientM f (ClientEnv manager toxiproxyUrl)

main :: IO ()
main = hspec $
  describe "Toxiproxy" $ do
    it "gets version" $
      run getVersion `shouldReturn` Right "git-fe6bf4f"
    it "posts reset" $
      run postReset `shouldReturn` Right NoContent
    it "gets proxies" $
      run getProxies `shouldReturn` Right Map.empty
    it "creates and gets a proxy" $ do
      let name = "myProxy"
      let proxy = Proxy
            { proxyName     = name
            , proxyListen   = "localhost:4444"
            , proxyUpstream = "lcoalhost:4445"
            , proxyEnabled  = False
            , proxyToxics   = []
            }
      run (createProxy proxy) `shouldReturn` Right proxy
      run getProxies `shouldReturn` Right (Map.fromList [(name, proxy)])
      run (getProxy name) `shouldReturn` Right proxy

