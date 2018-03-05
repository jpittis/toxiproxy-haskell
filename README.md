A complete Haskell client for [Toxiproxy](https://github.com/Shopify/toxiproxy).

(Requires Toxiproxy version 2.1.3 and above.)

## Example

````haskell
import Toxiproxy

main :: IO ()
main = do
  let proxy = Proxy
        { proxyName     = "myProxy"
        , proxyListen   = myProxyHost
        , proxyUpstream = myUpstreamHost
        , proxyEnabled  = True
        , proxyToxics   = []
        }
  let latency = Toxic
        { toxicName       = "latency"
        , toxicType       = "latency"
        , toxicStream     = "upstream"
        , toxicToxicity   = 1
        , toxicAttributes = Map.fromList [("latency", 1000), ("jitter", 0)]
        }
  withProxy proxy $ \proxy -> do
    withToxic proxy latency $ do
      getRequestToMyProxyHost -- This will take > 1 second
````
