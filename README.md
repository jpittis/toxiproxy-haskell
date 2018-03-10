A Haskell client for [Toxiproxy](https://github.com/Shopify/toxiproxy).

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
        , toxicType       = Latency
        , toxicStream     = Upstream
        , toxicToxicity   = 1
        , toxicAttributes = Map.fromList [("latency", 1000), ("jitter", 0)]
        }
  withProxy proxy $ \proxy ->
    withToxic proxy latency getRequestToMyProxyHost -- This will take > 1 second
````
