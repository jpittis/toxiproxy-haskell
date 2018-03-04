# toxiproxy-haskell

A complete Haskell client for [Toxiproxy](https://github.com/Shopify/toxiproxy).

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

## Todo

1. Before this client can be used with the official Toxiproxy build, the following patch has
to be accepted upstream. ([Open upstream PR](https://github.com/Shopify/toxiproxy/pull/204))

2. Ship a new version of Toxiproxy with the above patch.

3. Release to hackage and stackage.
