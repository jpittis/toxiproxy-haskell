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

Before this client can be used with the official Toxiproxy build, the following patch has
to be accepted upstream.

````diff
--- a/api.go
+++ b/api.go
@@ -382,7 +382,7 @@ func (server *ApiServer) ToxicDelete(response http.ResponseWriter, request *http
 }

 func (server *ApiServer) Version(response http.ResponseWriter, request *http.Request) {
-       response.Header().Set("Content-Type", "text/plain")
+       response.Header().Set("Content-Type", "text/plain;charset=utf-8")
        _, err := response.Write([]byte(Version))
        if err != nil {
                logrus.Warn("Version: Failed to write response to client", err)
````

Currently using stack nigtly to have access to servant 12.0. Switch to stable.
