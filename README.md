# toxiproxy-haskell

A work in progress client for [Toxiproxy](https://github.com/Shopify/toxiproxy).

## Implemented

- getVersion      :: ClientM Version
- postReset       :: ClientM NoContent
- getProxies      :: ClientM (Map Text Proxy)
- createProxy     :: Proxy -> ClientM Proxy
- getProxy        :: Text -> ClientM Proxy
- populateProxies :: [Proxy] -> ClientM [Proxy]
- updateProxy     :: Text -> Proxy -> ClientM Proxy
- deleteProxy     :: Text -> ClientM NoContent
- getToxics       :: Text -> ClientM [Toxic]
- createToxic     :: Text -> Toxic -> ClientM Toxic
- getToxic        :: Text -> Text -> ClientM Toxic
- updateToxic     :: Text -> Text -> Toxic -> ClientM Toxic
- deleteToxic     :: Text -> Text -> ClientM NoContent

## Todo Before V1

- Write tests for all of the endpoints.
- Submit server modification upstream. (See below.)
- Write a higher level client using the `with` pattern.

## Server Modifications

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

## Stack

Currently using stack nigtly to have access to servant 12.0.
