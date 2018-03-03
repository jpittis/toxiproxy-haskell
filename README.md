# toxiproxy-haskell

A work in progress client for [Toxiproxy](https://github.com/Shopify/toxiproxy).

## Implemented

- getVersion  :: ClientM Version
- postReset   :: ClientM NoContent
- getProxies  :: ClientM (Map Text Proxy)
- createProxy :: Proxy -> ClientM Proxy
- getProxy    :: Text -> ClientM Proxy

## Todo

- POST /populate - Create or replace a list of proxies
- POST /proxies/{proxy} - Update a proxy's fields
- DELETE /proxies/{proxy} - Delete an existing proxy
- GET /proxies/{proxy}/toxics - List active toxics
- POST /proxies/{proxy}/toxics - Create a new toxic
- GET /proxies/{proxy}/toxics/{toxic} - Get an active toxic's fields
- POST /proxies/{proxy}/toxics/{toxic} - Update an active toxic
- DELETE /proxies/{proxy}/toxics/{toxic} - Remove an active toxic

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
