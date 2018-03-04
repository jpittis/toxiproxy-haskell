# toxiproxy-haskell

A Haskell client for [Toxiproxy](https://github.com/Shopify/toxiproxy).

## Features

- The entire Toxiproxy API is implemented and tested.
- A simple high level Haskell API.

## Todo Before V1

- Submit server modification upstream. (See below.)

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
