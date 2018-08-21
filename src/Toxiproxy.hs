{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Toxiproxy
    ( getVersion
    , postReset
    , getProxies
    , createProxy
    , getProxy
    , postPopulate
    , updateProxy
    , deleteProxy
    , getToxics
    , createToxic
    , getToxic
    , updateToxic
    , deleteToxic
    , BaseUrl(..)
    , Proxy(..)
    , Toxic(..)
    , Populate(..)
    , Version(..)
    , Stream(..)
    , ToxicType(..)
    , ProxyName(..)
    , ToxicName(..)
    , Host
    , toxiproxyUrl
    , withDisabled
    , withToxic
    , withProxy
    , run
    ) where

import Servant.API hiding (Stream)
import Servant.Client
import qualified Data.Proxy as Proxy
import Data.Text (Text, pack, toLower, unpack)
import Data.List (stripPrefix)
import qualified Data.Char as Char (toLower)
import GHC.Generics
import Data.Aeson (FromJSON, parseJSON, fieldLabelModifier, defaultOptions, genericParseJSON,
                   ToJSON, genericToJSON, toJSON, FromJSONKey, Value( String ))
import Data.Map.Strict (Map)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.String (IsString)

type ToxiproxyAPI =
       "version"  :> Get '[PlainText] Version
  :<|> "reset"    :> Post '[] NoContent
  :<|> "proxies"  :> Get '[JSON] (Map ProxyName Proxy)
  :<|> "proxies"  :> ReqBody '[JSON] Proxy    :> Post '[JSON] Proxy
  :<|> "proxies"  :> Capture "name" ProxyName :> Get '[JSON] Proxy
  :<|> "populate" :> ReqBody '[JSON] [Proxy]  :> Post '[JSON] Populate
  :<|> "proxies"  :> Capture "name" ProxyName :> ReqBody '[JSON] Proxy :> Post '[JSON] Proxy
  :<|> "proxies"  :> Capture "name" ProxyName :> Delete '[] NoContent
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> Get '[JSON] [Toxic]
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> ReqBody '[JSON] Toxic    :> Post '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> Capture "name" ToxicName :> Get '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> Capture "name" ToxicName :> ReqBody '[JSON] Toxic :> Get '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> Capture "name" ToxicName :> Delete '[JSON] NoContent

-- | A unique string for identifying a proxy on the server.
newtype ProxyName = ProxyName Text
  deriving (Show, Eq, IsString, Ord, Generic, ToHttpApiData, FromJSONKey)

instance FromJSON ProxyName
instance ToJSON   ProxyName

-- | A unique string for identifying a toxic on a proxy.
newtype ToxicName = ToxicName Text
  deriving (Show, Eq, IsString, Generic, ToHttpApiData)

instance FromJSON ToxicName
instance ToJSON   ToxicName

-- | The version of the Toxiproxy server. This library is fully supported by any version
--   greater or equal to 2.1.3.
newtype Version = Version Text
  deriving (Show, Eq, MimeUnrender PlainText)

-- | A Toxiproxy proxy. It forwards TCP connections between a listen and upstream host.
--   Toxics can be injected into the proxy to simulate network failure.
data Proxy = Proxy
  { proxyName     :: ProxyName
  -- ^ A unique human readable name to identify a proxy.
  , proxyListen   :: Host
  -- ^ The proxy listens on this host:port.
  , proxyUpstream :: Host
  -- ^ The proxy forwards to this upstream host:port.
  , proxyEnabled  :: Bool
  -- ^ Whether a proxy is currently listening / accepting connections.
  , proxyToxics   :: [Toxic]
  -- ^ The toxics currently applied to the proxy. These should not be specified when
  --   initially creating a proxy. They must be created seperately with 'createToxic'
  --   or 'withToxic'.
  } deriving (Show, Eq, Generic)

instance FromJSON Proxy where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "proxy" }

instance ToJSON Proxy where
  toJSON = genericToJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "proxy" }

-- | A host:port pair to represent the entrence of a proxy or the upstream the proxy
--   forwards to. For the best experience, provide 127.0.0.1 instead of localhost.
type Host = Text

-- | A toxic is applied to a proxy. It allows the user to simulate a specified kind of
--   network failure on the proxy.
data Toxic = Toxic
  { toxicName       :: ToxicName
  -- ^ A unique human readable name to identify a toxic.
  , toxicType       :: ToxicType
  -- ^ The type of toxic. For example "latency". Please refer to 'ToxicType' or the
  -- Toxiproxy documentation for more information.
  , toxicStream     :: Stream
  -- ^ The direction on which the toxic is applied. Please refer to 'Stream'.
  , toxicToxicity   :: Float
  -- ^ The strength that the toxic is applied to the proxy. Please refer to the Toxiproxy
  --   documation.
  , toxicAttributes :: Map Text Int
  -- ^ Attributes configure a toxic. They differ based on the 'ToxicType'. Please refer to
  --   the Toxiproxy documentation.
  } deriving (Show, Eq, Generic)

instance FromJSON Toxic where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "toxic" }

instance ToJSON Toxic where
  toJSON = genericToJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "toxic" }

-- | The return value of the 'populate' endpoint.
newtype Populate = Populate { populateProxies :: [Proxy] }
  deriving (Show, Eq, Generic)

instance FromJSON Populate where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "populate" }

-- | A toxic can be applied to the upstream or the downstream of a connection. Upstream is
--   the stream traveling from the connecting client to the upstream server. Downstream is
--   the stream traveling from the upstream server to the connecting client.
data Stream = Upstream | Downstream
  deriving (Show, Eq)

instance ToJSON Stream where
  toJSON Upstream   = String "upstream"
  toJSON Downstream = String "downstream"

instance FromJSON Stream where
  parseJSON (String stream) =
    case stream of
      "upstream"   -> return Upstream
      "downstream" -> return Downstream

-- | Different toxic types simulate different kinds of failure. Different toxics require
--   different attribute configuration. Please refer to the Toxiproxy documentation.
data ToxicType =
    Latency
  | Bandwidth
  | SlowClose
  | Timeout
  | Slicer
  | LimitData
  | Other Text
  deriving (Show, Eq)

instance ToJSON ToxicType where
  toJSON Latency       = String "latency"
  toJSON Bandwidth     = String "bandwidth"
  toJSON SlowClose     = String "slow_close"
  toJSON Timeout       = String "timeout"
  toJSON Slicer        = String "slicer"
  toJSON LimitData     = String "limit_data"
  toJSON (Other other) = String other

instance FromJSON ToxicType where
  parseJSON (String toxicType) =
    case toxicType of
      "latency"   -> return Latency
      "bandwidth" -> return Bandwidth
      "slow_clos" -> return SlowClose
      "timeout"   -> return Timeout
      "slicer"    -> return Slicer
      "limit_dat" -> return LimitData
      other       -> return . Other $ other

stripPrefixJSON :: String -> String -> String
stripPrefixJSON prefix str =
  case stripPrefix prefix str of
    Nothing             -> str
    Just (first : rest) -> Char.toLower first : rest

toxiproxyAPI :: Proxy.Proxy ToxiproxyAPI
toxiproxyAPI = Proxy.Proxy

-- | Returns the server version number.
getVersion   :: ClientM Version
-- | Enable all proxies and remove all active toxics.
postReset    :: ClientM NoContent
-- | List existing proxies and their toxics.
getProxies   :: ClientM (Map ProxyName Proxy)
-- | Create a new proxy.
createProxy  :: Proxy -> ClientM Proxy
-- | Get a proxy with all its active toxics.
getProxy     :: ProxyName -> ClientM Proxy
-- | Create or replace a list of proxies.
postPopulate :: [Proxy] -> ClientM Populate
-- | Update a proxy's fields.
updateProxy  :: ProxyName -> Proxy -> ClientM Proxy
-- | Delete an existing proxy.
deleteProxy  :: ProxyName -> ClientM NoContent
-- | List active toxics.
getToxics    :: ProxyName -> ClientM [Toxic]
-- | Create a new toxic.
createToxic  :: ProxyName -> Toxic -> ClientM Toxic
-- |  Get an active toxic's fields.
getToxic     :: ProxyName -> ToxicName -> ClientM Toxic
-- | Update an active toxic.
updateToxic  :: ProxyName -> ToxicName -> Toxic -> ClientM Toxic
-- | Remove an active toxic.
deleteToxic  :: ProxyName -> ToxicName -> ClientM NoContent

(getVersion :<|> postReset :<|> getProxies :<|> createProxy :<|> getProxy :<|> postPopulate
            :<|> updateProxy :<|> deleteProxy :<|> getToxics :<|> createToxic :<|> getToxic
            :<|> updateToxic :<|> deleteToxic) = client toxiproxyAPI

-- | The default Toxiproxy service URL.
--   (127.0.0.1:8474)
toxiproxyUrl :: BaseUrl
toxiproxyUrl = BaseUrl Http "127.0.0.1" 8474 ""

-- | A helper for easily querying the Toxiproxy API. Assumes Toxiproxy is running on
--  'toxiproxyUrl'.
--
-- @
-- proxies <- run getProxies
-- @
run :: ClientM a -> IO (Either ServantError a)
run = run' toxiproxyUrl

-- | A version of 'run' where you specify the Toxiproxy URL.
run' :: BaseUrl -> ClientM a -> IO (Either ServantError a)
run' url f = do
  manager <- newManager defaultManagerSettings
  runClientM f (ClientEnv manager url Nothing)

-- | Given an enabled proxy, disable the proxy, run the given action and then re-enable
--   the proxy.
--
--   This is useful for simulating a crashed server or closed connection.
--
-- @
-- connectToMyProxy       -- This will connect.
-- withDisabled myProxy $
--   connectToMyProxy     -- This will get rejected.
-- connectToMyProxy       -- This will connect again.
-- @
withDisabled :: Proxy -> IO a -> IO a
withDisabled = withDisabled' toxiproxyUrl

-- | A version of withDisabled where you specify the Toxiproxy URL.
withDisabled' :: BaseUrl -> Proxy -> IO a -> IO a
withDisabled' url proxy f =
  bracket disable enable $ const f
  where
    enable        = const . run' url $ updateProxy (proxyName proxy) proxy
    disable       = void  . run' url $ updateProxy (proxyName proxy) disabledProxy
    disabledProxy = proxy { proxyEnabled = False }

-- | Given a proxy and a toxic, create the toxic on the proxy, run the given action and
--   then delete the toxic.
--
--   This is useful for running some action with a toxic enabled.
--
-- @
-- withToxic myProxy latencyToxic $
--   sendRequestThroughProxy -- This request will have latency applied to it.
-- @
withToxic :: Proxy -> Toxic -> IO a -> IO a
withToxic = withToxic' toxiproxyUrl

-- | A version of withToxic where you specify the Toxiproxy URL.
withToxic' :: BaseUrl -> Proxy -> Toxic -> IO a -> IO a
withToxic' url proxy toxic f =
  bracket enable disable $ const f
  where
    enable  = void  . run' url $ createToxic (proxyName proxy) toxic
    disable = const . run' url $ deleteToxic (proxyName proxy) (toxicName toxic)

-- | Given a proxy record, create the proxy on the server, run the given action and then
--   delete the proxy off the server.
--
--   This is useful for wrapping 'withDisabled' and 'withToxic' calls. It enures that your
--   test cleans up the Toxiproxy server so that proxies don't leak into your other tests.
withProxy :: Proxy -> (Proxy -> IO a) -> IO a
withProxy = withProxy' toxiproxyUrl

-- | A version of withProxy where you specify the Toxiproxy URL.
withProxy' :: BaseUrl -> Proxy -> (Proxy -> IO a) -> IO a
withProxy' url proxy =
  bracket create delete
  where
    create = run' url (createProxy proxy) >> return proxy
    delete = const . run' url $ deleteProxy (proxyName proxy)
