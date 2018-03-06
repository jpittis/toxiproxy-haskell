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
    , Proxy(..)
    , Toxic(..)
    , Populate(..)
    , toxiproxyUrl
    , withDisabled
    , withToxic
    , withProxy
    , run
    , Version(..)
    , Stream(..)
    , ToxicType(..)
    ) where

import Servant.API
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

newtype ProxyName = ProxyName Text
  deriving (Show, Eq, IsString, Ord, Generic, ToHttpApiData, FromJSONKey)

instance FromJSON ProxyName
instance ToJSON   ProxyName

newtype ToxicName = ToxicName Text
  deriving (Show, Eq, IsString, Generic, ToHttpApiData)

instance FromJSON ToxicName
instance ToJSON   ToxicName

newtype Version = Version Text
  deriving (Show, Eq, MimeUnrender PlainText)

data Proxy = Proxy
  { proxyName     :: ProxyName
  , proxyListen   :: Host
  , proxyUpstream :: Host
  , proxyEnabled  :: Bool
  , proxyToxics   :: [Toxic]
  } deriving (Show, Eq, Generic)

instance FromJSON Proxy where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "proxy" }

instance ToJSON Proxy where
  toJSON = genericToJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "proxy" }

type Host = Text

data Toxic = Toxic
  { toxicName       :: ToxicName
  , toxicType       :: ToxicType
  , toxicStream     :: Stream
  , toxicToxicity   :: Float
  , toxicAttributes :: Map Text Int
  } deriving (Show, Eq, Generic)

instance FromJSON Toxic where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "toxic" }

instance ToJSON Toxic where
  toJSON = genericToJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "toxic" }

newtype Populate = Populate { populateProxies :: [Proxy] }
  deriving (Show, Eq, Generic)

instance FromJSON Populate where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "populate" }

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

data ToxicType = Latency | Other Text
  deriving (Show, Eq)

instance ToJSON ToxicType where
  toJSON Latency       = String "latency"
  toJSON (Other other) = String other

instance FromJSON ToxicType where
  parseJSON (String toxicType) =
    case toxicType of
      "latency" -> return Latency
      other     -> return . Other $ other

stripPrefixJSON :: String -> String -> String
stripPrefixJSON prefix str =
  case stripPrefix prefix str of
    Nothing             -> str
    Just (first : rest) -> Char.toLower first : rest

toxiproxyAPI :: Proxy.Proxy ToxiproxyAPI
toxiproxyAPI = Proxy.Proxy

getVersion   :: ClientM Version
postReset    :: ClientM NoContent
getProxies   :: ClientM (Map ProxyName Proxy)
createProxy  :: Proxy -> ClientM Proxy
getProxy     :: ProxyName -> ClientM Proxy
postPopulate :: [Proxy] -> ClientM Populate
updateProxy  :: ProxyName -> Proxy -> ClientM Proxy
deleteProxy  :: ProxyName -> ClientM NoContent
getToxics    :: ProxyName -> ClientM [Toxic]
createToxic  :: ProxyName -> Toxic -> ClientM Toxic
getToxic     :: ProxyName -> ToxicName -> ClientM Toxic
updateToxic  :: ProxyName -> ToxicName -> Toxic -> ClientM Toxic
deleteToxic  :: ProxyName -> ToxicName -> ClientM NoContent

(getVersion :<|> postReset :<|> getProxies :<|> createProxy :<|> getProxy :<|> postPopulate
            :<|> updateProxy :<|> deleteProxy :<|> getToxics :<|> createToxic :<|> getToxic
            :<|> updateToxic :<|> deleteToxic) = client toxiproxyAPI

toxiproxyUrl :: BaseUrl
toxiproxyUrl = BaseUrl Http "127.0.0.1" 8474 ""

run :: ClientM a -> IO (Either ServantError a)
run f = do
  manager <- newManager defaultManagerSettings
  runClientM f (ClientEnv manager toxiproxyUrl)

withDisabled :: Proxy -> IO a -> IO a
withDisabled proxy f =
  bracket disable enable $ const f
  where
    enable        = const . run $ updateProxy (proxyName proxy) proxy
    disable       = void . run $ updateProxy (proxyName proxy) disabledProxy
    disabledProxy = proxy { proxyEnabled = False }

withToxic :: Proxy -> Toxic -> IO a -> IO a
withToxic proxy toxic f =
  bracket enable disable $ const f
  where
    enable = void . run $ createToxic (proxyName proxy) toxic
    disable = const . run $ deleteToxic (proxyName proxy) (toxicName toxic)

withProxy :: Proxy -> (Proxy -> IO a) -> IO a
withProxy proxy =
  bracket create delete
  where
    create = run (createProxy proxy) >> return proxy
    delete = const . run $ deleteProxy (proxyName proxy)
