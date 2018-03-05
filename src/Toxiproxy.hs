{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
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
    , Version
    ) where

import Servant.API
import Servant.Client
import qualified Data.Proxy as Proxy
import Data.Text (Text)
import Data.List (stripPrefix)
import Data.Char (toLower)
import GHC.Generics
import Data.Aeson (FromJSON, parseJSON, fieldLabelModifier, defaultOptions, genericParseJSON,
                   ToJSON, genericToJSON, toJSON)
import Data.Map.Strict (Map)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Exception (bracket)
import Control.Monad (void)

type ToxiproxyAPI =
       "version"  :> Get '[PlainText] Version
  :<|> "reset"    :> Post '[] NoContent
  :<|> "proxies"  :> Get '[JSON] (Map Text Proxy)
  :<|> "proxies"  :> ReqBody '[JSON] Proxy :> Post '[JSON] Proxy
  :<|> "proxies"  :> Capture "name" Text :> Get '[JSON] Proxy
  :<|> "populate" :> ReqBody '[JSON] [Proxy] :> Post '[JSON] Populate
  :<|> "proxies"  :> Capture "name" Text :> ReqBody '[JSON] Proxy :> Post '[JSON] Proxy
  :<|> "proxies"  :> Capture "name" Text :> Delete '[] NoContent
  :<|> "proxies"  :> Capture "name" Text :>
       "toxics"   :> Get '[JSON] [Toxic]
  :<|> "proxies"  :> Capture "name" Text :>
       "toxics"   :> ReqBody '[JSON] Toxic :> Post '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" Text :>
       "toxics"   :> Capture "name" Text :> Get '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" Text :>
       "toxics"   :> Capture "name" Text :> ReqBody '[JSON] Toxic :> Get '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" Text :>
       "toxics"   :> Capture "name" Text :> Delete '[JSON] NoContent

type Version = Text

data Proxy = Proxy
  { proxyName     :: Text
  , proxyListen   :: Text
  , proxyUpstream :: Text
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

data Toxic = Toxic
  { toxicName       :: Text
  , toxicType       :: Text
  , toxicStream     :: Text
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

stripPrefixJSON :: String -> String -> String
stripPrefixJSON prefix str =
  case stripPrefix prefix str of
    Nothing             -> str
    Just (first : rest) -> toLower first : rest

toxiproxyAPI :: Proxy.Proxy ToxiproxyAPI
toxiproxyAPI = Proxy.Proxy

getVersion   :: ClientM Version
postReset    :: ClientM NoContent
getProxies   :: ClientM (Map Text Proxy)
createProxy  :: Proxy -> ClientM Proxy
getProxy     :: Text -> ClientM Proxy
postPopulate :: [Proxy] -> ClientM Populate
updateProxy  :: Text -> Proxy -> ClientM Proxy
deleteProxy  :: Text -> ClientM NoContent
getToxics    :: Text -> ClientM [Toxic]
createToxic  :: Text -> Toxic -> ClientM Toxic
getToxic     :: Text -> Text -> ClientM Toxic
updateToxic  :: Text -> Text -> Toxic -> ClientM Toxic
deleteToxic  :: Text -> Text -> ClientM NoContent

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
