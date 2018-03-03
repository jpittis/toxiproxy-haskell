{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Toxiproxy
    ( getVersion
    , postReset
    , getProxies
    , createProxy
    , getProxy
    , Proxy(..)
    , Toxic(..)
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

type ToxiproxyAPI =
       "version"  :> Get '[PlainText] Version
  :<|> "reset"    :> Post '[] NoContent
  :<|> "proxies"  :> Get '[JSON] (Map Text Proxy)
  :<|> "proxies"  :> ReqBody '[JSON] Proxy :> Post '[JSON] Proxy
  :<|> "proxies"  :> Capture "name" Text :> Get '[JSON] Proxy
  :<|> "populate" :> ReqBody '[JSON] [Proxy] :> Post '[JSON] [Proxy]
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
  { toxicName     :: Text
  , toxicType     :: Text
  , toxicStream   :: Text
  , toxicToxicity :: Float
  } deriving (Show, Eq, Generic)

instance FromJSON Toxic where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "toxic" }

instance ToJSON Toxic where
  toJSON = genericToJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "toxic" }

stripPrefixJSON :: String -> String -> String
stripPrefixJSON prefix str =
  case stripPrefix prefix str of
    Nothing             -> str
    Just (first : rest) -> toLower first : rest

toxiproxyAPI :: Proxy.Proxy ToxiproxyAPI
toxiproxyAPI = Proxy.Proxy

getVersion      :: ClientM Version
postReset       :: ClientM NoContent
getProxies      :: ClientM (Map Text Proxy)
createProxy     :: Proxy -> ClientM Proxy
getProxy        :: Text -> ClientM Proxy
populateProxies :: [Proxy] -> ClientM [Proxy]
updateProxy     :: Text -> Proxy -> ClientM Proxy
deleteProxy     :: Text -> ClientM NoContent
getToxics       :: Text -> ClientM [Toxic]
createToxic     :: Text -> Toxic -> ClientM Toxic
getToxic        :: Text -> Text -> ClientM Toxic
updateToxic     :: Text -> Text -> Toxic -> ClientM Toxic
deleteToxic     :: Text -> Text -> ClientM NoContent

(getVersion :<|> postReset
            :<|> getProxies
            :<|> createProxy
            :<|> getProxy
            :<|> populateProxies
            :<|> updateProxy
            :<|> deleteProxy
            :<|> getToxics
            :<|> createToxic
            :<|> getToxic
            :<|> updateToxic
            :<|> deleteToxic) = client toxiproxyAPI
