{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Aeson.Types    as AT
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)
import           Data.UUID           (UUID)
import           GHC.Generics        (Generic)
import           Servant.API
import           Servant.Auth
import           Servant.Auth.Server (FromJWT, ToJWT)

import           Util

-- | The API.  Endpoints are:
--
-- @
-- GET /events?count=:count
-- GET /event/:uuid
-- GET /projects
-- GET /project/:name
-- GET /project/:name/events?count=:count
-- POST /project/:name/event
-- @
type Api =
       "events" :> QueryParam "count" Int :> Get '[JSON] [Event]
  :<|> "event" :> Capture "event" UUID :> Get '[JSON] Event
  :<|> "projects" :> Get '[JSON] [Project]
  :<|> "project" :> Capture "name" Text :> ProjectApi

type ProjectApi =
       Get '[JSON] Project
  :<|> "events" :> QueryParam "count" Int :> Get '[JSON] [Event]
  :<|> "event" :> Auth '[JWT] Token :> ReqBody '[JSON] Event :> Post '[JSON] Event

-- | Authentication token supplied on write requests.  Project name
-- comes from the URL, and the combination of UUID, owner, and project
-- is checked in the database to see if the permission exists.
data Token = Token
  { tokenUUID  :: UUID
  , tokenOwner :: Text
  }
  deriving (Eq, Ord, Read, Show, Generic)


instance ToJSON Token where
  toJSON = AT.genericToJSON (AT.defaultOptions { AT.fieldLabelModifier = fieldLabelModifier "token" })

instance FromJSON Token where
  parseJSON = AT.genericParseJSON (AT.defaultOptions { AT.fieldLabelModifier = fieldLabelModifier "token" })

instance ToJWT Token
instance FromJWT Token

-- | Build project.  Not mutable from the API, these exist only in the
-- db.
data Project = Project
  { projectName :: Text
  , projectUrl  :: Maybe Text
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON Project where
  toJSON = AT.genericToJSON (AT.defaultOptions { AT.fieldLabelModifier = fieldLabelModifier "project" })

instance FromJSON Project where
  parseJSON = AT.genericParseJSON (AT.defaultOptions { AT.fieldLabelModifier = fieldLabelModifier "project" })

-- | Build event.  Any timestamp or UUID sent in the initial POST is
-- ignored: these are generated when the event is inserted into the
-- database.
data Event = Event
  { eventUUID        :: Maybe UUID
  , eventTimestamp   :: Maybe UTCTime
  , eventStatus      :: Status
  , eventDescription :: Text
  , eventTag         :: Maybe Text
  , eventTagUrl      :: Maybe Text
  , eventDetailsUrl  :: Maybe Text
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON Event where
  toJSON = AT.genericToJSON (AT.defaultOptions { AT.fieldLabelModifier = fieldLabelModifier "event" })

instance FromJSON Event where
  parseJSON = AT.genericParseJSON (AT.defaultOptions { AT.fieldLabelModifier = fieldLabelModifier "event" })

-- | Build event status
data Status = Ok | Failure | Error
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance ToJSON Status
instance FromJSON Status
