{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module Database where

import           Data.Time.Clock              (UTCTime)
import           Data.UUID                    (UUID)
import           Database.Selda               hiding (Result)
import           Database.Selda.MakeSelectors
import           GHC.Generics                 (Generic)

import qualified API

-- | Create the database.
makedb :: SeldaM db ()
makedb = do
  createTable projects
  createTable tokens
  createTable events

-- | Authentication tokens.  This adds a project association, creation
-- time, and validity flag to the 'API.Token' type.
data Token = Token
  { tokenUUID      :: UUID
  , tokenCreatedAt :: UTCTime
  , tokenValid     :: Bool
  , tokenProject   :: Text
  , tokenOwner     :: Text
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance SqlRow Token

-- | Database table and selectors for tokens.
tokens :: Table Token
(tokens, dbTokenUUID :*: dbTokenCreatedAt :*: dbTokenValid :*: dbTokenProject :*: dbTokenOwner) =
  tableWithSelectors "tokens" [#tokenUUID :- primary, #tokenProject :- foreignKey projects dbProjectName]

-- | Projects.  This adds a publicity flag (only public projects can
-- be queried by the API), validity flag, and creation time to the
-- 'API.Project' type.
data Project = Project
  { projectName      :: Text
  , projectCreatedAt :: UTCTime
  , projectValid     :: Bool
  , projectPublic    :: Bool
  , projectURL       :: Maybe Text
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance SqlRow Project

-- | Database table and selectors for projects.
projects :: Table Project
(projects, dbProjectName :*: dbProjectCreatedAt :*: dbProjectValid :*: dbProjectPublic :*: dbProjectURL) =
  tableWithSelectors "projects" [#projectName :- primary]

-- | Build events.  This adds a project association to the 'API.Event'
-- type and makes the UUID required.
data Event = Event
  { eventUUID        :: UUID
  , eventCreatedAt   :: UTCTime
  , eventProject     :: Text
  , eventStatus      :: API.Status
  , eventDescription :: Text
  , eventTag         :: Maybe Text
  , eventTagUrl      :: Maybe Text
  , eventDetailsUrl  :: Maybe Text
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance SqlType API.Status
instance SqlRow Event

-- | Database table and selectors for events.
events :: Table Event
(events, dbEventUUID :*: dbEventCreatedAt :*: dbEventProject :*: dbEventStatus :*: dbEventDescription :*: dbEventTag :*: dbEventTagUrl :*: dbEventDetailsUrl) =
  tableWithSelectors "events" [#eventUUID :- primary, #eventProject :- foreignKey projects dbProjectName]

-------------------------------------------------------------------------------

-- | List projects (in lexicographical order, up to the limit) which
-- are public and valid.
listProjects :: Int -> SeldaM db [Project]
listProjects _lim = undefined

-- | List events (in reverse chronological order, up to the limit)
-- which belong to a public, valid, project.
listEvents :: Int -> SeldaM db [Event]
listEvents _lim = undefined

-- | Find a project by name.
findProject :: Text -> SeldaM db (Result Project)
findProject _projectName = undefined

-- | Find an event by UUID.
findEvent :: UUID -> SeldaM db (Result Event)
findEvent _uuid = undefined

-- | List events (in reverse chronological order, up to the limit)
-- which belong to the given project (if it's public and valid).
listEventsForProject :: Text -> Int -> SeldaM db (Result [Event])
listEventsForProject _projectName _lim = undefined

-- | Check if a token is valid for a project.
validateToken :: Text -> API.Token -> SeldaM db Auth
validateToken _projectName _token = undefined

-- | Three-way bool (trool?) which separates "thing doesn't exist"
-- from "thing is invalid".
data Result a = Found a | Invalid | Missing
  deriving (Eq, Ord, Read, Show)

-- | Bool alternative for token validity.
data Auth = Permitted | Forbidden
