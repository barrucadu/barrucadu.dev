{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module Database where

import           Control.Monad.IO.Class       (liftIO)
import           Data.Maybe                   (listToMaybe)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.UUID                    (UUID)
import           Database.Selda               hiding (Result)
import           Database.Selda.MakeSelectors
import           GHC.Generics                 (Generic)

import qualified API
import           Util

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
  , projectUrl       :: Maybe Text
  }
 deriving (Eq, Ord, Read, Show, Generic)

instance SqlRow Project

-- | Database table and selectors for projects.
projects :: Table Project
(projects, dbProjectName :*: dbProjectCreatedAt :*: dbProjectValid :*: dbProjectPublic :*: dbProjectUrl) =
  tableWithSelectors "projects" [#projectName :- primary]

-- | Build events.  This adds a project association to the 'API.Event'
-- type and makes the UUID required.
data Event = Event
  { eventUUID        :: UUID
  , eventCreatedAt   :: UTCTime
  , eventToken       :: UUID
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
(events, dbEventUUID :*: dbEventCreatedAt :*: dbEventToken :*: dbEventProject :*: dbEventStatus :*: dbEventDescription :*: dbEventTag :*: dbEventTagUrl :*: dbEventDetailsUrl) =
  tableWithSelectors "events" [#eventUUID :- primary, #eventToken :- foreignKey tokens dbTokenUUID, #eventProject :- foreignKey projects dbProjectName]

-------------------------------------------------------------------------------

-- | Create a token, generating a fresh UUID and using the current
-- time.
createToken :: Text -> Text -> SeldaM db Token
createToken projectName_ owner = do
  now  <- liftIO getCurrentTime
  uuid <- liftIO genUUID
  createToken' projectName_ owner now uuid

-- | Create a token, using the given time and UUID.
createToken' :: Text -> Text -> UTCTime -> UUID -> SeldaM db Token
createToken' projectName_ owner now uuid = insert_ tokens [t] >> pure t where
  t = Token
      { tokenUUID      = uuid
      , tokenCreatedAt = now
      , tokenValid     = True
      , tokenProject   = projectName_
      , tokenOwner     = owner
      }

-- | Create a project, using the current time.
createProject :: Bool -> API.Project -> SeldaM db Project
createProject public project = do
  now <- liftIO getCurrentTime
  createProject' public project now

-- | Create a project, using the given time.
createProject' :: Bool -> API.Project -> UTCTime -> SeldaM db Project
createProject' public project now = insert_ projects [p] >> pure p where
  p = Project
      { projectName      = API.projectName project
      , projectCreatedAt = now
      , projectValid     = True
      , projectPublic    = public
      , projectUrl       = API.projectUrl project
      }

-- | Create an event, generating a fresh UUID and using the current
-- time.
createEvent :: Text -> API.Token -> API.Event -> SeldaM db Event
createEvent projectName_ token event = do
  now  <- liftIO getCurrentTime
  uuid <- liftIO genUUID
  createEvent' projectName_ token event now uuid

-- | Create an event, using the given time and UUID.
createEvent' :: Text -> API.Token -> API.Event -> UTCTime -> UUID -> SeldaM db Event
createEvent' projectName_ token event now uuid = insert_ events [e] >> pure e where
  e = Event
      { eventUUID        = uuid
      , eventCreatedAt   = now
      , eventToken       = API.tokenUUID token
      , eventProject     = projectName_
      , eventStatus      = API.eventStatus event
      , eventDescription = API.eventDescription event
      , eventTag         = API.eventTag event
      , eventTagUrl      = API.eventTagUrl event
      , eventDetailsUrl  = API.eventDetailsUrl event
      }

-------------------------------------------------------------------------------

-- | List projects (in lexicographical order, up to the limit) which
-- are public and valid.
listProjects :: Int -> SeldaM db [Project]
listProjects lim = query . limit 0 lim $ do
  p <- select projects
  restrict (p ! dbProjectPublic)
  restrict (p ! dbProjectValid)
  pure p

-- | List events (in reverse chronological order, up to the limit)
-- which belong to a public, valid, project.
listEvents :: Int -> SeldaM db [Event]
listEvents lim = query . limit 0 lim $ do
  e <- select events
  _ <- innerJoin (\p -> p ! dbProjectName .== e ! dbEventProject) $ do
    p <- select projects
    restrict (p ! dbProjectPublic)
    restrict (p ! dbProjectValid)
    pure p
  pure e

-- | Find a project by name.
findProject :: Text -> SeldaM db (Result Project)
findProject projectName_ = findProject' projectName_ >>= \case
  Just project -> case (projectPublic project, projectValid project) of
    (False, _) -> pure Missing
    (_, False) -> pure Invalid
    _          -> pure (Found project)
  _ -> pure Missing

-- | Find a project by name, returning even private and invalid ones.
findProject' :: Text -> SeldaM db (Maybe Project)
findProject' projectName_ = fmap listToMaybe . query $ do
  p <- select projects
  restrict (p ! dbProjectName .== literal projectName_)
  pure p

-- | Find an event by UUID.
findEvent :: UUID -> SeldaM db (Result Event)
findEvent uuid = do
  results <- query $ do
    e <- select events
    restrict (e ! dbEventUUID .== literal uuid)
    p <- innerJoin (\p -> p ! dbProjectName .== e ! dbEventProject) (select projects)
    pure (e :*: p)
  pure $ case results of
    [event :*: project] -> case (projectPublic project, projectValid project) of
      (False, _) -> Missing
      (_, False) -> Invalid
      _          -> Found event
    _ -> Missing

-- | Find an event by UUID, returning even private and invalid ones.
findEvent' :: UUID -> SeldaM db (Maybe Event)
findEvent' uuid = fmap listToMaybe . query $ do
  e <- select events
  restrict (e ! dbEventUUID .== literal uuid)
  pure e

-- | Find a token by UUID, returning even private and invalid ones.
findToken' :: UUID -> SeldaM db (Maybe Token)
findToken' uuid = fmap listToMaybe . query $ do
  t <- select tokens
  restrict (t ! dbTokenUUID .== literal uuid)
  pure t

-- | List events (in reverse chronological order, up to the limit)
-- which belong to the given project (if it's public and valid).
listEventsForProject :: Text -> Int -> SeldaM db (Result [Event])
listEventsForProject projectName_ lim = findProject projectName_ >>= \case
  Found _ -> fmap Found . query . limit 0 lim $ do
    e <- select events
    restrict (e ! dbEventProject .== literal projectName_)
    pure e
  Missing -> pure Missing
  Invalid -> pure Invalid

-- | Check if a token is valid for a project.
validateToken :: Text -> API.Token -> SeldaM db Auth
validateToken projectName_ token = do
  results <- query $ do
    t <- select tokens
    restrict (t ! dbTokenUUID .== literal (API.tokenUUID token))
    restrict (t ! dbTokenOwner .== literal (API.tokenOwner token))
    restrict (t ! dbTokenProject .== literal projectName_)
    restrict (t ! dbTokenValid)
    pure t
  pure $ case results of
    [_] -> Permitted
    _   -> Forbidden

-- | Mark a project as invalid.
invalidateProject :: Text -> SeldaM db Bool
invalidateProject projectName_ = do
  n <- update projects (\p -> p ! dbProjectName .== literal projectName_) (\p -> with p [dbProjectValid := literal False])
  pure (n == 1)

-- | Mark a token as invalid.
invalidateToken :: UUID -> SeldaM db Bool
invalidateToken uuid = do
  n <- update tokens (\t -> t ! dbTokenUUID .== literal uuid) (\t -> with t [dbTokenValid := literal False])
  pure (n == 1)

-- | Three-way bool (trool?) which separates "thing doesn't exist"
-- from "thing is invalid".
data Result a = Found a | Invalid | Missing
  deriving (Eq, Ord, Read, Show)

-- | Bool alternative for token validity.
data Auth = Permitted | Forbidden
  deriving (Eq, Ord, Read, Show, Enum, Bounded)
