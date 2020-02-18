{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Server where

import           Control.Monad             (join)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, ask)
import           Data.Text                 (Text)
import           Data.UUID                 (UUID)
import           Database.Selda            (SeldaM)
import           Database.Selda.PostgreSQL (PG, PGConnectInfo, withPostgreSQL)
import           Servant.API
import           Servant.Auth.Server
import           Servant.Server

import qualified API
import qualified Database                  as DB
import           Util

-- | App monad: TODO use connection pooling here.
newtype App a = App { runApp :: ReaderT PGConnectInfo Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadError ServerError, MonadReader PGConnectInfo)

apiServer :: ServerT API.Api App
apiServer = getEvents :<|> getEvent :<|> getProjects :<|> projectApiServer where
  projectApiServer name =
    getProject name :<|> getProjectEvents name :<|> postProjectEvent name

getEvents :: Maybe Int -> ServerT (Get '[JSON] [API.Event]) App
getEvents count = map (uncurry toAPIEvent) <$> runDB (DB.listEvents (toLimit count))

getEvent :: UUID -> ServerT (Get '[JSON] API.Event) App
getEvent uuid = runDB (DB.findEvent uuid) >>= \case
  DB.Found (project, event) -> pure (toAPIEvent project event)
  DB.Invalid -> throwError err410
  DB.Missing -> throwError err404

getProjects :: ServerT (Get '[JSON] [API.Project]) App
getProjects = map toAPIProject <$> runDB (DB.listProjects (toLimit Nothing))

getProject :: Text -> ServerT (Get '[JSON] API.Project) App
getProject projectName = runDB (DB.findProject projectName) >>= \case
  DB.Found project -> pure (toAPIProject project)
  DB.Invalid -> throwError err410
  DB.Missing -> throwError err404

getProjectEvents :: Text -> Maybe Int -> Maybe UUID -> ServerT (Get '[JSON] [API.Event]) App
getProjectEvents projectName count since = join . runDB $ case since of
    Just uuid -> DB.findEvent uuid >>= \case
      DB.Found (_, event) -> go (Just (DB.eventCreatedAt event))
      DB.Invalid -> pure (throwError err410)
      DB.Missing -> pure (throwError err404)
    Nothing -> go Nothing
  where
    go startTime = do
      res <- DB.listEventsForProject projectName (toLimit count) startTime
      pure $ case res of
        DB.Found (project, events) -> pure (map (toAPIEvent project) events)
        DB.Invalid                 -> throwError err410
        DB.Missing                 -> throwError err404

postProjectEvent :: Text -> AuthResult API.Token -> API.Event -> ServerT (Post '[JSON] API.Event) App
postProjectEvent projectName (Authenticated token) event = join . runDB $ DB.validateToken projectName token >>= \case
  DB.Permitted project -> pure . toAPIEvent project <$> DB.createEvent projectName token event
  DB.Forbidden -> pure (throwError err403)
postProjectEvent _ _ _ = throwError err401

-- | Run a database action.
runDB :: SeldaM PG a -> App a
runDB ma = do
  cfg <- ask
  liftIO (withPostgreSQL cfg ma)

-- | Convert a 'DB.Event' to an 'API.Event'.
toAPIEvent :: DB.Project -> DB.Event -> API.Event
toAPIEvent project event = API.Event
  { API.eventUUID        = Just (DB.eventUUID event)
  , API.eventTimestamp   = Just (DB.eventCreatedAt event)
  , API.eventProject     = Just (toAPIProject project)
  , API.eventStatus      = DB.eventStatus event
  , API.eventDescription = DB.eventDescription event
  , API.eventPhase       = DB.eventPhase event
  , API.eventTag         = DB.eventTag event
  , API.eventTagUrl      = DB.eventTagUrl event
  , API.eventDetailsUrl  = DB.eventDetailsUrl event
  }

-- | Convert a 'DB.Project' to an 'API.Project'.
toAPIProject :: DB.Project -> API.Project
toAPIProject project = API.Project
  { API.projectName = DB.projectName project
  , API.projectUrl  = DB.projectUrl project
  }
