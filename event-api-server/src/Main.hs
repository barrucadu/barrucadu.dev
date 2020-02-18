{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (runReaderT)
import qualified Data.ByteString.Base64    as B64
import qualified Data.ByteString.Char8     as B8
import           Data.Maybe                (fromMaybe)
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.UUID                 (UUID)
import qualified Data.UUID                 as UUID
import           Database.Selda            (SeldaM)
import           Database.Selda.PostgreSQL (PGConnectInfo (..), withPostgreSQL)
import           Network.Wai.Handler.Warp  (run)
import           Servant.Auth.Server
import           Servant.Server
import           System.Environment        (getArgs, lookupEnv)
import           System.Exit               (exitFailure)

import qualified API
import qualified Database                  as DB
import qualified Database.Migrations       as DBM
import           Server                    (App (..), apiServer)
import           Util

main :: IO ()
main = do
  conn <- getConnectInfo
  let runDB = withPostgreSQL conn
  getArgs >>= \case
    ["run"] -> do
      cmdMigrate conn
      cmdServe conn =<< getJWTSecret
    ["serve"] -> cmdServe conn =<< getJWTSecret
    ["migrate"] -> cmdMigrate conn
    ["get-project", name] -> runDB (cmdGetProject (T.pack name))
    ["get-token", uuid] -> do
      secret <- getJWTSecret
      case UUID.fromString uuid of
        Just uuid' -> runDB (cmdGetToken secret uuid')
        Nothing    -> die "could not parse uuid"
    ["get-event", uuid] -> case UUID.fromString uuid of
      Just uuid' -> runDB (cmdGetEvent uuid')
      Nothing    -> die "could not parse uuid"
    ["make-db"] -> runDB cmdMakeDb
    ["make-secret"] -> cmdMakeSecret
    ["make-project", name, url, public] -> case readBool public of
      Just public' -> runDB (cmdMakeProject (T.pack name) (T.pack url) public')
      Nothing -> die "could not parse public flag (expected 'true' or 'false')"
    ["make-token", project, owner] -> do
      secret <- getJWTSecret
      runDB (cmdMakeToken secret (T.pack project) (T.pack owner))
    ["invalidate-project", name] -> runDB (cmdInvalidateProject (T.pack name))
    ["invalidate-token", uuid] -> case UUID.fromString uuid of
      Just uuid' -> runDB (cmdInvalidateToken uuid')
      Nothing    -> die "could not parse uuid"
    _ -> do
      putStrLn "usage:"
      putStrLn "    run                                 - migrate and serve"
      putStrLn "    serve                               - start the server on port 3000"
      putStrLn "    migrate                             - run DB migrations only"
      putStrLn "    get-project <name>                  - print a project's details by name"
      putStrLn "    get-token <uuid>                    - print a token's details by UUID"
      putStrLn "    get-event <uuid>                    - print an event's details by UUID"
      putStrLn "    make-db                             - create db tables"
      putStrLn "    make-secret                         - make and print a JWT secret"
      putStrLn "    make-project <name> <url> <public>  - make a new project"
      putStrLn "    make-token <project> <owner>        - make and print a new token"
      putStrLn "    invalidate-project <name>           - invalidate the given project"
      putStrLn "    invalidate-token <uuid>             - invalidate the given token"
      exitFailure

-- | Start the server.
cmdServe :: PGConnectInfo -> JWTSettings -> IO ()
cmdServe conn secret = run 3000 $
  serveWithContext (Proxy @ API.Api) (defaultCookieSettings :. secret :. EmptyContext) $
  hoistServerWithContext (Proxy @ API.Api) (Proxy @ '[CookieSettings, JWTSettings]) (\ma -> runReaderT (runApp ma) conn) apiServer

-- | Perform database migrations
cmdMigrate :: PGConnectInfo -> IO ()
cmdMigrate conn = DBM.runMigrations conn >>= \case
  DBM.MigrationSuccess   -> pure ()
  DBM.MigrationError err -> die err

-- | Get a project by name.
cmdGetProject :: Text -> SeldaM db ()
cmdGetProject name = liftIO . showProject =<< DB.findProject' name

-- | Get a token by UUID.
cmdGetToken :: JWTSettings -> UUID -> SeldaM db ()
cmdGetToken secret uuid = liftIO . showToken secret =<< DB.findToken' uuid

-- | Get an event by UUID.
cmdGetEvent :: UUID -> SeldaM db ()
cmdGetEvent uuid = liftIO . showEvent =<< DB.findEvent' uuid

-- | Create the DB tables.
cmdMakeDb :: SeldaM db ()
cmdMakeDb = DB.makedb

-- | Create the JWT secret.
cmdMakeSecret :: IO ()
cmdMakeSecret = putStrLn . B8.unpack . B64.encode =<< generateSecret

-- | Make a new project.
cmdMakeProject :: Text -> Text -> Bool -> SeldaM db ()
cmdMakeProject name url public = liftIO . showProject . Just =<< DB.createProject public project where
  project = API.Project
    { API.projectName = name
    , API.projectUrl  = if T.null url then Nothing else Just url
    }

-- | Make a new token (and print it).
cmdMakeToken :: JWTSettings -> Text -> Text -> SeldaM db ()
cmdMakeToken secret project owner = liftIO . showToken secret . Just =<< DB.createToken project owner

-- | Invalidate a project.
cmdInvalidateProject :: Text -> SeldaM db ()
cmdInvalidateProject name = liftIO . check =<< DB.invalidateProject name

-- | Invalidate a token.
cmdInvalidateToken :: UUID -> SeldaM db ()
cmdInvalidateToken uuid = liftIO . check =<< DB.invalidateToken uuid

-------------------------------------------------------------------------------

-- | Read PG environment variables
getConnectInfo :: IO PGConnectInfo
getConnectInfo = do
  host     <- T.pack <$> fmap (fromMaybe "localhost") (lookupEnv "PG_HOST")
  port     <- readPort <$> fmap (fromMaybe "5432") (lookupEnv "PG_PORT")
  db       <- fmap T.pack <$> lookupEnv "PG_DB"
  schema   <- fmap T.pack <$> lookupEnv "PG_SCHEMA"
  username <- fmap T.pack <$> lookupEnv "PG_USERNAME"
  password <- fmap T.pack <$> lookupEnv "PG_PASSWORD"

  case (port, db) of
    (Nothing, _) -> die "could not parse PG_PORT (expected integer in range 1..65535)"
    (_, Nothing) -> die "expected PG_DB"
    (Just port', Just db') -> pure PGConnectInfo
      { pgHost     = host
      , pgPort     = port'
      , pgDatabase = db'
      , pgSchema   = schema
      , pgUsername = username
      , pgPassword = password
      }

-- | Read the JWT environment variable
getJWTSecret :: IO JWTSettings
getJWTSecret = lookupEnv "JWT_SECRET" >>= \case
  Just secret -> case B64.decode (B8.pack secret) of
    Right secret' -> pure $ defaultJWTSettings (fromSecret secret')
    _-> die "could not parse JWT_SECRET (expected base64 string)"
  _ -> die "expected JWT_SECRET"

-------------------------------------------------------------------------------

-- | Pretty-print a project.
showProject :: Maybe DB.Project -> IO ()
showProject (Just project) = do
  putStrLn $ "name:       " ++ T.unpack (DB.projectName project)
  putStrLn $ "created at: " ++ show (DB.projectCreatedAt project)
  putStrLn $ "valid:      " ++ show (DB.projectValid project)
  putStrLn $ "public:     " ++ show (DB.projectPublic project)
  putStrLn $ "url:        " ++ maybe "(none)" T.unpack (DB.projectUrl project)
showProject Nothing = die "no such project"

-- | Pretty-print a token.
showToken :: JWTSettings -> Maybe DB.Token -> IO ()
showToken secret (Just token) = do
  putStrLn $ "uuid:       " ++ show (DB.tokenUUID token)
  putStrLn $ "created at: " ++ show (DB.tokenCreatedAt token)
  putStrLn $ "valid:      " ++ show (DB.tokenValid token)
  putStrLn $ "project:    " ++ T.unpack (DB.tokenProject token)
  putStrLn $ "owner:      " ++ T.unpack (DB.tokenOwner token)
  putStrLn ""
  makeJWT (API.Token { API.tokenUUID = DB.tokenUUID token, API.tokenOwner = DB.tokenOwner token }) secret Nothing >>= \case
    Right bs -> print bs
    Left err -> die $ "could not generate JWT data: " ++ show err
showToken _ Nothing = die "no such token"

-- | Pretty-print an event.
showEvent :: Maybe DB.Event -> IO ()
showEvent (Just event) = do
  putStrLn $ "uuid:        " ++ show (DB.eventUUID event)
  putStrLn $ "created at:  " ++ show (DB.eventCreatedAt event)
  putStrLn $ "token:       " ++ show (DB.eventToken event)
  putStrLn $ "project:     " ++ T.unpack (DB.eventProject event)
  putStrLn $ "status:      " ++ show (DB.eventStatus event)
  putStrLn $ "description: " ++ T.unpack (DB.eventDescription event)
  putStrLn $ "tag:         " ++ maybe "(none)" T.unpack (DB.eventTag event)
  putStrLn $ "tag url:     " ++ maybe "(none)" T.unpack (DB.eventTagUrl event)
  putStrLn $ "details url: " ++ maybe "(none)" T.unpack (DB.eventDetailsUrl event)
showEvent Nothing = die "no such event"

-- | Check if something was successful
check :: Bool -> IO ()
check True  = putStrLn "ok"
check False = putStrLn "failed"

-- | Print a message and exit
die :: String -> IO a
die s = putStrLn s >> exitFailure
