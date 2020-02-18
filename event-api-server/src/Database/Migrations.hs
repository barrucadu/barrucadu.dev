{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Migrations
  ( M.MigrationResult(..)
  , module Database.Migrations
  ) where

import           Data.ByteString                      (ByteString)
import           Data.Monoid                          ((<>))
import           Database.PostgreSQL.Simple           (Connection,
                                                       connectPostgreSQL,
                                                       rollback,
                                                       withTransaction)
import qualified Database.PostgreSQL.Simple.Migration as M
import           Database.Selda.PostgreSQL            (PGConnectInfo,
                                                       pgConnString)

-- | Run migrations using a Selda database config to connect.
runMigrations :: PGConnectInfo -> IO (M.MigrationResult String)
runMigrations conn = runMigrations' =<< connectPostgreSQL (pgConnString conn)

-- | Run any migrations which haven't yet run.  This is done in a
-- transaction.
runMigrations' :: Connection -> IO (M.MigrationResult String)
runMigrations' conn = withTransaction conn $ M.runMigrations True conn migrations >>= \case
  M.MigrationSuccess   -> pure M.MigrationSuccess
  M.MigrationError err -> rollback conn >> pure (M.MigrationError err)

-- | All migrations
migrations :: [M.MigrationCommand]
migrations =
  [ M.MigrationInitialization
  , M.MigrationScript "0001-add-phase-to-event" (addColumn "events" "eventPhase" "text")
  ]

-------------------------------------------------------------------------------
-- Migration helpers

-- TODO: do not generate SQL by string interpolation!
addColumn :: ByteString -> ByteString -> ByteString -> ByteString
addColumn tbl col ty = "ALTER TABLE public." <> tbl <> " ADD COLUMN \"" <> col <> "\" " <> ty
