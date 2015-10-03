{-# LANGUAGE OverloadedStrings #-}
module Web.Postgres() where

import Data.Pool (createPool, withResource, Pool)
import Database.PostgreSQL.Simple


withTx :: Pool Connection -> (Connection -> IO a) -> IO a
withTx pool ioa = withResource pool (\conn -> withTransaction conn (ioa conn))
