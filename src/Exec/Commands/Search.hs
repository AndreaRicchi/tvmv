module Exec.Commands.Search (search) where

import API qualified
import Command (SearchOptions (..))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Domain.API (APIWrapper)
import Domain.Error (Error)
import Exec.Env (Env)
import Print.Color (Colorized (..))
import Text.Printf (printf)

-- Query the configured API for a show (or a movie) with the given name. The
-- `search` command!
search :: (MonadIO m, MonadError Error m) => Env -> APIWrapper m -> SearchOptions -> m ()
search env withApi (SearchOptions maybeApiKey maybeLanguage movieMode searchQuery) = do
  apiConfig <- API.resolveAPIConfig maybeApiKey maybeLanguage env
  putStrLn' "Querying API..."
  if movieMode
    then API.searchMoviesByName withApi apiConfig searchQuery >>= printResults
    else API.searchShowByName withApi apiConfig searchQuery >>= printResults
  where
    printResults results = do
      putStrLn' $ resultsMsg results
      printColorizedListLn results
    resultsMsg r = printf "Found %d results\n" (length r)

-- Wrapper for less lifting :')
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' = liftIO . putStrLn
