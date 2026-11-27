module API.Stub (testAPI) where

import Data.Text qualified as T
import Domain.API (APIConfig, APIWrapper (..))
import Domain.Movie (Movie (..))
import Domain.Show (Episode (..), ItemId, Season (..), TvShow (..))
import Monad.Tvmv (Tvmv)

-- Just a simple stub! Hard-coded to return season 12 of Poirot (or, for
-- movies, Alien) for now, but should be easy to flesh it out if we need more
-- complex tests.
testAPI :: APIWrapper Tvmv
testAPI =
  APIWrapper
    { getShow = getShowStub,
      getSeason = getSeasonStub,
      queryShows = queryShowsStub,
      getMovie = getMovieStub,
      queryMovies = queryMoviesStub
    }

getMovieStub :: Monad m => APIConfig -> ItemId -> m Movie
getMovieStub _ _ = return alienMovieData

queryMoviesStub :: Monad m => APIConfig -> T.Text -> m [Movie]
queryMoviesStub _ _ = return [alienMovieData]

alienMovieData :: Movie
alienMovieData =
  Movie
    { movieId = 348,
      movieTitle = "Alien",
      movieYear = Just 1979,
      movieDescription = "!!!",
      movieUrl = Nothing
    }

getShowStub :: Monad m => APIConfig -> ItemId -> m TvShow
getShowStub _ _ = return poirotShowData

getSeasonStub :: Monad m => APIConfig -> TvShow -> Int -> m Season
getSeasonStub _ _ _ = return Season {seasonNumber = 12, episodes = poirotSeasonEps}

poirotSeasonEps :: [Episode]
poirotSeasonEps =
  [ Episode 1 "Three Act Tragedy" 12 poirotName,
    Episode 2 "Hallowe'en Party" 12 poirotName,
    Episode 3 "Murder on the Orient Express" 12 poirotName,
    Episode 4 "The Clocks" 12 poirotName
  ]

queryShowsStub :: Monad m => APIConfig -> T.Text -> m [TvShow]
queryShowsStub _ _ = return [poirotShowData]

poirotName :: T.Text
poirotName = "Agatha Christie's Poirot"

poirotShowData :: TvShow
poirotShowData =
  TvShow
    { showId = 790,
      showName = poirotName,
      seasons = [],
      description = "!!!",
      numberOfSeasons = 0,
      numberOfEpisodes = 0,
      showUrl = Nothing
    }
