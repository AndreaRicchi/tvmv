module API.TMDBSpec (spec) where

import API.TMDB
import Data.Time.Calendar (fromGregorian)
import Domain.API (APIConfig (..))
import Domain.Movie (Movie (..))
import Domain.Show (Episode (..), Season (..), TvShow (..))
import Network.API.TheMovieDB qualified as TMDB
import Test.Hspec

spec :: Spec
spec = do
  describe "mapTvShow" $ do
    it "maps a TV show and all its episodes into our domain" $ do
      mapTvShow tmdbShow `shouldBe` resultShow

  describe "mapMovie" $ do
    it "maps a movie into our domain" $ do
      mapMovie tmdbMovie `shouldBe` resultMovie

    it "leaves the year unset if the movie has no release date" $ do
      mapMovie tmdbMovie {TMDB.movieReleaseDate = Nothing} `shouldBe` resultMovie {movieYear = Nothing}

  describe "tmdbSettings" $ do
    it "passes the configured language along to TMDB" $ do
      let settings = tmdbSettings (APIConfig "someKey" (Just "it-IT"))
      TMDB.tmdbKey settings `shouldBe` "someKey"
      TMDB.tmdbLanguage settings `shouldBe` Just "it-IT"

    it "leaves the language unset if none is configured" $ do
      TMDB.tmdbLanguage (tmdbSettings (APIConfig "someKey" Nothing)) `shouldBe` Nothing

tmdbShow :: TMDB.TV
tmdbShow =
  TMDB.TV
    { TMDB.tvID = 12345,
      TMDB.tvName = "Buffy",
      TMDB.tvOverview = "Good stuff",
      TMDB.tvGenres = [],
      TMDB.tvPopularity = 100,
      TMDB.tvPosterPath = "poster/path/here",
      TMDB.tvFirstAirDate = Nothing,
      TMDB.tvLastAirDate = Nothing,
      TMDB.tvNumberOfSeasons = 7,
      TMDB.tvNumberOfEpisodes = 666,
      TMDB.tvSeasons = [tmdbSeason]
    }

tmdbMovie :: TMDB.Movie
tmdbMovie =
  TMDB.Movie
    { TMDB.movieID = 348,
      TMDB.movieTitle = "Alien",
      TMDB.movieOverview = "In space, etc.",
      TMDB.movieGenres = [],
      TMDB.moviePopularity = 100,
      TMDB.moviePosterPath = "poster/path/here",
      TMDB.movieReleaseDate = Just (fromGregorian 1979 5 25),
      TMDB.movieAdult = False,
      TMDB.movieIMDB = "tt0078748",
      TMDB.movieRunTime = 117
    }

resultMovie :: Movie
resultMovie =
  Movie
    { movieId = 348,
      movieTitle = "Alien",
      movieYear = Just 1979,
      movieDescription = "In space, etc.",
      movieUrl = Just "https://www.themoviedb.org/movie/348"
    }

tmdbSeason :: TMDB.Season
tmdbSeason =
  TMDB.Season
    { TMDB.seasonID = 123,
      TMDB.seasonNumber = 1,
      TMDB.seasonAirDate = Nothing,
      TMDB.seasonEpisodeCount = 22,
      TMDB.seasonPosterPath = "whatever",
      TMDB.seasonEpisodes = [tmdbEpisode]
    }

tmdbEpisode :: TMDB.Episode
tmdbEpisode =
  TMDB.Episode
    { TMDB.episodeID = 9000,
      TMDB.episodeNumber = 12,
      TMDB.episodeName = "Best ep",
      TMDB.episodeOverview = "Stuff happened",
      TMDB.episodeSeasonNumber = 1,
      TMDB.episodeAirDate = Nothing,
      TMDB.episodeStillPath = "path/to/still"
    }

resultShow :: TvShow
resultShow =
  TvShow
    { showId = 12345,
      showName = "Buffy",
      seasons = [resultSeason],
      description = "Good stuff",
      numberOfSeasons = 7,
      numberOfEpisodes = 666,
      showUrl = Just "https://www.themoviedb.org/tv/12345"
    }

resultSeason :: Season
resultSeason = Season {seasonNumber = 1, episodes = [resultEpisode]}

resultEpisode :: Episode
resultEpisode =
  Episode
    { episodeNumber = 12,
      episodeName = "Best ep",
      episodeSeasonNumber = 1,
      episodeShowName = "Buffy"
    }
