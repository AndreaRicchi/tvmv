module Domain.Movie
  ( Movie (..),
    movieInfoBrief,
  )
where

import Data.Text qualified as T
import Domain.Show (ItemId, wrapAndTrim)
import Print.Color (Colorized (..), cyan, green, mono, uncolor, yellow)
import Print.Pretty (Pretty (..))

data Movie = Movie
  { movieId :: ItemId,
    movieTitle :: T.Text,
    -- The release year. Not every movie has a release date (unreleased or
    -- obscure stuff, e.g.), in which case this is `Nothing`.
    movieYear :: Maybe Integer,
    movieDescription :: T.Text, -- Some "flavor text"
    -- Can/should be populated by the API code, will be API-specific. If it's
    -- not populated (`Nothing`), the field will not be shown:
    movieUrl :: Maybe T.Text
  }
  deriving (Eq, Show)

instance Colorized Movie where
  colorize m =
    "ID:\t"
      <> green (toText (movieId m))
      <> "\nName:\t"
      <> yellow (movieTitle m)
      <> year (movieYear m)
      <> "\nBlurb:\t"
      <> mono (wrapAndTrim (movieDescription m))
      <> url (movieUrl m)
    where
      year Nothing = ""
      year (Just y) = "\nYear:\t" <> mono (toText y)
      url Nothing = ""
      url (Just u) = "\nLink:\t" <> cyan u

instance Pretty Movie where
  prettyText = movieInfoBrief

-- Brief text summary of a movie. As with TV shows, the colored text is the
-- "base" and we UNcolor it for the plain text version.
movieInfoBrief :: Movie -> T.Text
movieInfoBrief = uncolor . colorize

toText :: (Show a) => a -> T.Text
toText = T.pack . show
