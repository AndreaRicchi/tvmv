module Domain.RenameSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import Domain.Movie (Movie (..))
import Domain.Rename
import Domain.Show (Episode (..))
import System.FilePath (normalise)
import Test.Hspec

spec :: Spec
spec = do
  describe "matchEpisodes" $ do
    it "returns an error value if given mismatched number of files/episodes" $ do
      matchEpisodes [welcomeEp] ["two filenames", "but one ep"] `shouldSatisfy` isLeft

    -- Note that we *can't* construct a value to assert against here, as the
    -- `MatchedEpisodes` data constructor is not exposed. Our `renameFiles`
    -- tests will also cover this, however.
    it "returns a success value if the input lists are the same length" $ do
      matchEpisodes [welcomeEp, hushEp] ["two filenames", "and two eps"] `shouldSatisfy` isRight

  describe "matchEpisodesAllowPartial" $ do
    it "allows a match if there are more episodes than files" $ do
      matchEpisodesAllowPartial [welcomeEp, hushEp] ["only one file"] `shouldSatisfy` isRight

    it "allows an empty list of files" $ do
      matchEpisodesAllowPartial [welcomeEp, hushEp] [] `shouldSatisfy` isRight

    it "still returns an error value if there are more files than episodes" $ do
      matchEpisodesAllowPartial [welcomeEp] ["two filenames", "but one ep"] `shouldSatisfy` isLeft

  describe "renameFiles" $ do
    it "generates the correct file names" $ do
      let matchedEps = matchEpisodes [welcomeEp, hushEp] welcomeHushFileNames
      renameFiles <$> matchedEps `shouldBe` Right expectedRenameOps

    it "returns an empty list if given empty lists" $ do
      renameFiles <$> matchEpisodes [] [] `shouldBe` Right []

  describe "renameFile" $ do
    it "generates the correct file name" $ do
      renameFile hushEp "/root/buffy/season 4/buff 4.10.mkv"
        `shouldBe` RenameOp
          { oldPath = "/root/buffy/season 4/buff 4.10.mkv",
            newPath = p "/root/buffy/season 4/Buffy the Vampire Slayer - s04e10 - Hush.mkv"
          }

    it "makes invalid file names valid" $ do
      renameFile invalidNameEp "/root/buffy/season 42/buff 42.666.mkv"
        `shouldBe` RenameOp
          { oldPath = "/root/buffy/season 42/buff 42.666.mkv",
            newPath = p "/root/buffy/season 42/Buffy the Vampire Slayer - s42e666 - The A-B-Cs of dusting vamps and the 1-2-3s of dating them.mkv"
          }

  describe "renameMovieFiles" $ do
    it "names a movie's file after its title and year" $ do
      renameMovieFiles alien ["/root/movies/a1.mkv"]
        `shouldBe` Right [RenameOp {oldPath = "/root/movies/a1.mkv", newPath = p "/root/movies/Alien (1979).mkv"}]

    it "leaves the year off if there isn't one" $ do
      renameMovieFiles alien {movieYear = Nothing} ["/root/movies/a1.mkv"]
        `shouldBe` Right [RenameOp {oldPath = "/root/movies/a1.mkv", newPath = p "/root/movies/Alien.mkv"}]

    it "gives all of a movie's files the same name, keeping subtitle language metadata" $ do
      renameMovieFiles alien ["a1.mkv", "a1.en.srt", "a1.de.forced.srt"]
        `shouldBe` Right
          [ RenameOp {oldPath = "a1.mkv", newPath = "Alien (1979).mkv"},
            RenameOp {oldPath = "a1.en.srt", newPath = "Alien (1979).en.srt"},
            RenameOp {oldPath = "a1.de.forced.srt", newPath = "Alien (1979).de.forced.srt"}
          ]

    it "makes invalid file names valid" $ do
      renameMovieFiles alien {movieTitle = "Face/Off"} ["/root/movies/fo.mkv"]
        `shouldBe` Right [RenameOp {oldPath = "/root/movies/fo.mkv", newPath = p "/root/movies/Face-Off (1979).mkv"}]

    it "returns an error value if two files would get the same name" $ do
      renameMovieFiles alien ["a1.mkv", "some-other-movie.mkv", "a1.en.srt"] `shouldSatisfy` isLeft

    it "returns an empty list if given no files" $ do
      renameMovieFiles alien [] `shouldBe` Right []

  describe "undoRenameOp" $ do
    it "flip the old/new args for a given op" $ do
      undoRenameOp RenameOp {oldPath = "old/path/to/file.mkv", newPath = "new/path/to/file.mkv"}
        `shouldBe` RenameOp
          { oldPath = "new/path/to/file.mkv",
            newPath = "old/path/to/file.mkv"
          }

-- `normalise` replaces `/` with the correct system path separator. Just a
-- shortcut so we can quickly use it all over the place. Without doing this, a
-- lot of these tests will break in Windows.
p :: FilePath -> FilePath
p = normalise

welcomeHushFileNames :: [FilePath]
welcomeHushFileNames = ["/root/buffy/season 1/buff 1.1.mkv", "/root/buffy/season 4/buff 4.10.mkv"]

expectedRenameOps :: [RenameOp]
expectedRenameOps =
  [ RenameOp
      { oldPath = "/root/buffy/season 1/buff 1.1.mkv",
        newPath = p "/root/buffy/season 1/Buffy the Vampire Slayer - s01e01 - Welcome to the Hellmouth.mkv"
      },
    RenameOp
      { oldPath = "/root/buffy/season 4/buff 4.10.mkv",
        newPath = p "/root/buffy/season 4/Buffy the Vampire Slayer - s04e10 - Hush.mkv"
      }
  ]

alien :: Movie
alien =
  Movie
    { movieId = 348,
      movieTitle = "Alien",
      movieYear = Just 1979,
      movieDescription = "In space, etc.",
      movieUrl = Nothing
    }

buffy :: T.Text
buffy = "Buffy the Vampire Slayer"

welcomeEp :: Episode
welcomeEp =
  Episode
    { episodeNumber = 1,
      episodeName = "Welcome to the Hellmouth",
      episodeSeasonNumber = 1,
      episodeShowName = buffy
    }

hushEp :: Episode
hushEp =
  Episode
    { episodeNumber = 10,
      episodeName = "Hush",
      episodeSeasonNumber = 4,
      episodeShowName = buffy
    }

invalidNameEp :: Episode
invalidNameEp =
  Episode
    { episodeNumber = 666,
      -- Note the various invalid (filename) characters D:
      episodeName = "The A/B/Cs of dusting vamps\nand the 1\\2\\3s of dating them",
      episodeSeasonNumber = 42,
      episodeShowName = buffy
    }
