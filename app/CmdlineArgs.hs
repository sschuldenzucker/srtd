{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module for cmdline args. Separate module to prevent namespace pollution mostly.
module CmdlineArgs where

import AppAttr
import Data.List (intercalate)
import Options.Applicative

data Args = Args
  { theme :: Maybe AppTheme
  }
  deriving (Show)

allThemes :: [(String, AppTheme)]
allThemes = [("dark", CatppuccinDark), ("light", CatppuccinLight)]

parseTheme :: ReadM AppTheme
parseTheme = eitherReader $ \theme -> case lookup theme allThemes of
  Just res -> Right res
  Nothing -> Left $ "Unknown theme: " ++ theme

parser :: Parser Args
parser = do
  theme <-
    optional $
      option
        parseTheme
        ( long "theme"
            <> metavar "THEME"
            <> help ("Select theme: " ++ intercalate ", " (map fst allThemes))
        )
  pure Args {..}

opts :: ParserInfo Args
opts = info (parser <**> helper) (fullDesc <> header "srtd - a task management app for humans")

execAppParser :: IO Args
execAppParser = execParser opts
