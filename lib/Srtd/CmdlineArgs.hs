{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module for cmdline args. Separate module to prevent namespace pollution mostly.
module Srtd.CmdlineArgs where

import Options.Applicative

data Args = Args
  { theme_name :: Maybe String
  , -- SOMEDAY This does nothing right now.
    theme_file :: Maybe String
  }
  deriving (Show)

parser :: Parser Args
parser = do
  theme_name <-
    optional $
      option
        str
        ( long "theme"
            <> metavar "THEME"
            <> help "Select theme (see files in `themes/`)"
        )
  theme_file <-
    optional . option str $
      ( long "theme-file"
          <> metavar "FILE"
          <> help "Use theme file."
      )
  pure Args {..}

opts :: ParserInfo Args
opts = info (parser <**> helper) (fullDesc <> header "srtd - a task management app for humans")

execAppParser :: IO Args
execAppParser = execParser opts
