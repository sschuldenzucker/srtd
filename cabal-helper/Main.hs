{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Monad (forM)
import Control.Monad.State
import Data.ByteString qualified as BS
import Data.List (isSuffixOf, nub, sort)
import Data.Maybe (catMaybes)
import Distribution.ModuleName (ModuleName)
import Distribution.PackageDescription (GenericPackageDescription (..), Library (..))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Text qualified as DText
import Distribution.Types.CondTree (mapCondTree)
import Distribution.Types.Lens qualified as L
import Lens.Micro
import Options.Applicative
import System.Directory
import System.Exit (die)

main :: IO ()
main = execParser cmdParser >>= runCmd

data Cmd
  = AddExposedModule AddExposedModuleOpts
  | RemoveExposedModule RemoveExposedModuleOpts
  deriving (Show)

data AddExposedModuleOpts = AddExposedModuleOpts ModuleName
  deriving (Show)

data RemoveExposedModuleOpts = RemoveExposedModuleOpts ModuleName
  deriving (Show)

cmdParser :: ParserInfo Cmd
cmdParser =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Small utility to change .cabal files programmatically, beyond cabal-tool"
        <> header "cabal-helper"
    )
 where
  parser :: Parser Cmd
  parser =
    hsubparser $
      ( command
          "add-exposed-module"
          ( info
              (AddExposedModule <$> addExposedModuleOptsParser)
              (progDesc "Add an entry to exposed-modules of the library section")
          )
          <> metavar "COMMAND"
      )
        <> ( command
               "remove-exposed-module"
               ( info
                   (RemoveExposedModule <$> removeExposedModuleOptsParser)
                   (progDesc "Remove an entry from exposed-modules of the library section")
               )
           )
  addExposedModuleOptsParser = do
    moduleName <-
      argument
        (maybeReader DText.simpleParse)
        ( metavar "MODULE"
            <> help "Module name, e.g., Foo.Bar.Qux"
        )
    pure $ AddExposedModuleOpts moduleName
  removeExposedModuleOptsParser = do
    moduleName <-
      argument
        (maybeReader DText.simpleParse)
        ( metavar "MODULE"
            <> help "Module name, e.g., Foo.Bar.Qux"
        )
    pure $ RemoveExposedModuleOpts moduleName

runCmd :: Cmd -> IO ()
runCmd (AddExposedModule (AddExposedModuleOpts modname)) = do
  cabalFile <- findCabalFile
  cabalFileContent <- BS.readFile cabalFile
  let mgpd = parseGenericPackageDescriptionMaybe cabalFileContent
  case mgpd of
    Nothing -> die $ "Unreadable .cabal file: " ++ cabalFile
    Just gpd -> do
      let gpd' = addExposedModule modname gpd
      writeGenericPackageDescription cabalFile gpd'
runCmd (RemoveExposedModule (RemoveExposedModuleOpts modname)) = do
  cabalFile <- findCabalFile
  cabalFileContent <- BS.readFile cabalFile
  let mgpd = parseGenericPackageDescriptionMaybe cabalFileContent
  case mgpd of
    Nothing -> die $ "Unreadable .cabal file: " ++ cabalFile
    Just gpd -> do
      let mgpd' = tryRemoveExposedModule modname gpd
      case mgpd' of
        Nothing ->
          die $
            "Module not in project: " ++ show modname
        Just gpd' ->
          writeGenericPackageDescription cabalFile gpd'

-- | Add exposed module to the default library.
addExposedModule :: ModuleName -> GenericPackageDescription -> GenericPackageDescription
-- addExposedModule m gpd = gpd {condLibrary = fmap (mapCondTree (_addExposedModule m)) (condLibrary gpd)}
addExposedModule m = L.condLibrary %~ fmap (mapCondTree go id id)
 where
  go :: Library -> Library
  go = L.exposedModules %~ nub . sort . (m :)

tryRemoveExposedModule ::
  ModuleName -> GenericPackageDescription -> Maybe GenericPackageDescription
tryRemoveExposedModule m gpd = case runState act False of
  (_, False) -> Nothing
  (gpd', True) -> Just gpd'
 where
  act :: State Bool GenericPackageDescription
  act = forOf L.condLibrary gpd $ traverse (traverse goLib)
  goLib :: Library -> State Bool Library
  goLib lib =
    forOf L.exposedModules lib $ \mods ->
      fmap catMaybes . forM mods $ \mo ->
        if mo == m
          then put True >> return Nothing
          else return (Just mo)

findCabalFile :: IO FilePath
findCabalFile = do
  files <- listDirectory "."
  let cabals = filter (".cabal" `isSuffixOf`) files
  case cabals of
    [path] -> return path
    [] -> die "No .cabal file found in current path"
    _ -> die "Multiple .cabal files found in current path"
