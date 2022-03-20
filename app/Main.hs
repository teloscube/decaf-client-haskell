module Main where

import           Data.Aeson                           (Value, (.:))
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.ByteString.Lazy.Char8           as BLC
import           Data.Char                            (toLower)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Text                            as T
import           Data.Version                         (showVersion)
import qualified Decaf.Client                         as DC
import           Decaf.Client.Cli.SubCommands.Tui.Tui (runTui)
import           ExampleProfiles                      (runExampleProfiles)
import           GHC.Generics                         (Generic)
import           MicrolotBatchRunner                  (MicrolotBatchRunConfig(MicrolotBatchRunConfig), runBatchMicrolot)
import qualified Options.Applicative                  as OA
import           Paths_decaf_client                   (version)
import           System.Exit                          (ExitCode, die, exitFailure, exitSuccess, exitWith)
import           System.IO                            (hPutStrLn, stderr)


-- | Entrypoint of the CLI program.
main :: IO ()
main = exitWith =<< (cliProgram =<< OA.execParser cliProgramParserInfo)


-- | CLI program.
cliProgram :: Command -> IO ExitCode
cliProgram (CommandTui config)      = runTui config >> exitSuccess
cliProgram (CommandMicrolot config) = runBatchMicrolot config >> exitSuccess
cliProgram (CommandProfiles config) = runExampleProfiles config >> exitSuccess
cliProgram (CommandVersions fp)     = hPutStrLn stderr "Not implemented yet." >> exitFailure


-- | Commands registry
data Command =
    CommandTui FilePath
  | CommandMicrolot MicrolotBatchRunConfig
  | CommandProfiles FilePath
  | CommandVersions FilePath


-- | CLI arguments parser.
parserProgramOptions :: OA.Parser Command
parserProgramOptions = OA.hsubparser
  (  OA.command "tui" (OA.info (CommandTui <$> profileFilePathParser) (OA.progDesc "Runs the TUI application"))
  <> OA.command "microlot" (OA.info (CommandMicrolot <$> microlotRunConfigParser) (OA.progDesc "Run DECAF Microlot query over profiles"))
  <> OA.command "example-profiles" (OA.info (CommandProfiles <$> profileFilePathParser) (OA.progDesc "Produce example yaml file for profiles"))
  <> OA.command "versions" (OA.info (CommandVersions <$> profileFilePathParser) (OA.progDesc "Get DECAF Barista versions for all profiles"))
  )


-- | Version option.
parserVersionOption :: OA.Parser (a -> a)
parserVersionOption = OA.infoOption (showVersion version) (OA.long "version" <> OA.help "Show version")


-- | CLI program information.
cliProgramParserInfo :: OA.ParserInfo Command
cliProgramParserInfo = OA.info
  (OA.helper <*> parserVersionOption <*> parserProgramOptions)
  (OA.fullDesc <> OA.progDesc "DECAF Client" <> OA.header "decafcli - DECAF Command-line Client Application")


-- * Helpers
-- $helpers


microlotRunConfigParser :: OA.Parser MicrolotBatchRunConfig
microlotRunConfigParser = MicrolotBatchRunConfig
  <$> profileFilePathParser
  <*> OA.optional profileNameParser
  <*> OA.strOption (OA.long "query" <> OA.metavar "QUERY" <> OA.help "Microlot GraphQL query")
  <*> OA.optional (queryParametersOption (OA.long "params" <> OA.metavar "PARAM" <> OA.help "Microlot GraphQL query parameters"))


profileFilePathParser :: OA.Parser FilePath
profileFilePathParser =
  OA.strOption (OA.long "file-profiles" <> OA.metavar "FILE-PROFILES" <> OA.help "Path to profiles path")


profileNameParser :: OA.Parser T.Text
profileNameParser =
  OA.strOption (OA.long "profile" <> OA.metavar "PROFILE-NAME" <> OA.help "Name of the profile")


queryParametersOption :: OA.Mod OA.OptionFields Value -> OA.Parser Value
queryParametersOption = OA.option optionQueryParameters


optionQueryParameters :: OA.ReadM Value
optionQueryParameters = OA.eitherReader (\x -> maybe (Left ("Invalid JSON for query parameters: \"" <> x <> "\"")) Right (Aeson.decode (BLC.pack x)))
