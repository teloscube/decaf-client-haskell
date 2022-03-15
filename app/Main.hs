{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                  (Value, (.:))
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Char8  as BLC
import           Data.Char                   (toLower)
import qualified Data.HashMap.Strict         as HM
import qualified Data.Text                   as T
import           Data.Version                (showVersion)
import qualified Decaf.Client                as DC
import           Decaf.Client.Internal.Utils (applyFirst)
import           GHC.Generics                (Generic)
import qualified Options.Applicative         as OA
import           Paths_decaf_client          (version)
import           System.Exit                 (ExitCode, die, exitFailure, exitSuccess, exitWith)
import           System.IO                   (hPutStrLn, stderr)


main :: IO ()
main = exitWith =<< (cliProgram =<< OA.execParser cliProgramParserInfo)


data MicrolotCommandParameters = MicrolotCommandParameters
  { microlotCommandParametersConfigFile      :: !FilePath
  , microlotCommandParametersProfile         :: !T.Text
  , microlotCommandParametersQuery           :: !FilePath
  , microlotCommandParametersQueryParameters :: !(Maybe Value)
  }


microlotCommandParametersParser :: OA.Parser MicrolotCommandParameters
microlotCommandParametersParser = MicrolotCommandParameters
  <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG" <> OA.help "Path to generic DECAF configuration file")
  <*> OA.strOption (OA.long "profile" <> OA.metavar "PROFILE" <> OA.help "Profile name")
  <*> OA.strOption (OA.long "query" <> OA.metavar "QUERY" <> OA.help "Microlot GraphQL query")
  <*> OA.optional (queryParametersOption (OA.long "params" <> OA.metavar "PARAM" <> OA.help "Microlot GraphQL query parameters"))


queryParametersOption :: OA.Mod OA.OptionFields Value -> OA.Parser Value
queryParametersOption = OA.option optionQueryParameters


optionQueryParameters :: OA.ReadM Value
optionQueryParameters = OA.eitherReader (\x -> maybe (Left ("Invalid JSON for query parameters: \"" <> x <> "\"")) Right (Aeson.decode (BLC.pack x)))


microlotCommandProgram :: MicrolotCommandParameters -> IO ExitCode
microlotCommandProgram (MicrolotCommandParameters fileConfig profileName fileQuery params) = do --Program.runAndUploadDir config dir >> exitSuccess
  eConfig <- Aeson.eitherDecodeFileStrict fileConfig
  case eConfig of
    Left x -> hPutStrLn stderr ("Can not read the configuration file: " <> x) >> exitFailure
    Right c -> case HM.lookup profileName (decafConfigProfiles c) of
      Nothing -> hPutStrLn stderr ("Can not find the profile: " <> T.unpack profileName) >> exitFailure
      Just (DecafConfigProfile _ u k s) -> case DC.mkDecafClient u (DC.CredentialsKey (DC.KeyCredentials k s)) of
        Left err -> hPutStrLn stderr ("Can not create the DECAF client: " <> show err) >> exitFailure
        Right dc -> do
          query <- readFile fileQuery
          value <- DC.microlotResponseData . DC.responseValue <$> DC.runMicrolot (maybe (DC.mkMicrolotQuery' query) (DC.mkMicrolotQuery query) params) (DC.decafClientMicrolot dc)
          BLC.putStr (Aeson.encode (value :: Aeson.Value)) >> exitSuccess


data DecafConfigProfile = DecafConfigProfile
  { decafConfigProfileName   :: !T.Text
  , decafConfigProfileUrl    :: !T.Text
  , decafConfigProfileKey    :: !T.Text
  , decafConfigProfileSecret :: !T.Text
  } deriving (Generic)

instance Aeson.FromJSON DecafConfigProfile where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = applyFirst toLower . drop 18 }


newtype DecafConfig = DecafConfig { decafConfigProfiles :: HM.HashMap T.Text DecafConfigProfile }

instance Aeson.FromJSON DecafConfig where
  parseJSON (Aeson.Object x) = (\x -> DecafConfig $ HM.fromList (zip (fmap decafConfigProfileName x) x)) <$> (.:) x "profiles"
  parseJSON x                = fail $ "Unexpected type for DECAF configuration file contents: " <> show x


-- | Registry of commands.
data Command =
    CommandBarista
  | CommandMicrolot MicrolotCommandParameters


-- | Parsed command line arguments.
newtype CliArguments = CliArguments { cliArgumentsCommand :: Command }


-- | CLI arguments parser.
parserProgramOptions :: OA.Parser CliArguments
parserProgramOptions = CliArguments <$> OA.hsubparser
  (  OA.command "barista" (OA.info (pure CommandBarista) (OA.progDesc "Run Barista"))
  <> OA.command "microlot" (OA.info (CommandMicrolot <$> microlotCommandParametersParser) (OA.progDesc "Run Barista"))
  )


-- | Version option.
parserVersionOption :: OA.Parser (a -> a)
parserVersionOption = OA.infoOption (showVersion version) (OA.long "version" <> OA.help "Show version")


-- | CLI program information.
cliProgramParserInfo :: OA.ParserInfo CliArguments
cliProgramParserInfo = OA.info
  (OA.helper <*> parserVersionOption <*> parserProgramOptions)
  (OA.fullDesc <> OA.progDesc "DECAF Client" <> OA.header "decafcli - DECAF Command-line Client Application")


-- | CLI program.
cliProgram :: CliArguments -> IO ExitCode
cliProgram (CliArguments CommandBarista)           = hPutStrLn stderr "Not implemented yet." >> exitFailure
cliProgram (CliArguments (CommandMicrolot params)) = microlotCommandProgram params
