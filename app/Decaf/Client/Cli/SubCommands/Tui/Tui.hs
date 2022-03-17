{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Decaf.Client.Cli.SubCommands.Tui.Tui where

import           Brick                         ((<=>))
import qualified Brick
import qualified Brick.Widgets.Border          as Border
import qualified Brick.Widgets.Table           as Table
import           Control.Monad                 (void)
import           Control.Monad.Catch           (catch)
import           Control.Monad.IO.Class        (liftIO)
import qualified Control.Monad.Parallel        as MP
import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import qualified Decaf.Client                  as DC
import qualified Graphics.Vty                  as Vty
import           Network.HTTP.Types            (statusCode)
import           System.Exit                   (ExitCode(ExitSuccess))


-- * Top-Level
-- $topLevel


runTui :: FilePath -> IO ()
runTui fp = do
  profiles <- DC.readDecafProfiles fp
  void . Brick.defaultMain app $ TuiState
     { tuiStateProfiles = profiles
     , tuiStateWorld    = TuiStateWorldProfileInfos []
     }


-- * Data Definitions
-- $dataDefinitions


data TuiState = TuiState
  { tuiStateProfiles :: ![DC.DecafProfile]
  , tuiStateWorld    :: !TuiStateWorld
  }


data TuiStateWorld =
    TuiStateWorldAbout
  | TuiStateWorldProfileInfos [TuiProfileInfo]


data TuiProfileInfo = TuiProfileInfo
  { tuiProfileInfoProfile              :: !DC.DecafProfile
  , tuiProfileInfoVersionCultproxy     :: !(Either T.Text T.Text)
  , tuiProfileInfoVersionBarista       :: !(Either T.Text T.Text)
  , tuiProfileInfoVersionEstate        :: !(Either T.Text T.Text)
  , tuiProfileInfoStatusFunction       :: !(Either T.Text T.Text)
  , tuiProfileInfoStatusBeanbag        :: !(Either T.Text T.Text)
  , tuiProfileInfoCountPortfolio       :: !(Either T.Text T.Text)
  , tuiProfileInfoCountActivePrincipal :: !(Either T.Text T.Text)
  , tuiProfileInfoCountPolicy          :: !(Either T.Text T.Text)
  }


-- * Application
-- $application


app :: Brick.App TuiState e ()
app = Brick.App
  { Brick.appDraw         = \s -> [mkFrame s]
  , Brick.appChooseCursor = Brick.neverShowCursor
  , Brick.appHandleEvent  = handleEvent
  , Brick.appStartEvent   = reloadTuiProfileInfos
  , Brick.appAttrMap      = const attrMap
  }


handleEvent :: TuiState -> Brick.BrickEvent () e -> Brick.EventM () (Brick.Next TuiState)
handleEvent s (Brick.VtyEvent ev) = case ev of
  Vty.EvKey Vty.KEsc []            -> Brick.halt s
  Vty.EvKey (Vty.KChar 'q') _      -> Brick.halt s
  Vty.EvKey (Vty.KChar 'a') _      -> Brick.continue (s { tuiStateWorld = TuiStateWorldAbout })
  Vty.EvKey (Vty.KChar 'r') _      -> reloadTuiProfileInfos s >>= Brick.continue
  Vty.EvKey _ _                    -> Brick.continue s
  Vty.EvMouseDown _n _i _but _mods -> Brick.continue s
  Vty.EvMouseUp _n _i _mBut        -> Brick.continue s
  Vty.EvResize _n _i               -> Brick.continue s
  Vty.EvPaste _bs                  -> Brick.continue s
  Vty.EvLostFocus                  -> Brick.continue s
  Vty.EvGainedFocus                -> Brick.continue s
handleEvent b _ = Brick.continue b


attrMap :: Brick.AttrMap
attrMap = Brick.attrMap Vty.defAttr
    [ ("info",    Vty.blue `Brick.on` Vty.black)
    , ("muted",   Vty.cyan  `Brick.on` Vty.black)
    , ("success", Vty.green `Brick.on` Vty.black)
    , ("warning", Vty.yellow `Brick.on` Vty.black)
    , ("failure", Vty.red `Brick.on` Vty.black)
    ]


-- * UI
-- $ui


mkFrame :: TuiState -> Brick.Widget ()
mkFrame s = Border.hBorderWithLabel (Brick.txt " DECAF Client ")
  <=> Brick.padBottom Brick.Max (case tuiStateWorld s of
    TuiStateWorldAbout           -> Brick.txt "This application is a simple TUI interface to DECAF Client Library."
    TuiStateWorldProfileInfos xs -> profilesTableWidget xs
  )
  <=> Border.hBorderWithLabel (Brick.txt " ESC: Quit - q: Quit - r: Refresh Status - a: About ")


profilesTableWidget :: [TuiProfileInfo] -> Brick.Widget ()
profilesTableWidget = Table.renderTable
  . Table.surroundingBorder True
  . Table.table
  . (profilesTableHeader :)
  . fmap mkProfilesTableRow


profilesTableHeader :: [Brick.Widget ()]
profilesTableHeader =
  [ Brick.txt "Profile"
  , Brick.txt "Remote"
  , Brick.txt "Cultproxy"
  , Brick.txt "Barista"
  , Brick.txt "Estate"
  , Brick.txt "Function"
  , Brick.txt "Beanbag"
  , Brick.txt "#Portfolio"
  , Brick.txt "#Users (Active)"
  , Brick.txt "#Policy"
  ]


mkProfilesTableRow :: TuiProfileInfo -> [Brick.Widget ()]
mkProfilesTableRow TuiProfileInfo{..} =
  [ Brick.txt (DC.decafProfileName tuiProfileInfoProfile)
  , Brick.txt (DC.remoteToUrl . DC.decafProfileRemote $ tuiProfileInfoProfile)
  , widget tuiProfileInfoVersionCultproxy
  , widget tuiProfileInfoVersionBarista
  , widget tuiProfileInfoVersionEstate
  , widget tuiProfileInfoStatusFunction
  , widget tuiProfileInfoStatusBeanbag
  , widget tuiProfileInfoCountPortfolio
  , widget tuiProfileInfoCountActivePrincipal
  , widget tuiProfileInfoCountPolicy
  ]
  where
    widget (Left err) = Brick.withAttr "failure" $ Brick.txt err
    widget (Right sv) = Brick.withAttr "success" $ Brick.txt sv


-- * Event Logic
-- $eventLogic


reloadTuiProfileInfos :: TuiState -> Brick.EventM () TuiState
reloadTuiProfileInfos state@TuiState{..} = do
  infos <- liftIO $ getTuiProfileInfos tuiStateProfiles
  pure $ state
    { tuiStateWorld = TuiStateWorldProfileInfos infos
    }


getTuiProfileInfos :: [DC.DecafProfile] -> IO [TuiProfileInfo]
getTuiProfileInfos = MP.mapM getTuiProfileInfo


getTuiProfileInfo :: DC.DecafProfile -> IO TuiProfileInfo
getTuiProfileInfo profile = do
  let client = DC.mkDecafClientFromProfile profile
  cultproxy <- getCultproxyVersion client `catch` handleError
  barista <- getBaristaVersion client `catch` handleError
  estate <- getEstateVersion client `catch` handleError
  function <- getFunctionStatus client `catch` handleError
  beanbag <- getBeanbagStatus client `catch` handleError
  portfolioCount <- getPortfolioCount client `catch` handleError
  activePrincipalCount <- getActivePrincipalCount client `catch` handleError
  policyCount <- getPolicyCount client `catch` handleError
  pure $ TuiProfileInfo
    { tuiProfileInfoProfile              = profile
    , tuiProfileInfoVersionCultproxy     = cultproxy
    , tuiProfileInfoVersionBarista       = barista
    , tuiProfileInfoVersionEstate        = estate
    , tuiProfileInfoStatusFunction       = function
    , tuiProfileInfoStatusBeanbag        = beanbag
    , tuiProfileInfoCountPortfolio       = portfolioCount
    , tuiProfileInfoCountActivePrincipal = activePrincipalCount
    , tuiProfileInfoCountPolicy          = policyCount
    }
  where
    portfolioCountQuery = "query {\n  portfolio_aggregate {\n    aggregate {\n      count\n    }\n  }\n}"
    handleError = \(_ :: DC.DecafClientException) -> pure $ Left "ERR"  -- (T.pack (show s))


getCultproxyVersion :: DC.DecafClient -> IO (Either T.Text T.Text)
getCultproxyVersion client = do
  value <- DC.runDecafClientJson (DC.path "_cultproxy") client
  case ACD.parseEither (ACD.at ["version"] ACD.auto) value of
    Left err -> pure $ Left (T.pack err)
    Right sv -> pure $ Right sv


getBaristaVersion :: DC.DecafClient -> IO (Either T.Text T.Text)
getBaristaVersion client = do
  value <- DC.runDecafBaristaJson (DC.path "version") client
  case ACD.parseEither (ACD.at ["version"] ACD.auto) value of
    Left err -> pure $ Left (T.pack err)
    Right sv -> pure $ Right sv


getEstateVersion :: DC.DecafClient -> IO (Either T.Text T.Text)
getEstateVersion client = do
  value <- DC.runDecafEstateJson (DC.path "version") client
  case ACD.parseEither (ACD.at ["value"] ACD.auto) value of
    Left err -> pure $ Left (T.pack err)
    Right sv -> pure $ Right sv


getFunctionStatus :: DC.DecafClient -> IO (Either T.Text T.Text)
getFunctionStatus = fmap Right . DC.runDecafFunctionJson (DC.path "echo" . DC.post . DC.jsonPayload ("working" :: T.Text))


getBeanbagStatus :: DC.DecafClient -> IO (Either T.Text T.Text)
getBeanbagStatus = fmap (Right . TL.toStrict . TLE.decodeUtf8) . DC.runDecafBeanbag (DC.path "commons/_status")


getPortfolioCount :: DC.DecafClient -> IO (Either T.Text T.Text)
getPortfolioCount client = do
  value <- DC.runDecafMicrolotNoVars query client
  case ACD.parseEither (ACD.at ["portfolio_aggregate", "aggregate", "count"] ACD.auto) value of
    Left err -> pure $ Left (T.pack err)
    Right sv -> pure $ Right (T.pack (show (sv :: Int)))
  where
    query = "query {\n  portfolio_aggregate {\n    aggregate {\n      count\n    }\n  }\n}"


getActivePrincipalCount :: DC.DecafClient -> IO (Either T.Text T.Text)
getActivePrincipalCount client = do
  value <- DC.runDecafMicrolotNoVars query client
  case ACD.parseEither (ACD.at ["principal_aggregate", "aggregate", "count"] ACD.auto) value of
    Left err -> pure $ Left (T.pack err)
    Right sv -> pure $ Right (T.pack (show (sv :: Int)))
  where
    query = "query {\n  principal_aggregate(where: {is_active: {_eq: true}}) {\n    aggregate {\n      count\n    }\n  }\n}"


getPolicyCount :: DC.DecafClient -> IO (Either T.Text T.Text)
getPolicyCount client = do
  response <- DC.performDecafRequest (DC.buildDecafRequest (DC.noCheckResponse . DC.graphqlNoVars query . DC.apiModulePdms) client)
  case statusCode (DC.decafResponseStatus response) of
    200 -> case ACD.eitherDecode (ACD.at ["data", "policy_aggregate", "aggregate", "count"] ACD.auto) (DC.decafResponseBody response) of
       Left err -> pure $ Left (T.pack err)
       Right sv -> pure $ Right (T.pack (show (sv :: Int)))
    404 -> pure (Right " ")
    _   -> pure (Left "Err")
  where
    query = "query {\n  policy_aggregate(where: {closed_on: {_is_null: true}}) {\n    aggregate {\n      count\n    }\n  }\n}"
