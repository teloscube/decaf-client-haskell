{-# LANGUAGE RecordWildCards #-}

module Decaf.Client.Cli.SubCommands.Serve.Serve where

import           Data.Default.Class            (def)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Version                  (showVersion)
import qualified Decaf.Client                  as DC
import qualified Network.Wai.Handler.Warp      as Warp
import           Paths_decaf_client            (version)
import qualified Text.Blaze                    as BM
import           Text.Blaze.Html               ((!))
import qualified Text.Blaze.Html               as BH
import qualified Text.Blaze.Html               as BH5.Attibutes
import qualified Text.Blaze.Html.Renderer.Text as BH.Text
import qualified Text.Blaze.Html.Renderer.Utf8 as BH.Utf8
import qualified Text.Blaze.Html5              as BH5
import qualified Text.Blaze.Html5.Attributes   as BH5.Attributes
import qualified Web.Scotty                    as Web


data ServeRunConfig = ServeRunConfig
  { serveRunConfigFile :: !FilePath
  , serveRunConfigPort :: !Int
  }


runServe :: ServeRunConfig -> IO ()
runServe ServeRunConfig{..} = do
  allProfiles <- DC.readDecafProfiles serveRunConfigFile
  Web.scottyOpts scottyOpts $ do
    Web.get "/" . blazeHtml $ viewIndex allProfiles
    Web.get "/about" . blazeHtml $ viewAbout
  where
    scottyOpts = def
      { Web.settings =
          Warp.setPort serveRunConfigPort
        . Warp.setBeforeMainLoop (putStrLn ("Open http://localhost:" <> show serveRunConfigPort <> " in your browser."))
        $ Web.settings def
      }


viewIndex :: [DC.DecafProfile] -> BH.Html
viewIndex dps = BH5.table $ do
  BH5.thead $ do
    BH5.tr $ do
      BH5.th "Profile Name"
      BH5.th "Profile Remote"
      BH5.th "Profile Credentials Type"
  BH5.tbody $ do
    mapM_ toRow dps
  where
    toRow dp = BH5.tr $ do
      BH5.td . BH.toHtml $ DC.decafProfileName dp
      BH5.td $ do
        let url = DC.remoteToUrl (DC.decafProfileRemote dp)
        BH5.a (BH.toHtml url)
          ! BH5.Attributes.href (BM.toValue url)
          ! BH5.Attributes.target "_blank"
      BH5.td . BH.toHtml $ case DC.decafProfileCredentials dp of
         DC.DecafCredentialsHeader _ -> ("HEADER" :: T.Text)
         DC.DecafCredentialsBasic _  -> "BASIC"
         DC.DecafCredentialsKey _    -> "KEY"
         DC.DecafCredentialsToken _  -> "TOKEN"


viewAbout :: BH.Html
viewAbout = BH5.p ("DECAF Client Application v" <> BH.toHtml (showVersion version))


blazeHtml :: BH.Html -> Web.ActionM ()
blazeHtml main = Web.html . BH.Text.renderHtml $ do
  BH5.docType
  BH5.html ! BM.dataAttribute "data-theme" "dark" $ do
    BH5.head $ do
      BH5.title "DECAF Client Application"
      BH5.link
        ! BH5.Attributes.rel "stylesheet"
        ! BH5.Attributes.href "https://unpkg.com/@picocss/pico@latest/css/pico.min.css"
    BH5.body $ do
      BH5.nav ! BH5.Attributes.class_ "container-fluid" $ do
        BH5.ul $ do
          BH5.li $ do
            BH5.a "DECAF Client Application" ! BH5.Attributes.href "/"
        BH5.ul $ do
          BH5.li $ do
            BH5.a "Homepage " ! BH5.Attributes.href "/"
          BH5.li $ do
            BH5.a "About " ! BH5.Attributes.href "/about"
      BH5.div ! BH5.Attributes.class_ "container-fluid" $ do
        main
