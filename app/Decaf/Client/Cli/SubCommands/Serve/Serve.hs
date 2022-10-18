{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Decaf.Client.Cli.SubCommands.Serve.Serve where

import           Data.Default.Class            (def)
import qualified Data.List                     as List
import           Data.String.Interpolate       (i)
import qualified Data.Text                     as T
import           Data.Version                  (showVersion)
import qualified Decaf.Client                  as DC
import qualified Network.HTTP.Types.Status     as Http
import qualified Network.Wai.Handler.Warp      as Warp
import           Paths_decaf_client            (version)
import qualified Text.Blaze                    as BM
import qualified Text.Blaze.Html               as BH
import           Text.Blaze.Html               ((!))
import qualified Text.Blaze.Html.Renderer.Text as BH.Text
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
    Web.get "/profiles/:profileName/graphiql/microlot" $ do
      profileName <- Web.param "profileName"
      let mProfile = List.find (\p -> DC.decafProfileName p == profileName) allProfiles
      case mProfile of
        Nothing -> Web.status Http.status404
        Just p  -> graphiqlMicrolot p
    Web.get "/profiles/:profileName/graphiql/module-pdms" $ do
      profileName <- Web.param "profileName"
      let mProfile = List.find (\p -> DC.decafProfileName p == profileName) allProfiles
      case mProfile of
        Nothing -> Web.status Http.status404
        Just p  -> graphiqlModulePdms p
    Web.get "/:profileName" $ do
      profileName <- Web.param "profileName"
      let mProfile = List.find (\p -> DC.decafProfileName p == profileName) allProfiles
      case mProfile of
        Nothing -> Web.status Http.status404
        Just p  -> blazeHtml $ viewProfileDetails p
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
      BH5.td $ do
         BH5.a (BH.toHtml (DC.decafProfileName dp))
          ! BH5.Attributes.href (BM.toValue (DC.decafProfileName dp))
      BH5.td $ do
        let url = DC.remoteToUrl (DC.decafProfileRemote dp)
        BH5.a (BH.toHtml url)
          ! BH5.Attributes.href (BM.toValue url)
          ! BH5.Attributes.target "_blank"
      BH5.td $ viewCredentials (DC.decafProfileCredentials dp)


credentialsToHeader :: DC.DecafCredentials -> T.Text
credentialsToHeader cred = case cred of
  DC.DecafCredentialsHeader header    -> "'Authorization': '" <> header <> "'"
  DC.DecafCredentialsBasic (DC.DecafBasicCredentials username password) -> "'Authorization': 'BASIC ' + btoa('" <> username <> ":" <> password <> "')"
  DC.DecafCredentialsKey (DC.DecafKeyCredentials key secret) -> "'Authorization': 'KEY " <> key <> ":" <> secret <> "'"
  DC.DecafCredentialsToken token -> "'Authorization': 'TOKEN " <> token <> "'"


viewCredentials :: DC.DecafCredentials -> BH.Html
viewCredentials cred = BH.toHtml $ case cred of
  DC.DecafCredentialsHeader _ -> ("HEADER" :: T.Text)
  DC.DecafCredentialsBasic _  -> "BASIC"
  DC.DecafCredentialsKey _    -> "KEY"
  DC.DecafCredentialsToken _  -> "TOKEN"


viewProfileDetails :: DC.DecafProfile -> BH.Html
viewProfileDetails DC.DecafProfile{..} = BH5.ul $ do
  BH5.li $ "Profile Name: " <> BH.toHtml decafProfileName
  BH5.li $ "Profile Remote: " <>
    BH5.a (BH.toHtml url)
      ! BH5.Attributes.href (BM.toValue url)
      ! BH5.Attributes.target "_blank"
  BH5.li $ "Profile Credentials Type: " <> viewCredentials decafProfileCredentials
  BH5.li $ "Barista API URL: " <> BH5.a (BH.toHtml (url <> "/api"))
      ! BH5.Attributes.href (BM.toValue (url <> "/api"))
      ! BH5.Attributes.target "_blank"
  BH5.li $ "Microlot GraphQL URL: " <> BH5.a (BH.toHtml (url <> "/apis/microlot/v1/graphql"))
      ! BH5.Attributes.href (BM.toValue (url <> "/apis/microlot/v1/graphql"))
      ! BH5.Attributes.target "_blank"
  BH5.li $ "Microlot GraphiQL: " <> BH5.a (BH.toHtml ("/profiles/" <> decafProfileName <> "/graphiql/microlot"))
      ! BH5.Attributes.href (BM.toValue ("/profiles/" <> decafProfileName <> "/graphiql/microlot"))
      ! BH5.Attributes.target "_blank"
  BH5.li $ "Microlot Console: " <> BH5.a (BH.toHtml (url <> "/apis/microlot/console"))
      ! BH5.Attributes.href (BM.toValue (url <> "/apis/microlot/console"))
      ! BH5.Attributes.target "_blank"
  BH5.li $ "Module PDMS GraphQL URL: " <> BH5.a (BH.toHtml (url <> "/apis/modules/pdms/v1/graphql"))
      ! BH5.Attributes.href (BM.toValue (url <> "/apis/modules/pdms/v1/graphql"))
      ! BH5.Attributes.target "_blank"
  BH5.li $ "Module PDMS GraphiQL: " <> BH5.a (BH.toHtml ("/profiles/" <> decafProfileName <> "/graphiql/modules/pdms"))
      ! BH5.Attributes.href (BM.toValue ("/profiles/" <> decafProfileName <> "/graphiql/modules/pdms"))
      ! BH5.Attributes.target "_blank"
  BH5.li $ "Module PDMS Console: " <> BH5.a (BH.toHtml (url <> "/apis/modules/pdms/console"))
      ! BH5.Attributes.href (BM.toValue (url <> "/apis/modules/pdms/console"))
      ! BH5.Attributes.target "_blank"
  where
    url = DC.remoteToUrl decafProfileRemote


graphiqlMicrolot :: DC.DecafProfile -> Web.ActionM ()
graphiqlMicrolot p = graphiqlHtml (url <> "/apis/microlot/v1/graphql") (credentialsToHeader (DC.decafProfileCredentials p))
  where
    url = DC.remoteToUrl (DC.decafProfileRemote p)


graphiqlModulePdms :: DC.DecafProfile -> Web.ActionM ()
graphiqlModulePdms p = graphiqlHtml (url <> "/apis/modules/pdms/v1/graphql") (credentialsToHeader (DC.decafProfileCredentials p))
  where
    url = DC.remoteToUrl (DC.decafProfileRemote p)


viewAbout :: BH.Html
viewAbout = BH5.p ("DECAF Client Application v" <> BH.toHtml (showVersion version))


-------------
-- HELPERS --
-------------

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


graphiqlHtml :: T.Text -> T.Text ->  Web.ActionM ()
graphiqlHtml graphqlUrl authHeader = Web.html . BH.Text.renderHtml $ do
  BH5.docType
  BH5.html $ do
    BH5.head $ do
      BH5.style $ BH5.toHtml ("body { height: 100%; margin: 0; width: 100%; overflow: hidden; } #graphiql { height: 100vh; }" :: T.Text)
      BH5.script ! BH5.Attributes.src "https://unpkg.com/react@16/umd/react.development.js" $ pure ()
      BH5.script ! BH5.Attributes.src "https://unpkg.com/react-dom@16/umd/react-dom.development.js" $ pure ()
      BH5.link
        ! BH5.Attributes.rel "stylesheet"
        ! BH5.Attributes.href "https://unpkg.com/graphiql/graphiql.min.css"
    BH5.body $ do
      BH5.div ! BH5.Attributes.id "graphiql" $ BH5.html "Loading..."
      BH5.script
        ! BH5.Attributes.src "https://unpkg.com/graphiql/graphiql.min.js"
        ! BH5.Attributes.type_ "application/javascript" $ pure ()
      -- <script src="/renderExample.js" type="application/javascript"></script>
      BH5.script . BH5.toHtml $ generateScriptBody graphqlUrl authHeader


generateScriptBody :: T.Text -> T.Text -> T.Text
generateScriptBody graphqlUrl authHeader = [i| function graphQLFetcher(graphQLParams) {
        return fetch(
          '#{graphqlUrl}',
          {
            method: 'post',
            headers: {
              Accept: 'application/json',
              'Content-Type': 'application/json',
              #{authHeader},
            },
            body: JSON.stringify(graphQLParams),
            credentials: 'omit',
          },
        ).then(function (response) {
          return response.json().catch(function () {
            return response.text();
          });
        });
      }

      ReactDOM.render(
        React.createElement(GraphiQL, {
          fetcher: graphQLFetcher,
          defaultVariableEditorOpen: true,
        }),
        document.getElementById('graphiql'),
      );|] :: T.Text
