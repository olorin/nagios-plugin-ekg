{-# LANGUAGE OverloadedStrings #-}

module System.Nagios.Plugin.Ekg.Check (
    checkEkg,
    checkEkg'
) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Network.Wreq hiding (header)
import qualified Network.Wreq as W
import Options.Applicative
import System.Nagios.Plugin

import System.Nagios.Plugin.Ekg.Types

data PluginOpts = PluginOpts
  { optsEndpoint :: String }

pluginOptParser :: ParserInfo PluginOpts
pluginOptParser =  info (helper <*> opts)
    (   fullDesc
     <> progDesc "Nagios plugin exposing perfdata from EKG."
     <> header   "check_ekg"
    )
  where
    opts = PluginOpts <$> strOption (   long "ekg-endpoint"
                                     <> short 'e'
                                     <> metavar "EKG-ENDPOINT"
                                     <> value "http://localhost:8888"
                                     <> help "URL of the EKG endpoint."
                                    )

-- | Run the check against the EKG endpoint specified on the command
--   line. Terminate according to the
--   <https://nagios-plugins.org/doc/guidelines.html#PLUGOUTPUT plugin development guidelines> -
--   'OK' if we retrieve and successfully parse a result, otherwise
--   'CRITICAL'.
checkEkg :: NagiosPlugin ()
checkEkg = do
    opts <- liftIO $ execParser pluginOptParser
    let reqOpts = W.header "Accept" .~ ["application/json"] $ defaults
    resp <- liftIO . getWith reqOpts $ optsEndpoint opts
    case resp ^. responseStatus . statusCode of
        200 -> checkEkg' $ resp ^. responseBody
        code -> addResult Critical . T.pack $ "EKG endpoint failed with status " <> show code

-- | Version of 'checkEkg' which takes a 'ByteString' rather than
--   requesting it from an EKG endpoint.
checkEkg' :: ByteString -> NagiosPlugin ()
checkEkg' bs = case (eitherDecode' bs :: Either String MetricTree) of
    Left err -> addResult Critical $ "failed to parse EKG output: " <> T.pack err
    Right meters -> do
        addPerfData meters
        addResult OK "perfdata only"
