{-# LANGUAGE OverloadedStrings #-}

module System.Nagios.Plugin.Ekg.Check where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Network.Wreq hiding (header)
import qualified Network.Wreq as W
import Options.Applicative
import System.Nagios.Plugin

import System.Nagios.Plugin.Ekg.Types

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

checkEkg :: NagiosPlugin ()
checkEkg = do
    opts <- liftIO $ execParser pluginOptParser
    let reqOpts = W.header "Accept" .~ ["application/json"] $ defaults
    resp <- liftIO . getWith reqOpts $ optsEndpoint opts
    case resp ^. responseStatus . statusCode of
        200 -> checkEkg' $ resp ^. responseBody
        code -> addResult Critical . T.pack $ "EKG endpoint failed with status " <> show code

checkEkg' :: ByteString -> NagiosPlugin ()
checkEkg' bs = case (decode bs :: Maybe MetricTree) of
    Nothing -> addResult Critical "failed to parse EKG output"
    Just meters -> do
        addPerfData meters
        addResult OK "perfdata only"
