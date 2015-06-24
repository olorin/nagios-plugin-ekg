{-# LANGUAGE OverloadedStrings #-}

module System.Nagios.Plugin.Ekg.Types where

import Data.Aeson
import Data.Int
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

data EkgMetric =
     -- * Nondecreasing counter, e.g., all-time number of requests.
      EkgCounter Int64
     -- * Measure of a quantity over time, e.g., number of requests per minute.
    | EkgGauge Double
    -- * Can't meaningfully turn labels into perfdata, this is a placeholder.
    | EkgLabel
    -- * Can't meaningfully turn distributions into perfdata, this is a placeholder.
    | EkgDistribution
  deriving (Eq, Show)

instance FromJSON EkgMetric where
    parseJSON (Object o) = do
        metric_type <- o .: "type"
        case metric_type of
            "c" -> EkgCounter <$> o .: "val"
            "g" -> EkgGauge <$> o .: "val"
            "l" -> return EkgLabel
            "d" -> return EkgDistribution
            x   -> fail $ "Invalid metric type " <> T.unpack x
    parseJSON _          = fail "EkgMetric must be an object"
