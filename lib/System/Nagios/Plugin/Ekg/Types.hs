{-# LANGUAGE OverloadedStrings #-}

module System.Nagios.Plugin.Ekg.Types where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Int
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
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

data MetricNode =
      Leaf EkgMetric
    | Branch (HashMap Text MetricNode)

instance FromJSON MetricNode where
    parseJSON (Object o) = do
        leaf <- isLeaf <$> parseJSON (Object o)
        if leaf
            then Leaf <$> parseJSON (Object o)
            else Branch <$> parseJSON (Object o)
      where
        isLeaf :: HashMap Text Value -> Bool
        isLeaf m = HM.member "type" m && HM.member "val" m
    parseJSON _          = fail "MetricNode must be an object"

newtype MetricTree = MetricTree
    { unMetricTree :: HashMap Text MetricNode }

instance FromJSON MetricTree where
    parseJSON (Object o) = MetricTree <$> parseJSON (Object o)
    parseJSON _          = fail "MetricTree must be an object"
