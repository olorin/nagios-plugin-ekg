{-# LANGUAGE OverloadedStrings #-}

module System.Nagios.Plugin.Ekg.Types where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Int
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import System.Nagios.Plugin

data PluginOpts = PluginOpts
  { optsEndpoint :: String }

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
    | Branch (Map Text MetricNode)

instance FromJSON MetricNode where
    parseJSON (Object o) = do
        leaf <- isLeaf <$> parseJSON (Object o)
        if leaf
            then Leaf <$> parseJSON (Object o)
            else Branch <$> parseJSON (Object o)
      where
        -- Educated guess as to whether this object is a leaf. It'll
        -- definitely have "type"; it'll have "val" if it's a counter,
        -- gauge or label and it'll have "variance" and "mean" if it's
        -- a distribution.
        --
        -- My kingdom for a schema.
        isLeaf :: HashMap Text Value -> Bool
        isLeaf m = HM.member "type" m &&
            (HM.member "val" m ||
                (HM.member "variance" m && HM.member "mean" m)
            )
    parseJSON x          = fail $ "MetricNode must be an object, not " <> show x

newtype MetricTree = MetricTree
    { unMetricTree :: Map Text MetricNode }

instance FromJSON MetricTree where
    parseJSON (Object o) = MetricTree <$> parseJSON (Object o)
    parseJSON _          = fail "MetricTree must be an object"

instance ToPerfData MetricTree where
    toPerfData (MetricTree m) = M.foldrWithKey (renderValue Nothing) [] m

renderMetric :: Text
             -> EkgMetric
             -> [PerfDatum]
renderMetric lbl (EkgCounter n) = 
    [barePerfDatum lbl (IntegralValue n) Counter]
renderMetric lbl (EkgGauge n) =
    [barePerfDatum lbl (RealValue n) NullUnit]
renderMetric _ EkgLabel = []
renderMetric _ EkgDistribution = []

renderValue :: Maybe Text
            -> Text
            -> MetricNode
            -> [PerfDatum]
            -> [PerfDatum]
renderValue prefix lbl (Leaf val) acc = acc
    <> (renderMetric (withPrefix prefix lbl) val)
renderValue prefix lbl (Branch branch) acc = acc
    <> M.foldrWithKey (renderValue (Just $ withPrefix prefix lbl)) [] branch

withPrefix :: Maybe Text
           -> Text
           -> Text
withPrefix Nothing suff = suff
withPrefix (Just prefix) suff = prefix <> "_" <> suff


