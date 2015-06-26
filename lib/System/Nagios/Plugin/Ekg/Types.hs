{-# LANGUAGE OverloadedStrings #-}

module System.Nagios.Plugin.Ekg.Types (
    MetricTree
) where

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

data EkgMetric =
     -- | Nondecreasing counter, e.g., all-time number of requests.
      EkgCounter Int64
     -- | Measure of a quantity over time, e.g., number of requests per minute.
    | EkgGauge Double
     -- | Can't meaningfully turn labels into perfdata, this is a placeholder.
    | EkgLabel
     -- | Can't meaningfully turn distributions into perfdata, this is a placeholder.
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

-- | A node in the 'MetricTree'; a Leaf is a single metric.
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

-- | Top-level object for parsed EKG metrics. Structurally, this is an
--   n-ary tree; the leaves are the metrics themselves and the
--   non-leaf nodes are used to construct the metric labels.
newtype MetricTree = MetricTree
    { unMetricTree :: Map Text MetricNode }

instance FromJSON MetricTree where
    parseJSON (Object o) = MetricTree <$> parseJSON (Object o)
    parseJSON _          = fail "MetricTree must be an object"

instance ToPerfData MetricTree where
    toPerfData (MetricTree m) = M.foldrWithKey (renderValue Nothing) [] m

-- | Build perfdata from a single metric. The Nagios perfdata format
--   doesn't allow us to sensibly represent the EKG 'Distribution' or
--   'Label' types so we don't try.
renderMetric :: Text
             -> EkgMetric
             -> Maybe PerfDatum
renderMetric lbl (EkgCounter n) = 
    Just $ barePerfDatum lbl (IntegralValue n) Counter
renderMetric lbl (EkgGauge n) =
    Just $ barePerfDatum lbl (RealValue n) NullUnit
renderMetric _ EkgLabel = Nothing
renderMetric _ EkgDistribution = Nothing

-- | Build perfdata from a node in the metric tree. Produce a
--   'PerfDatum' from a 'Leaf', recursively walk a 'Branch' and
--   mappend the leaves.
renderValue :: Maybe Text
            -> Text
            -> MetricNode
            -> [PerfDatum]
            -> [PerfDatum]
renderValue prefix lbl (Leaf val) acc =
    case renderMetric (withPrefix prefix lbl) val of
        Nothing -> acc
        Just pd -> pd : acc
renderValue prefix lbl (Branch branch) acc = acc
    <> M.foldrWithKey (renderValue (Just $ withPrefix prefix lbl)) [] branch

-- | Construct a metric name, optionally prepended with a prefix (we
--   want a prefix for every component of the name except the first one).
withPrefix :: Maybe Text
           -> Text
           -> Text
withPrefix Nothing suff = suff
withPrefix (Just prefix) suff = prefix <> "_" <> suff
