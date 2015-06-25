{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy         as BS
import qualified Data.Text                    as T
import           Test.Hspec
import           Test.HUnit.Base

import           System.Nagios.Plugin
import           System.Nagios.Plugin.Ekg

main :: IO ()
main = hspec suite

suite :: Spec
suite =
    describe "checkEkg'" $ do
        validJson <- runIO $ BS.readFile "tests/data/ekg_sample_output.json"
        (_, validState) <- runIO . runNagiosPlugin' $ checkEkg' validJson
        let (validStatus, validOutput) = finishState validState

        let invalidJson = "{ \"iamnotarealmetric\": \"noreallyimnot\" }"
        (_, borkedState) <- runIO . runNagiosPlugin' $ checkEkg' invalidJson
        let (borkedStatus, borkedOutput) = finishState borkedState
        
        let emptyString = ""
        (_, emptyState) <- runIO . runNagiosPlugin' $ checkEkg' emptyString
        let (emptyStatus, emptyOutput) = finishState emptyState

        it "exits with OK status given valid input" $
            validStatus @?= OK

        it "outputs the correct perfdata" $
            validOutput @?= "OK: perfdata only | iterations=41734c;;;; rts_gc_bytes_allocated=0c;;;; rts_gc_bytes_copied=0c;;;; rts_gc_cpu_ms=0c;;;; rts_gc_cumulative_bytes_used=0c;;;; rts_gc_current_bytes_slop=0.0;;;; rts_gc_current_bytes_used=0.0;;;; rts_gc_gc_cpu_ms=0c;;;; rts_gc_gc_wall_ms=0c;;;; rts_gc_max_bytes_slop=0.0;;;; rts_gc_max_bytes_used=0.0;;;; rts_gc_mutator_cpu_ms=0c;;;; rts_gc_mutator_wall_ms=0c;;;; rts_gc_num_bytes_usage_samples=0c;;;; rts_gc_num_gcs=0c;;;; rts_gc_par_avg_bytes_copied=0.0;;;; rts_gc_par_max_bytes_copied=0.0;;;; rts_gc_par_tot_bytes_copied=0.0;;;; rts_gc_peak_megabytes_allocated=0.0;;;; rts_gc_wall_ms=0c;;;; ekg_server_timestamp_ms=1435212142955c;;;;"

        it "exits with CRITICAL status given invalid JSON input" $
           borkedStatus @?= Critical

        it "outputs correct failure message on invalid JSON input" $
           borkedOutput @?= "CRITICAL: failed to parse EKG output: MetricNode must be an object, not String \"noreallyimnot\""

        it "exits with CRITICAL status given empty input" $
           emptyStatus @?= Critical

        it "outputs correct failure message on empty input" $
           emptyOutput @?= "CRITICAL: failed to parse EKG output: not enough input"
