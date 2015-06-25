module Main where

import System.Nagios.Plugin (runNagiosPlugin)
import System.Nagios.Plugin.Ekg (checkEkg)

main :: IO ()
main = runNagiosPlugin checkEkg
