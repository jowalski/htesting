module Main where

import qualified MyTests as MT
import qualified Props as PS
import System.Exit

main :: IO ()
main =
  do
     -- add test runners into the array for each module
     good <- and <$> sequence [MT.runTests,PS.runTests]
     if good
        then exitSuccess
        else exitFailure