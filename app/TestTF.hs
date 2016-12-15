{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import System.Random.TF ( newTFGen )
import System.Random.TF.Gen ( RandomGen )
import Test.HTestU (TestResult, runBatteryToResults, c_smallCrush)
import Test.HTestU.Streaming (nextStreamFromGen)
import Test.HTestU.Wrapping (Battery)

-- Actions for running batteries on a newly generated instance of a PRNG

runCrush :: (RandomGen g, Functor f) => Battery -> f g -> f [TestResult]
runCrush crush gen = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap`
                      gen

runSmallCrushTF :: IO [TestResult]
runSmallCrushTF = runCrush c_smallCrush newTFGen

main :: IO ()
main = do
  rTF <- runSmallCrushTF
  putStrLn "TF"
  putStrLn $ show rTF

