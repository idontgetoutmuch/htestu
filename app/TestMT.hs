{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import System.Random.TF.Gen ( RandomGen, next )
import System.Random.Mersenne.Pure64 ( newPureMT, PureMT, randomInt )
import Test.HTestU (TestResult, runBatteryToResults, c_smallCrush)
import Test.HTestU.Streaming (nextStreamFromGen)
import Test.HTestU.Wrapping (Battery)


runCrush :: (RandomGen g, Functor f) => Battery -> f g -> f [TestResult]
runCrush crush gen = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap`
                      gen

runSmallCrushMT :: IO [TestResult]
runSmallCrushMT = runCrush c_smallCrush newPureMT

instance RandomGen PureMT where
   next x = (fromIntegral y, n)
     where
       (y, n) = randomInt x

main :: IO ()
main = do
  rMT <- runSmallCrushMT
  putStrLn "MT"
  putStrLn $ show rMT
