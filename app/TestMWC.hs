{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import System.Random.TF.Gen ( RandomGen, next )
import qualified System.Random.MWC as MWC
import Test.HTestU (TestResult, runBatteryToResults, c_smallCrush)
import Test.HTestU.Streaming (nextStreamFromGen)
import Test.HTestU.Wrapping (Battery)

import Control.Monad.Primitive
import System.IO.Unsafe


runCrush :: (RandomGen g, Functor f) => Battery -> f g -> f [TestResult]
runCrush crush gen = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap`
                      gen

data MWCRNG = MWCRNG (MWC.Gen (PrimState IO))

instance RandomGen MWCRNG where
  next g@(MWCRNG gen) = unsafeDupablePerformIO $
    do v <- MWC.uniform gen
       return (v, g)

runSmallCrushMWC :: IO [TestResult]
runSmallCrushMWC = do
  s <- MWC.create
  runCrush c_smallCrush (return $ MWCRNG s)

main :: IO ()
main = do
  rMWC <- runSmallCrushMWC
  putStrLn "MWC"
  putStrLn $ show rMWC

