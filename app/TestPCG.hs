{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import System.Random.TF.Gen ( RandomGen, next )
import System.Random.PCG.Pure ( FrozenGen, save, create, next' )
import Test.HTestU (TestResult, runBatteryToResults, c_smallCrush)
import Test.HTestU.Streaming (nextStreamFromGen)
import Test.HTestU.Wrapping (Battery)

import Control.Monad.Primitive
import Control.Monad ( join )


runCrush :: (RandomGen g, Functor f) => Battery -> f g -> f [TestResult]
runCrush crush gen = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap`
                      gen

runSmallCrushPCG :: IO [TestResult]
runSmallCrushPCG = runCrush c_smallCrush (join foo)
  where
    foo :: PrimMonad m => m (m FrozenGen)
    foo = fmap save create

instance RandomGen FrozenGen
  where
    next = next'

main :: IO ()
main = do
  rPCG <- runSmallCrushPCG
  putStrLn "PCG"
  putStrLn $ show rPCG
