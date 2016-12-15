module Test.HTestU.Streaming
 ( RandomStream
 , nextStreamFromGen
 , splitNextStreamFromGen
 , leftSplitStreamFromGen
 , rightSplitStreamFromGen
 ) where

import System.Random.TF.Gen ( RandomGen, next, split )
import Data.Tuple (swap)
import GHC.Word ( Word32 )

type RandomNumber = Word32

-- | Synonym for an infinite stream of random numbers
type RandomStream = [RandomNumber]

-- | Endomorphic wrapper for 'next' function
wrappedNext :: RandomGen g => (RandomNumber, g) -> (RandomNumber, g)
wrappedNext (_, newGen) = next newGen

-- | Generates a stream from a PRNG by using a repeated 'next' application
nextStreamFromGen :: RandomGen g => g -> RandomStream
nextStreamFromGen gen = map fst $ iterate wrappedNext firstStreamElement
  where firstStreamElement = next gen

-- | Generates a stream from a PRNG by using 'split' and a provided choose function
splitStreamFromGen :: RandomGen g => ((g, g) -> (g, g)) -> g -> RandomStream
splitStreamFromGen choose gen = let (old, new) = choose $ split gen
                                    (value, _) = next old
                                     in value : splitStreamFromGen choose new

-- | Does nothing to the provided pair of generators
goLeft :: (g, g) -> (g, g)
goLeft = id

-- | Swaps the generators so they change roles
goRight :: (g, g) -> (g, g)
goRight = swap

-- | Generates a stream from a PRNG by using 'split' and a left generator to
-- produce a next random number
leftSplitStreamFromGen :: RandomGen g => g -> RandomStream
leftSplitStreamFromGen = splitStreamFromGen goLeft

-- | Generates a stream from a PRNG by using 'split' and a right generator to
-- produce a next random number
rightSplitStreamFromGen :: RandomGen g => g -> RandomStream
rightSplitStreamFromGen = splitStreamFromGen goRight

-- | Splits a provided generator and intertwines the generated random streams
splitNextStreamFromGen :: RandomGen g => g -> RandomStream
splitNextStreamFromGen gen = intertwineStreams (nextStreamFromGen leftGen) $ nextStreamFromGen rightGen
  where (leftGen, rightGen) = split gen

-- | Returns a new stream of elements in which elements from two given streams
-- alternate
intertwineStreams :: [a] -> [a] -> [a]
intertwineStreams [] ys = ys
intertwineStreams xs [] = xs
intertwineStreams (x : xs) (y : ys) = x : y : intertwineStreams xs ys
