-- example two, learn a shift function.
-- The visible units of the RBM are partitioned into three groups.
-- One is of size N and represents the input bits.
-- The second is also of size N and represents the output bits.
-- And a third represents the amount of bits to shift, it is represented
-- decoded, and thus needs as many bits as different amounts of shifts
-- are to be applied.

import qualified Data.Vector as V
import Data.Bits(testBit, shift, (.|.), (.&.))
import RBM(randomRBM, learnFromTrainingSet, stringFromBoolVector)


booleanVectorFromInt :: Int -> Int -> V.Vector Bool
booleanVectorFromInt n word =
  V.fromList $ reverse
  [ testBit word bit
  | bit <- [0 .. pred n]]

-- learn the shift function
main =
  do let nbits = 3
         maxshift = 2
         numVisibles = maxshift+1 + 2*nbits
         mask = shift 1 nbits - 1
     let dataset =
           [ booleanVectorFromInt numVisibles $
             input
             .|. shift output nbits
             .|. (shift shDecoded $ 2*nbits)
           | input <- [0..mask]
           , sh <- [0..maxshift]
           , let shDecoded = shift 1 sh
           , let output = shift input sh .&. mask ]
     mapM_ (print . stringFromBoolVector) dataset
     r <- randomRBM numVisibles 6
     learnFromTrainingSet True 200000 r dataset

