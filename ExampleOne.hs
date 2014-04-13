import qualified Data.Matrix as MAT
import RBM (RBM(RBM), stringToBoolVector, randomRBM, learnFromTrainingSet)

-- example 1, learn some random input by one hidden bit

input =
  map stringToBoolVector
  [ "111000"
  , "000111"
  , "111000"
  , "000111"
  , "111000"
  , "000101"
  , "101010"
  , "010111" ]

rbm1 =
  RBM $ MAT.fromLists $
    [ [ 0,    0,    0 ]
    , [ 0.2,  1.5, -1.4 ]
    , [ -0.3, -1.2,  1.0 ] ]

rbm2 =
  RBM $ MAT.fromLists $
    [ [ 0,    0,    0 ]
    , [ 0,  0.1,  -0.03 ]
    , [ 0,  0.2,  -0.04 ] ]

main =
  do r <- randomRBM 6 1
     -- let r = rbm1
     putStrLn $ "starting with\n" ++ show r
     learnFromTrainingSet True 5000 r (take 8 input)
     -- learnFromTrainingSetByRandomSearch 100 r (take 2 input2)
