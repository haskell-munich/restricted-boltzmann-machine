{-# LANGUAGE BangPatterns #-}
-- Restricted Boltzmann Machine
module RBM(RBM(RBM), 
           randomRBM, hiddenFromVisible, hiddenFromVisibleD, 
           visibleFromHidden, visibleFromHiddenD, sample,
           visibleFromBooleanVector, hiddenFromBooleanVector,
           matApplyUn, matApplyBin, energy,
           learn, learnFromTrainingSet,
           learnByRandomSearch, learnFromTrainingSetByRandomSearch,
           stringToBoolVector, stringFromBoolVector
           ) where

import qualified Data.List as L
import qualified Data.Matrix as MAT
import qualified Data.Vector as V
import qualified System.Random as R
import Control.Monad(replicateM, liftM)
import Data.Ord(comparing)
import Control.DeepSeq(deepseq)

data RBM =
  RBM { rbmWeights :: MAT.Matrix Double }

instance Show RBM where
  show (RBM w) = "RBM\n" ++ show w

randomRBM :: Int -> Int -> IO RBM
randomRBM numVisible numHidden =
  do lists <-
       replicateM (succ numVisible) $
       replicateM (succ numHidden) $
       R.getStdRandom (R.randomR (-1, 1))
     let mat = MAT.fromLists lists
     -- putStrLn $ MAT.prettyMatrix mat
     return $ RBM $
       MAT.setElem 0 (1,1) mat

booleanToDouble True  = 1.0
booleanToDouble False = 0.0

randomVectorOfLength :: Int -> IO (V.Vector Double)
randomVectorOfLength n =
  liftM (V.fromList) $
  replicateM n $
  R.getStdRandom (R.randomR (0, 1.0))

visibleFromBooleanVector = V.map booleanToDouble
hiddenFromBooleanVector = V.map booleanToDouble

hiddenFromVisible :: RBM -> V.Vector Bool -> IO (V.Vector Bool)
hiddenFromVisible r visible =
  sample $
  hiddenFromVisibleD r $
  visibleFromBooleanVector visible

hiddenFromVisibleTr:: RBM -> V.Vector Bool -> IO (V.Vector Bool)
hiddenFromVisibleTr r visible =
  do let a = hiddenFromVisibleD r $
             visibleFromBooleanVector visible
     print a
     sample a

hiddenFromVisibleD :: RBM -> V.Vector Double -> V.Vector Double
hiddenFromVisibleD r v =
  let m = MAT.multStd (MAT.rowVector v) (rbmWeights r)
  in V.map logistic $ V.drop 1 $ MAT.getRow 1 m

logistic x = 1 / (1 + exp (negate (1 * x)))

sample :: V.Vector Double -> IO (V.Vector Bool)
sample a =
  do d <- randomVectorOfLength $ V.length a
     return $ V.zipWith (>) a d

visibleFromHidden :: RBM -> V.Vector Bool -> IO (V.Vector Bool)
visibleFromHidden r hidden =
  sample $
  visibleFromHiddenD r $
  hiddenFromBooleanVector hidden

visibleFromHiddenD :: RBM -> V.Vector Double -> V.Vector Double
visibleFromHiddenD r h =
  let m = MAT.multStd (rbmWeights r) (MAT.colVector h)
  in V.map logistic $ V.drop 1 $ MAT.getCol 1 m


matApplyUn :: (a -> a) -> MAT.Matrix a -> MAT.Matrix a
matApplyUn f !m =
  MAT.fromLists $
  [ [ f $ m MAT.! (i,j)
    | j <- [1..MAT.ncols m]]
  | i <- [1..MAT.nrows m]]

matApplyBin :: (a -> a -> a) -> MAT.Matrix a -> MAT.Matrix a
               -> MAT.Matrix a
matApplyBin _ m n
  | MAT.ncols m /= MAT.ncols n
    || MAT.nrows m /= MAT.nrows n
    = error "matrixes don't match"
matApplyBin f !m !n =
  MAT.fromLists
  [ [ f (m MAT.! (i,j)) (n MAT.! (i,j))
    | j <- [1..MAT.ncols m]]
  | i <- [1..MAT.nrows m]]

energy :: RBM -> V.Vector Bool -> V.Vector Bool -> Double
energy r visible hidden =
  sum
  [ negate $ sum [ a i * v i | i <- [1 .. V.length visible]]
  , negate $ sum [ b j * h j | j <- [1 .. V.length hidden]]
  , negate $ sum [ v i * h j * w i j
                 | i <- [1 .. V.length visible]
                 , j <- [1 .. V.length hidden]]]
  where m = rbmWeights r
        a i = MAT.getElem 1 (succ i) m
        b j = MAT.getElem (succ j) 1 m
        w i j = MAT.getElem (succ j) (succ i) m
        v i = booleanToDouble $ visible V.! (pred i)
        h j = booleanToDouble $ hidden V.! (pred j)


coactivityMatrix :: V.Vector Double -> V.Vector Double
                 -> MAT.Matrix Double
coactivityMatrix !v !h =
  MAT.fromLists
  [ [ vi * hj
    | hj <- V.toList h ]
  | vi <- V.toList v ]

learn :: Bool -> RBM -> V.Vector Bool -> IO RBM
learn verbose !r v =
  do h <- hiddenFromVisible r (V.cons True v)
     -- putStrLn "hidden:"; print h
     let energy1 = energy r v h
         visibles = V.cons 1 $ V.map booleanToDouble v
         hiddens = V.cons 1 $ V.map booleanToDouble h
         positive = coactivityMatrix visibles hiddens
     -- putStrLn "positive:"; print positive
     let dv = V.cons 1 $ visibleFromHiddenD r hiddens
     verbr <- R.getStdRandom (R.randomR (0, 1.0::Double))
     let doPrint = verbose || verbr < 0.01
     if doPrint
        then do putStrLn "v:"; print v
                putStrLn "dv:"
                print $ V.map ((/100) . fromIntegral . round . (*100)) dv
                print("diff:",
                      V.sum $ V.map (**2) $ V.zipWith (-) visibles dv)
                let boolDiff x y = if x == y then 0 else 1::Int
                print("diff (>0.5):",
                      V.sum $
                      V.zipWith boolDiff (V.drop 1 $ V.map (>0.5) dv) v)
       else return ()
     let dh = V.cons 1 $ hiddenFromVisibleD r dv
     -- putStrLn "dh:"; print dh
     let negative = coactivityMatrix dv dh
     -- putStrLn "negative:"; print negative
     let r2 = RBM $
              matApplyBin (+) (rbmWeights r) $
              matApplyUn (*0.01) $
              matApplyBin (-) positive negative
     let energy2 = energy r2 v h
     if doPrint
       then print("energy change:", energy2 - energy1)
       else return ()
     rbmWeights r2 `deepseq` return r2

learnFromTrainingSet :: Bool -> Int -> RBM -> [V.Vector Bool] -> IO RBM
learnFromTrainingSet verbose n r is = repea n r is
  where repea 0 r _ = return r
        repea n r [] = repea n r is
        repea n r1 (i:is) =
          do r2 <- learn verbose r1 i
             -- print $ rbmWeights r2
             repea (pred n) r2 is
-- todo: use 'cycle' and 'iterateM'(??)




-- make some random modifications to the matrix and see
-- if there is any that decreases the energy for the given
-- data instance. if there is any, choose the best.
-- return False if there isn't.
learnByRandomSearch :: RBM -> V.Vector Bool -> IO (RBM, Bool)
learnByRandomSearch r v =
  do putStrLn $ "input: " ++ show v
     h <- hiddenFromVisibleTr r (V.cons True v)
     putStrLn $ "  hidden: " ++ show h
     let energy1 = energy r v h
     changes <- replicateM 7 $
       do r2 <- applyRandomChange r
          let energy2 = energy r2 v h
          return (energy2 - energy1, r2)
     let (diff, rbest) = L.minimumBy (comparing fst) changes
     if diff < 0
       then do putStrLn $
                 "  improved energy by " ++ show diff
                 ++ " (from " ++ show energy1
                 ++ " to " ++ show (energy1 + diff) ++ ")"
               return (rbest, True)
       else return (r, False)

applyRandomChange :: RBM -> IO RBM
applyRandomChange (RBM weights) =
  do r <- R.getStdRandom (R.randomR (1, MAT.nrows weights))
     c <- R.getStdRandom (R.randomR (1, MAT.ncols weights))
     d <- R.getStdRandom (R.randomR (0, 0.1))
     let n = d + MAT.getElem r c weights
     return $
       RBM $ MAT.setElem n (r, c) weights

learnFromTrainingSetByRandomSearch
  :: Int -> RBM -> [V.Vector Bool] -> IO RBM
learnFromTrainingSetByRandomSearch n r0 is = repea n r0 is
  where repea 0 r _ = return r
        repea n r [] = repea n r is
        repea n r1 (i:is) =
          do (r2, improved) <- learnByRandomSearch r1 i
             -- print $ rbmWeights r2
             repea (pred n) r2 is
-- todo: use 'cycle' and 'iterateM'(??)


stringToBoolVector = V.fromList . map (=='1')

stringFromBoolVector :: V.Vector Bool -> String
stringFromBoolVector = V.toList . V.map toDigit
  where toDigit True = '1'
        toDigit False = '0'


