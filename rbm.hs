{-# LANGUAGE BangPatterns #-}
import qualified Data.List as L
import qualified Data.Matrix as MAT
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified System.Random as R
import Control.Monad(replicateM, liftM, replicateM)
import Data.Ord(comparing)
import Data.Bits((.|.), (.&.), shift, testBit)
import qualified Data.Char as C
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
     if verbose
        then do putStrLn "v:"; print v
                putStrLn "dv:"; print dv
       else return ()
     print("diff:",
           V.sum $ V.map (**2) $ V.zipWith (-) visibles dv)
     let dh = V.cons 1 $ hiddenFromVisibleD r dv
     -- putStrLn "dh:"; print dh
     let negative = coactivityMatrix dv dh
     -- putStrLn "negative:"; print negative
     let r2 = RBM $
              matApplyBin (+) (rbmWeights r) $
              matApplyUn (*0.03) $
              matApplyBin (-) positive negative
     let energy2 = energy r2 v h
     print("energy change:", energy2 - energy1)
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




-- example 1, learn some random input by one hidden bit

stringToBoolVector = V.fromList . map (=='1')

inputNotFunction =
  map stringToBoolVector
  [ "01"
  , "10" ]

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

example1 =
  do r <- randomRBM 6 1
     -- let r = rbm1
     putStrLn $ "starting with\n" ++ show r
     learnFromTrainingSet True 5000 r (take 8 input)
     -- learnFromTrainingSetByRandomSearch 100 r (take 2 input2)

booleanVectorFromInt :: Int -> Int -> V.Vector Bool
booleanVectorFromInt n word =
  V.fromList $ reverse
  [ testBit word bit
  | bit <- [0 .. pred n]]

stringFromBooleanVector :: V.Vector Bool -> String
stringFromBooleanVector = V.toList . V.map toDigit
  where toDigit True = '1'
        toDigit False = '0'

-- learn the shift function
example2 =
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
     mapM_ (print . stringFromBooleanVector) dataset
     r <- randomRBM numVisibles 6
     learnFromTrainingSet True 20000 r dataset



data Features
  = First Char
  | Other Char
  | Last3 Char
  | Last2 Char
  | Last Char
    deriving (Eq, Ord, Show)

extractFeatures :: [String] -> [(Int, Features)]
extractFeatures words =
  [ (pos, feature)
  | (word, pos) <- zip words [1..]
  , feature <- features word]

features :: String -> [Features]
features [] = []
features word =
  [ First $ head word ]
  ++ (if l > 1
      then [Last $ word !! (l-1)]
      else [])
  ++ (if l > 2
      then [Last2 $ word !! (l-2)]
      else [])
  ++ (if l > 3
      then [Last3 $ word !! (l-3)]
      else [])
  ++ (map Other $ L.nub $ drop 1 $ dropAtEnd 3 $ word)
  where l = length word

dropAtEnd n = reverse . drop n . reverse

xwords = recur [] []
  where recur res word (c:cs)
          | C.isSpace c =
            if not $ null word
            then recur (reverse word : res) [] cs
            else recur res [] cs
          | C.isAlpha c || c == '\'' =
              recur res (c:word) cs
          | otherwise =
              recur (reverse word : res) [c] cs
        recur res [] [] = reverse $ res
        recur res word [] = reverse $ reverse word : res

-- http://www.gutenberg.org/cache/epub/3160/pg3160.txt
exampleLearnTrigrams =
  do odyssey <- readFile "pg3160.txt"
     let dataset =
           [ (someWords, extractFeatures someWords)
           | ws <- L.tails $ take 1000 $ drop 1000 $
                   xwords odyssey
           , let someWords = take 3 ws ]
     let allFeatures = S.unions $ map (S.fromList . snd) dataset
     print ("number of different features:", S.size $ allFeatures)
     let featureIds = M.fromList $ zip (S.toList allFeatures) [1..]
         (_, maxFeatureId) = M.findMax featureIds
     let encodedDataset =
           [ (theWords,
              let featureSet = S.fromList $
                               map (featureIds M.!) features
              in V.generate maxFeatureId (flip S.member featureSet))
           | (theWords, features) <- dataset]
     -- mapM_ print encodedDataset
     r <- randomRBM maxFeatureId 30
     learnFromTrainingSet False 2000000 r $ map snd encodedDataset
     


main = exampleLearnTrigrams


