-- This example tries to learn some lexical aspects of the input text.
-- Namely, which letters are used together in three consecutive words (aka Trigram)
-- The features are built by marking the first and the last three letters distinct
-- from the other letters of each word.
--
-- The used text is from http://www.gutenberg.org/cache/epub/3160/pg3160.txt
-- which you'd need to download first...


import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified System.Random as R
import Data.Bits(testBit, shift, (.|.), (.&.))
import Data.Ord(comparing)
import Data.Maybe(catMaybes)
import Control.Monad(replicateM)
import Control.DeepSeq(deepseq)
import PrettyClassExt(printPretty)
import Text.PrettyPrint.HughesPJClass(Pretty, pPrint, text)
import RBM(randomRBM, randomVectorOfLength, learnFromTrainingSet,
           stringFromBoolVector, findMostProbableVisibles)

data Feature
  = First Char
  | Other Char
  | Last3 Char
  | Last2 Char
  | Last Char
    deriving (Eq, Ord, Show)

-- A WordFeature is a feature of the word at a relative position
type WordFeature = (Int, Feature)

type Words = [String]

extractFeatures :: Words -> [WordFeature]
extractFeatures words =
  [ (pos, feature)
  | (word, pos) <- zip words [1..]
  , feature <- features word]

features :: String -> [Feature]
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
  ++ (map Other $ unique $ drop 1 $ dropAtEnd 3 $ word)
  where l = length word

unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

dropAtEnd n = reverse . drop n . reverse

xwords = recur [] []
  where recur res word (c:cs)
          | C.isSpace c = wordBreak res word cs []
          | C.isAlpha c || c == '\'' =
              recur res (c:word) cs
          | c == ',' || c == '.' || c == '!' 
            || c == '?' || c == ';' =
              wordBreak res word (' ':cs) [c]
          | otherwise =
              wordBreak res word cs []
        recur res [] [] = reverse $ res
        recur res word [] = reverse $ reverse word : res
        
        wordBreak res [] cs nextWord = 
          recur res nextWord cs
        wordBreak res word cs nextWord =
          recur (reverse word : res) nextWord cs



encodeDataset :: M.Map WordFeature Int -> [[WordFeature]] -> [V.Vector Bool]
encodeDataset featureIds dataset =
  map (encodeInstance featureIds) dataset
  
encodeInstance featureIds xinstance =
  V.generate maxid $ flip S.member featureSet
  where
    maxid = maximumFeatureId featureIds
    featureSet = S.fromList $ catMaybes $ map lookup xinstance
    lookup feat = M.lookup feat featureIds

maximumFeatureId featureIds =
  L.maximum $ map snd $ M.toList featureIds

randomizeList :: [a] -> IO [a]
randomizeList list =
  do rs <- randomVectorOfLength $ length list
     return $ map snd $ L.sortBy (comparing fst) $
       zip (V.toList rs) list

instance Pretty Feature where
  pPrint = text . show

type Data = (Words, [WordFeature])

data TestData =
  Correct Words [WordFeature] (V.Vector Bool)
  | Modified [Modification] Words Words [WordFeature] [WordFeature]
             (V.Vector Bool) (V.Vector Bool)

data Modification =
  DroppedLetter Char
  | InsertedLetter Char
  | SwappedLetters Char Char
  | ReplacedLetter Char Char
    deriving (Show)
  
instance Show TestData where
  show (Correct ws _ _) = 
    "Correct " ++ show ws
  show (Modified ms _ mod _ _ _ _) = 
    "Modified " ++ show ms ++ "\n  " ++ show mod


prepareTestData :: M.Map WordFeature Int -> [Words] -> IO [TestData]
prepareTestData featureIds ts =
  do ms <- replicateM 2000 $ 
           randomlyModifiedTrigram featureIds $ 
           V.fromList ts
     return $
       [ let fs = extractFeatures trigram 
         in Correct trigram fs $ encodeInstance featureIds fs
       | trigram <- ts ]
       ++ take 1000 (catMaybes ms)

randomInt :: (Int,Int) -> IO Int
randomInt (from, to) = R.getStdRandom (R.randomR (from, to))

randomChar :: IO Char
randomChar = R.getStdRandom (R.randomR ('a', 'z'))

randomVectorElement :: V.Vector a -> IO a
randomVectorElement vec =
  do index <- randomInt (0, pred $ V.length vec)
     return $ vec V.! index

randomlyModifiedTrigram featureIds trigrams =
  do trigram <- randomVectorElement trigrams
     mods <- mapM randomlyModifiedWord trigram
     let (modifications0, modifiedWords) = unzip mods
         modifications = concat modifications0
         tfs = extractFeatures trigram
         mfs = extractFeatures modifiedWords
         enc = encodeInstance featureIds
     return $
       if null modifications
       then Nothing
       else Just $ 
            Modified modifications trigram modifiedWords
            tfs mfs (enc tfs) (enc mfs)

randomlyModifiedWord word =
  do r <- randomInt (1,100)
     if r < 30
       then modify
       else return ([], word)
  where
    modify =
      do r <- randomInt (1, 4)
         case r of
           1 -> do charIndex <- randomInt (0, pred $ length word)
                   let char = word !! charIndex
                   return ( [DroppedLetter char]
                          , deleteIndex charIndex word)
           2 -> do charIndex <- randomInt (0, length word)
                   char <- randomChar
                   return ( [InsertedLetter char]
                          , insertAtIndex charIndex char word)
           3 -> do let n = length word - 2
                   if n < 0
                     then return ([], word)
                     else do charIndex <- randomInt (0, n)
                             let (a, b, modword) =
                                   swapElementsAtIndex charIndex word
                             return ( [SwappedLetters a b] , modword)
           4 -> do charIndex <- randomInt (0, pred $ length word)
                   char <- randomChar
                   let (prev, modword) =
                         replaceCharAtIndex charIndex char word
                   return ( [ReplacedLetter prev char]
                          , modword)

deleteIndex idx list =
  let (before, (_:after)) = L.splitAt idx list
  in before ++ after

insertAtIndex idx el list =
  let (before, after) = L.splitAt idx list
  in before ++ [el] ++ after

swapElementsAtIndex idx list =
  let (before, a:b:after) = L.splitAt idx list
  in (a, b, before ++ [b,a] ++ after)

replaceCharAtIndex idx el list =
  let (before, x:after) = L.splitAt idx list
  in (x, before ++ [el] ++ after)


runTest r testData =
  do cs <- mapM canReconstruct testData
     putStrLn "results by number of modifications:"
     printPretty $
       M.fromListWith (M.unionWith (+))
       [ (numModifications t, M.singleton diff (1::Int))
       | (t, diff) <- zip testData cs ]
     putStrLn "results by kind of modifications:"
     printPretty  $
       M.fromListWith (M.unionWith (+))
       [ (modkind t, M.singleton diff (1::Int))
       | (t, diff) <- zip testData cs ]
  where
    canReconstruct testData =
      do -- print ("testing", testData)
         mp <- findMostProbableVisibles r $ input testData
         let res = boolVectorDifference mp $ expected testData
         res `deepseq` return res
    input (Correct _ _ encoded) = encoded
    input (Modified _ _ _ _ _ _ encoded) = encoded
    expected (Correct _ _ encoded) = encoded
    expected (Modified _ _ _ _ _ encoded _) = encoded
    numModifications (Correct _ _ _) = 0
    numModifications (Modified mods _ _ _ _ _ _) = length mods
    modkind (Correct _ _ _) = S.empty
    modkind (Modified mods _ _ _ _ _ _) = S.fromList $ map short mods
    short (DroppedLetter _) = "DroppedLetter"
    short (InsertedLetter _) = "InsertedLetter"
    short (SwappedLetters _ _) = "SwappedLetters"
    short (ReplacedLetter _ _) = "ReplacedLetter"

boolVectorDifference a b = V.sum $ V.zipWith boolDiff a b
  where boolDiff x y = if x == y then 0 else 1::Int


numTrigrams = 10000

main =
  do odyssey <- readFile "pg3160.txt"
     let trigrams = 
           [ take 3 ws
           | ws <- L.tails $ take numTrigrams $ drop 1000 $ 
                   xwords odyssey ]
     shuffledTrigrams <- randomizeList trigrams
     let (testTrigrams, trainingTrigrams) = 
           L.splitAt 100 shuffledTrigrams
     let trainingDataset =
           [ (trigram, extractFeatures trigram)
           | trigram <- trainingTrigrams ]
     putStrLn $ "some random input trigrams of the training set; " 
       ++ show (length trainingDataset)
     mapM_ printPretty $ take 20 $ shuffledTrigrams
     let allFeatures = 
           S.fromList $ concatMap extractFeatures trigrams
     print ("number of different features:", S.size $ allFeatures)
     let featureIds = M.fromList $ zip (S.toList allFeatures) [1..]
     let encodedTrainingData = 
           encodeDataset featureIds $ map snd trainingDataset
     -- mapM_ print encodedDataset
     let numVisibles = maximumFeatureId featureIds
     r <- randomRBM numVisibles 300
     preparedTestData <- prepareTestData featureIds testTrigrams
     mapM_ print preparedTestData
     learnLoop r encodedTrainingData preparedTestData
     
batchsize = 1000

learnLoop r0 trainingData testData =
  do r1 <- learnFromTrainingSet False batchsize r0 trainingData
     putStrLn "training iteration done, now testing:"
     runTest r1 testData
     learnLoop r1 trainingData testData

-- One interesting extension could be to input not only single
-- characters via the 'Other' data, but pairs of characters.
-- Maybe adding a vowel/consonant distinction is helpful too.
