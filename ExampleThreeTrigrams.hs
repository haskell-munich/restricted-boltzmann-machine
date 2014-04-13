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
import Data.Bits(testBit, shift, (.|.), (.&.))
import Data.Ord(comparing)
import PrettyClassExt(printPretty)
import Text.PrettyPrint.HughesPJClass(Pretty, pPrint, text)
import RBM(randomRBM, randomVectorOfLength, learnFromTrainingSet, stringFromBoolVector)

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


encodeDataset :: M.Map WordFeature Int 
                 -> [(Words, [WordFeature])]
                 -> [(Words, V.Vector Bool)]
encodeDataset featureIds dataset =
  [ (theWords,
     let featureSet = S.fromList $
                      map (featureIds M.!) features
     in V.generate maxid (flip S.member featureSet))
  | (theWords, features) <- dataset]
  where maxid = maximumFeatureId featureIds

maximumFeatureId featureIds =
  L.maximum $ map snd $ M.toList featureIds

randomizeList :: [a] -> IO [a]
randomizeList list =
  do rs <- randomVectorOfLength $ length list
     return $ map snd $ L.sortBy (comparing fst) $
       zip (V.toList rs) list

instance Pretty Feature where
  pPrint = text . show

numTrigrams = 10000

main =
  do odyssey <- readFile "pg3160.txt"
     let dataset =
           [ (someWords, extractFeatures someWords)
           | ws <- L.tails $ take numTrigrams $ drop 1000 $
                   xwords odyssey
           , let someWords = take 3 ws ]
     shuffledDataset <- randomizeList dataset
     putStrLn $ "some random input trigrams of the " ++ show numTrigrams
     mapM_ printPretty $ take 20 $ shuffledDataset
     let (testData, trainingData) = L.splitAt 100 shuffledDataset
     let allFeatures = S.unions $ map (S.fromList . snd) trainingData
     print ("number of different features:", S.size $ allFeatures)
     let featureIds = M.fromList $ zip (S.toList allFeatures) [1..]
     let encodedTrainingData = encodeDataset featureIds trainingData
     -- mapM_ print encodedDataset
     let numVisibles = maximumFeatureId featureIds
     r <- randomRBM numVisibles 30
     learnFromTrainingSet False 2000000 r $
       map snd encodedTrainingData
     

-- One interesting extension could be to input not only single
-- characters via the 'Other' data, but pairs of characters.
-- Maybe adding a vowel/consonant distinction is helpful too.
