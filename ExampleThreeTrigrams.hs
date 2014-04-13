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
import RBM(randomRBM, learnFromTrainingSet, stringFromBoolVector)

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

main =
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
     

-- One interesting extension could be to input not only single
-- characters via the 'Other' data, but pairs of characters.
-- Maybe adding a vowel/consonant distinction is helpful too.
