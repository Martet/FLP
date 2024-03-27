-- main.hs
-- Functional FLP project, training and classification with decision trees
-- Author: Martin Zmitko
-- Login: xzmitk01
-- Year: 2024

import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Data.List ( nub, sortBy )
import Data.Ord ( comparing )
import qualified Data.Text as T -- for splitOn

-- decision tree
data Tree =
    Node Int Double Tree Tree -- feature index, threshold, left subtree, right subtree
    | Leaf String -- label
    | Empty

-- type for arguments as type of action containing input file names
data Arguments =
    Classify String String -- tree file, data file
    | Train String -- data file
    deriving Show

-- single labeled datapoint, [Datapoint] later refered to as dataset
data Datapoint =
    Datapoint [Double] String -- list of features, label
    deriving Show

-- data type for split dataset on each node, containing:
-- left subtree, right subtree, feature index, gini impurity, threshold
data Split =
    Split [Datapoint] [Datapoint] Int Double Double
    deriving Show

instance Show Tree where
    show n@(Node {}) = showHelper n 2
    show l@(Leaf {}) = showHelper l 2
    show Empty = showHelper Empty 2

-- Converts the tree into string with correct indentation
-- @param Tree the tree to print
-- @param Int indentation in spaces of the next layer
-- @return String formatted tree
showHelper :: Tree -> Int -> String
showHelper (Node i t n1 n2) indent =
    "Node: " ++ show i ++ ", " ++ show t ++ "\n" ++
        replicate indent ' ' ++ showHelper n1 (indent + 2) ++ "\n" ++
        replicate indent ' ' ++ showHelper n2 (indent + 2)
showHelper (Leaf l) _ =
    "Leaf: " ++ l
showHelper Empty _ = "Empty"

-- Entrypoint
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Classify treeFile dataFile) -> do
            treeContent <- readFile treeFile
            dataContent <- readFile dataFile
            case parseTree treeContent of
                decisionTree@(Node {}) -> do
                    mapM_ (putStrLn . classify decisionTree) (parseInput dataContent parseLine)
                decisionTree@(Leaf {}) -> do
                    mapM_ (putStrLn . classify decisionTree) (parseInput dataContent parseLine)
                _ -> do
                    hPutStrLn stderr "Error parsing tree"
                    exitFailure
        Just (Train dataFile) -> do
            dataContent <- readFile dataFile
            let inputData = parseInput dataContent parseLabeledLine
            print (train inputData)
        Nothing -> do
            hPutStrLn stderr "Wrong arguments"
            exitFailure

-- Parses program arguments
-- @param [String] list of arguments
-- @return Maybe Arguments Classify or Train, Nothing on fail
parseArgs :: [String] -> Maybe Arguments
parseArgs ["-1", treeFile, dataFile] = Just (Classify treeFile dataFile)
parseArgs ["-2", dataFile] = Just (Train dataFile)
parseArgs _ = Nothing

-- Parses the decision tree from text, wrapper for recursive parser
-- @param String content of input file
-- @return Tree the parsed tree
parseTree :: String -> Tree
parseTree s = tree
    where
        (tree, _) = parseTree' (lines s)

-- Recursive tree parser, splits each line on whitespace and parses values in expected format
-- @param [String] list of input lines to parse
-- @return (Tree, [String]) parsed tree with remaining lines to parse
parseTree' :: [String] -> (Tree, [String])
parseTree' [] = (Empty, [])
parseTree' (x:xs)
    | len == 2 || len == 3 =
        case head splitLine of
            "Node:" ->
                (Node
                    (read $ init (splitLine !! 1))
                    (read $ splitLine !! 2)
                    left
                    right, rest)
                where
                    (right, rest) = parseTree' afterLeft
                    (left, afterLeft) = parseTree' xs
            "Leaf:" -> (Leaf (last splitLine), xs)
            _ -> (Empty, xs)
    | otherwise = (Empty, xs)
    where
        splitLine = words x
        len = length splitLine

-- Parses input data from file content
-- @param String content of input file
-- @param (String -> a) function that parses a single line (labeled or not)
-- @return [a] list of parsed objects ([[Double]] for unlabeled, [Datapoint] for labeled)
parseInput :: String -> (String -> a) -> [a]
parseInput input parse =
    map parse (lines input)

-- Parses unlabeled line by splitting on commas
-- @param String line to parse
-- @return [Double] list of features
parseLine :: String -> [Double]
parseLine line =
    map (read . T.unpack) (T.splitOn (T.pack ",") (T.pack line))

-- Parses labeled line by splitting on commas and saving last as label
-- @param String line to parse
-- @return Datapoint labeled list of features
parseLabeledLine :: String -> Datapoint
parseLabeledLine line =
    Datapoint (map read $ init values) (last values)
    where
        values = map T.unpack (T.splitOn (T.pack ",") (T.pack line))

-- Classifies an object
-- @param Tree decision tree to use
-- @param [Double] features of the object to parse
-- @return String class of object
classify :: Tree -> [Double] -> String
classify Empty _ = []
classify (Leaf label) _ = label
classify (Node index threshold left right) values
    | values !! index < threshold = classify left values
    | otherwise = classify right values

-- Calculates the gini impurity for a given dataset
-- @param [Datapoint] dataset to calculate
-- @return Double gini impurity
calcGini :: [Datapoint] -> Double
calcGini dataset =
    1 - sum [(fromIntegral l / numLabels) ^ (2 :: Int) | l <- labelCounts]
    where
        numLabels = fromIntegral (length allLabels)
        allLabels = [l | (Datapoint _ l) <- dataset]
        labelCounts = [length (filter (== cur) allLabels) | cur <- nub allLabels]

-- Calculates the gini impurity for a new split of the dataset
-- @param [Datapoint] left part of dataset
-- @param [Datapoint] right part of dataset
-- @return Double gini impurity
giniForSplit :: [Datapoint] -> [Datapoint] -> Double
giniForSplit d1 d2 =
    calcGini d1 * len1 / total + calcGini d2 * len2 / total
    where
        len1 = fromIntegral $ length d1
        len2 = fromIntegral $ length d2
        total = len1 + len2

-- Splits the dataset on feature by threshold into two parts
-- @param Int index of feature to split on
-- @param [Datapoint] dataset to split
-- @param Double threshold
-- @return ([Datapoint], [Datapoint], Double) tuple with both parts of dataset and used threshold
splitData :: Int -> [Datapoint] -> Double -> ([Datapoint], [Datapoint], Double)
splitData i dataset threshold =
    ([d | d@(Datapoint features _) <- dataset, features !! i < threshold],
    [d | d@(Datapoint features _) <- dataset, features !! i >= threshold],
    threshold)

-- Get all possible splits on a given feature
-- @param [Datapoint] dataset to split
-- @param Int index of feature to split on
-- @return [Split] list of all possible splits
getSplits :: [Datapoint] -> Int -> [Split]
getSplits dataset i =
    [Split d1 d2 i (giniForSplit d1 d2) threshold | (d1, d2, threshold) <- allSplits]
    where
        column = [features !! i | Datapoint features _ <- sortedDataset]
        splitPoints = [(x2 + x1) / 2 | (x1, x2) <- zip column (tail column)]
        allSplits = map (splitData i sortedDataset) splitPoints
        sortedDataset = sortBy (comparing $ getFeature i) dataset

-- Gets a feature
-- @param Int index of feature to return
-- @param [Datapoint] datapoint
-- @return Double the desired feature
getFeature :: Int -> Datapoint -> Double
getFeature i (Datapoint features _) =
    features !! i

-- Finds the split with the lowest gini impurity in a list of splits
-- @param Split the current best split
-- @param [Split] list of splits to search
-- @return Split the best split
bestSplit :: Split -> [Split] -> Split
bestSplit (Split bestLeft bestRight bestI bestGini bestT) ((Split left right i gini t):xs)
    | gini < bestGini = bestSplit (Split left right i gini t) xs
    | otherwise = bestSplit (Split bestLeft bestRight bestI bestGini bestT) xs
bestSplit best [] = best

-- Finds the best possible split on the dataset
-- @param [Datapoint] dataset
-- @return ([Datapoint], [Datapoint], Int, Double) tuple with both parts of the newly split dataset,
-- index of feature that was split on and the threshold used
findBestSplit :: [Datapoint] -> ([Datapoint], [Datapoint], Int, Double)
findBestSplit dataset =
    (d1, d2, i, t)
    where
        splits = map (bestSplit initSplit . getSplits dataset) [0..len] -- best split for each feature
        len = length features - 1
        (Split d1 d2 i _ t) = bestSplit initSplit splits -- best split across all features
        initSplit = Split [] [] 0 99 0
        Datapoint features _ = head dataset

-- Constructs a decision tree from training labeled dataset in a greedy way using CERT
-- @param [Datapoint] dataset
-- @return Tree the resulting tree
train :: [Datapoint] -> Tree
train [Datapoint _ label] =
    Leaf label
train dataset
    | calcGini dataset == 0 = Leaf label -- end if all labels same
    | otherwise = Node i t (train left) (train right)
    where
        (left, right, i, t) = findBestSplit dataset
        Datapoint _ label = head dataset
