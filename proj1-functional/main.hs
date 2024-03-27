import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Data.List ( nub, sortBy )
import Data.Ord ( comparing )
import qualified Data.Text as T

data Tree =
    Node Int Double Tree Tree
    | Leaf String
    | Empty

data Arguments =
    Classify String String
    | Train String
    deriving Show

data Datapoint =
    Datapoint [Double] String
    deriving Show

data Split =
    Split [Datapoint] [Datapoint] Int Double Double
    deriving Show

instance Show Tree where
    show n@(Node {}) = showHelper n 2
    show l@(Leaf {}) = showHelper l 2
    show Empty = showHelper Empty 2

showHelper :: Tree -> Int -> String
showHelper (Node i t n1 n2) indent =
    "Node: " ++ show i ++ ", " ++ show t ++ "\n" ++
        replicate indent ' ' ++ showHelper n1 (indent + 2) ++ "\n" ++
        replicate indent ' ' ++ showHelper n2 (indent + 2)
showHelper (Leaf l) _ =
    "Leaf: " ++ l
showHelper Empty _ = "Empty"

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

parseArgs :: [String] -> Maybe Arguments
parseArgs ["-1", treeFile, dataFile] = Just (Classify treeFile dataFile)
parseArgs ["-2", dataFile] = Just (Train dataFile)
parseArgs _ = Nothing

parseTree :: String -> Tree
parseTree s = tree
    where
        (tree, _) = parseTree' (lines s)

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

parseInput :: String -> (String -> a) -> [a]
parseInput input parse =
    map parse (lines input)

parseLine :: String -> [Double]
parseLine line =
    map (read . T.unpack) (T.splitOn (T.pack ",") (T.pack line))

parseLabeledLine :: String -> Datapoint
parseLabeledLine line =
    Datapoint (map read $ init values) (last values)
    where
        values = map T.unpack (T.splitOn (T.pack ",") (T.pack line))

classify :: Tree -> [Double] -> String
classify Empty _ = []
classify (Leaf label) _ = label
classify (Node index threshold left right) values
    | values !! index < threshold = classify left values
    | otherwise = classify right values

calcGini :: [Datapoint] -> Double
calcGini dataset =
    1 - sum [(fromIntegral l / numLabels) ^ (2 :: Int) | l <- labelCounts]
    where
        numLabels = fromIntegral (length allLabels)
        allLabels = [l | (Datapoint _ l) <- dataset]
        labelCounts = [length (filter (== cur) allLabels) | cur <- nub allLabels]

giniForSplit :: [Datapoint] -> [Datapoint] -> Double
giniForSplit d1 d2 =
    calcGini d1 * len1 / total + calcGini d2 * len2 / total
    where
        len1 = fromIntegral $ length d1
        len2 = fromIntegral $ length d2
        total = len1 + len2

splitData :: Int -> [Datapoint] -> Double -> ([Datapoint], [Datapoint], Double)
splitData i dataset threshold =
    ([d | d@(Datapoint features _) <- dataset, features !! i < threshold],
    [d | d@(Datapoint features _) <- dataset, features !! i >= threshold],
    threshold)

getSplits :: [Datapoint] -> Int -> [Split]
getSplits dataset i =
    [Split d1 d2 i (giniForSplit d1 d2) threshold | (d1, d2, threshold) <- allSplits]
    where
        column = [features !! i | Datapoint features _ <- sortedDataset]
        splitPoints = [(x2 + x1) / 2 | (x1, x2) <- zip column (tail column)]
        allSplits = map (splitData i sortedDataset) splitPoints
        sortedDataset = sortBy (comparing $ getFeature i) dataset

getFeature :: Int -> Datapoint -> Double
getFeature i (Datapoint features _) =
    features !! i

bestSplit :: Split -> [Split] -> Split
bestSplit (Split bestLeft bestRight bestI bestGini bestT) ((Split left right i gini t):xs)
    | gini < bestGini = bestSplit (Split left right i gini t) xs
    | otherwise = bestSplit (Split bestLeft bestRight bestI bestGini bestT) xs
bestSplit best [] = best

findBestSplit :: [Datapoint] -> ([Datapoint], [Datapoint], Int, Double)
findBestSplit dataset =
    (d1, d2, i, t)
    where
        splits = map (bestSplit initSplit . getSplits dataset) [0..len]
        len = length features - 1
        (Split d1 d2 i _ t) = bestSplit initSplit splits
        initSplit = Split [] [] 0 99 0
        Datapoint features _ = head dataset

train :: [Datapoint] -> Tree
train [Datapoint _ label] =
    Leaf label
train dataset
    | calcGini dataset == 0 = Leaf label
    | otherwise = Node i t (train left) (train right)
    where
        (left, right, i, t) = findBestSplit dataset
        Datapoint _ label = head dataset
