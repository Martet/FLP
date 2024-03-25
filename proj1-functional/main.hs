import System.Environment ( getArgs )
import System.Exit ( die )
--import Debug.Trace
import qualified Data.Text as T
import Data.List ( nub )

data Tree =
    Node Int Float Tree Tree
    | Leaf String

data Arguments =
    Classify String String
    | Train String
    deriving Show

data Datapoint =
    Datapoint [Float] String
    deriving Show

instance Show Tree where
    show n@(Node {}) = showHelper n 2
    show l@(Leaf {}) = showHelper l 2

showHelper :: Tree -> Int -> String
showHelper (Node i t n1 n2) indent =
    "Node: " ++ show i ++ ", " ++ show t ++ "\n" ++
        take indent (repeat ' ') ++ showHelper n1 (indent + 2) ++ "\n" ++
        take indent (repeat ' ') ++ showHelper n2 (indent + 2)
showHelper (Leaf l) _ =
    "Leaf: " ++ l

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Classify treeFile dataFile) -> do
            treeContent <- readFile treeFile
            case parseTree treeContent of
                Just decisionTree@(Node {}) -> do
                    dataContent <- readFile dataFile
                    mapM_ (putStrLn . classify decisionTree) (parseInput dataContent parseLine)
                _ ->
                    die "Error parsing tree"
        Just (Train dataFile) -> do
            dataContent <- readFile dataFile
            let inputData = parseInput dataContent parseLabeledLine
            print (train inputData)
        Nothing ->
            die "Wrong arguments"

parseArgs :: [String] -> Maybe Arguments
parseArgs ["-1", treeFile, dataFile] = Just (Classify treeFile dataFile)
parseArgs ["-2", dataFile] = Just (Train dataFile)
parseArgs _ = Nothing

parseTree :: String -> Maybe Tree
parseTree s =
    case parseTree' (lines s) of
        Just (t, _) -> Just t
        Nothing -> Nothing

parseTree' :: [String] -> Maybe (Tree, [String])
parseTree' [] = Nothing
parseTree' (x:xs) =
    case head splitLine of
        "Node:" ->
            Just (Node
                (read $ init (splitLine !! 1))
                (read $ splitLine !! 2)
                left
                right, rest)
            where
                Just (right, rest) = parseTree' afterLeft
                Just (left, afterLeft) = parseTree' xs
        "Leaf:" -> Just (Leaf (last splitLine), xs)
        _ -> Nothing
    where
        splitLine = words x

parseInput :: String -> (String -> a) -> [a]
parseInput input parse =
    map parse (lines input)

parseLine :: String -> [Float]
parseLine line =
    map (read . T.unpack) (T.splitOn (T.pack ",") (T.pack line))

parseLabeledLine :: String -> Datapoint
parseLabeledLine line =
    Datapoint (map read $ init values) (last values)
    where
        values = map (T.unpack) (T.splitOn (T.pack ",") (T.pack line))

classify :: Tree -> [Float] -> String
classify (Leaf label) _ = label
classify (Node index threshold left right) values
    | values !! index < threshold = classify left values
    | otherwise = classify right values

calcGini :: [Datapoint] -> Float
calcGini dataset =
    1.0 - sum [(fromIntegral (length l) / numLabels) ^ 2 | l <- [filter (== cur) labels | cur <- nub labels]]
    where
        numLabels = fromIntegral (length labels)
        labels = [l | (Datapoint _ l) <- dataset]

giniForSplit :: ([Datapoint], [Datapoint]) -> Float
giniForSplit (d1, d2) =
    (calcGini d1) * len1 / total + (calcGini d2) * len2 / total
    where
        len1 = fromIntegral $ length d1
        len2 = fromIntegral $ length d2
        total = len1 + len2

splitData :: Int -> [Datapoint] -> Float -> ([Datapoint], [Datapoint], Float)
splitData i dataset threshold =
    ([d | d@(Datapoint features _) <- dataset, features !! i < threshold],
    [d | d@(Datapoint features _) <- dataset, features !! i >= threshold],
    threshold)

getSplits :: [Datapoint] -> Int -> [(([Datapoint], [Datapoint]), Int, Float, Float)]
getSplits dataset i =
    [((d1, d2), i, giniForSplit (d1, d2), threshold) | (d1, d2, threshold) <- map (splitData i dataset) splitPoints]
    where
        column = [features !! i | Datapoint features _ <- dataset]
        splitPoints = [(x2 + x1) / 2 | (x1, x2) <- zip column (tail column)]

bestSplit :: (([Datapoint], [Datapoint]), Int, Float, Float) -> [(([Datapoint], [Datapoint]), Int, Float, Float)] -> (([Datapoint], [Datapoint]), Int, Float, Float)
bestSplit (bestSets, bestI, bestGini, bestT) ((datasets, i, gini, t):xs)
    | gini < bestGini = bestSplit (datasets, i, gini, t) xs
    | otherwise = bestSplit (bestSets, bestI, bestGini, bestT) xs
bestSplit best [] = best

findBestSplit :: [Datapoint] -> (([Datapoint], [Datapoint]), Int, Float)
findBestSplit dataset =
    ((d1, d2), i, t)
    where
        splits = map (bestSplit (([],[]), 0, 99, 0)) (map (getSplits dataset) [0..len])
        len = (featureCount $ head dataset) - 1
        ((d1, d2), i, _, t) = bestSplit (([],[]), 0, 99, 0) splits

featureCount :: Datapoint -> Int
featureCount (Datapoint features _) = length features

train :: [Datapoint] -> Tree
train [Datapoint _ label] =
    Leaf label
train dataset =
    Node i t (train left) (train right)
    where
        ((left, right), i, t) = findBestSplit dataset
