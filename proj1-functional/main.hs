import System.Environment
import System.IO
import System.Exit
import Debug.Trace
import qualified Data.Text as T

data Tree =
    Node Int Float Tree Tree
    | Leaf String
    deriving Show

data Arguments =
    Classify String String
    | Learn String
    deriving Show

main = do
    args <- getArgs
    case parseArgs args of
        Just (Classify treeFile dataFile) -> do
            treeContent <- readFile treeFile
            case parseTree treeContent of
                Just decisionTree@(Node _ _ _ _) -> do
                    dataContent <- readFile dataFile
                    mapM_ putStrLn $ map (classify decisionTree) (parseInput dataContent)
                Nothing ->
                    die "Error parsing tree"
        Just (Learn dataFile) -> do
            dataContent <- readFile dataFile
            let inputData = parseInput dataContent
            putStrLn $ show inputData
        Nothing ->
            die "Wrong arguments"

parseArgs :: [String] -> Maybe Arguments
parseArgs ("-1":treeFile:dataFile:[]) = Just (Classify treeFile dataFile)
parseArgs ("-2":dataFile:[]) = Just (Learn dataFile)
parseArgs _ = Nothing

parseTree :: String -> Maybe Tree
parseTree s =
    case parseTree' (lines s) of
        Just (t, _) -> Just t
        Nothing -> Nothing

parseTree' :: [String] -> Maybe (Tree, [String])
parseTree' [] = Nothing
parseTree' (x:xs) = do
    let splitLine = words x
    case head splitLine of
        "Node:" -> do
            (left, afterLeft) <- parseTree' xs
            (right, rest) <- parseTree' afterLeft
            return (Node
                    (read $ init (splitLine !! 1))
                    (read $ splitLine !! 2)
                    left
                    right,
                rest)
        "Leaf:" -> return (Leaf (last splitLine), xs)
        _ -> Nothing

parseLine :: String -> [Float]
parseLine line =
    map (read . T.unpack) (T.splitOn (T.pack ",") (T.pack line))

parseInput :: String -> [[Float]]
parseInput input =
    map parseLine (lines input)

classify :: Tree -> [Float] -> String
classify (Leaf label) _ = label
classify (Node index threshold left right) values
    | values !! index < threshold = classify left values
    | otherwise = classify right values
