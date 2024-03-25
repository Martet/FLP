import System.Environment ( getArgs )
import System.Exit ( die )
--import Debug.Trace
import qualified Data.Text as T

data Tree =
    Node Int Float Tree Tree
    | Leaf String
    deriving Show

data Arguments =
    Classify String String
    | Train String
    deriving Show

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Classify treeFile dataFile) -> do
            treeContent <- readFile treeFile
            case parseTree treeContent of
                Just decisionTree@(Node {}) -> do
                    dataContent <- readFile dataFile
                    mapM_ (putStrLn . classify decisionTree) (parseInput dataContent)
                _ ->
                    die "Error parsing tree"
        Just (Train dataFile) -> do
            dataContent <- readFile dataFile
            let inputData = parseInput dataContent
            print inputData
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
