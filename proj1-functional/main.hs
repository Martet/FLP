import System.Environment ( getArgs )
import System.Exit ( die )
--import Debug.Trace
import qualified Data.Text as T

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
