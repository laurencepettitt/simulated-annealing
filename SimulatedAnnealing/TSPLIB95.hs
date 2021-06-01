module SimulatedAnnealing.TSPLIB95 where

import SimulatedAnnealing
import SimulatedAnnealing.TSP
import qualified Data.Map as M
import qualified Data.List as L (intercalate, stripPrefix, sortBy)
import Data.Maybe
import qualified Data.Text as T
import Data.Ord (comparing)
-- import Data.Function (on)
import System.Random (mkStdGen)
import qualified Data.Sequence as S
import Control.Monad.State (runState)
import Data.Foldable (toList)
import Debug.Trace (trace)

fieldDelim :: Char
fieldDelim = ':'

keyWords :: [String]
keyWords = 
    [
    "NAME", "TYPE", "COMMENT", "DIMENSION", "CAPACITY",
    "EDGE_WEIGHT_TYPE", "EDGE_WEIGHT_FORMAT", "EDGE_DATA_FORMAT",
    "NODE_COORD_TYPE", "DISPLAY_DATA_TYPE",
    "EOF",
    "NODE_COORD_SECTION",
    "DEPOT_SECTION",
    "DEMAND_SECTION",
    "EDGE_DATA_SECTION",
    "FIXED_EDGES_SECTION",
    "DISPLAY_DATA_SECTION",
    "TOUR_SECTION",
    "EDGE_WEIGHT_SECTION"
    ]

spanOnPrefix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
spanOnPrefix prefix list = do
    remaining <- L.stripPrefix prefix list
    return (prefix, remaining)

spanOnAnyPrefix :: Eq a => [[a]] -> [a] -> Maybe ([a], [a])
spanOnAnyPrefix prefixes list = 
    let
        maybeSplits = map (flip spanOnPrefix list) prefixes
        splits = catMaybes maybeSplits
    in
        listToMaybe splits

splitBy  :: ([a] -> Maybe ([a], [a]))-> [a] -> [[a]]
splitBy f [] = []
splitBy f s =
    let continue (p, s') = p : (splitBy f s')
        search s' =  if isJust (f s')
                    then ([], s')
                    else if null s' then ([], [])
                    else let (p, t) = (search $ tail s')
                         in  (head s' : p, t)
    in maybe (continue $ search s) continue $ f s

cleanField :: String -> String
cleanField =
    let
        strip = T.unpack . T.strip . T.pack
        replaceUnwanted = map (\c -> if c == fieldDelim || c == '\n' then ' ' else c)
    in strip . replaceUnwanted

pairs :: [a] -> ([a], [a])
pairs [] = ([], [])
pairs (x : xs) = (x : y1, y2) where (y2, y1) = pairs xs

triples :: [a] -> [(a, a, a)]
triples [] = []
triples [_] = []
triples [_, _] = []
triples (a : (b : (c : rs))) = (a, b, c) : triples rs

sections :: String -> M.Map String String
sections string =
    let
        prefixes = L.sortBy (comparing length) keyWords
        sectionsRaw = splitBy (spanOnAnyPrefix prefixes) string
        sectionsClean = map cleanField sectionsRaw
        assocList = uncurry zip $ pairs sectionsClean
    in
        M.fromList assocList

euclidian2d :: (Int, Int) -> (Int, Int) -> Integer
euclidian2d (x, y) (x', y') = 
    let
        xd = toInteger x - toInteger x'
        yd = toInteger y - toInteger y'
    in toInteger $ round $ sqrt $ fromIntegral $ (xd * xd) + (yd * yd)

weight :: M.Map Int (Int, Int) -> (Node, Node) -> Cost
weight coords (a, b) =
    let coord = (coords M.!)
    in euclidian2d (coord a) (coord b)

readOptTour :: String -> [Int]
readOptTour string =
    let
        ls = lines string
        getFromTourSection ("TOUR_SECTION" : rs) = rs
        getFromTourSection (x : rs) = getFromTourSection rs
        fromTourSection = getFromTourSection ls
        tourSectionRaw = reverse $ tail $ reverse fromTourSection
    in map (\x -> x-1) $ map read tourSectionRaw

readTSP :: String -> TSP
readTSP string =
    -- Extract sections, and then process into a TSP
    let
        ss = sections string
        n = read $ ss M.! "DIMENSION"
        nodeCoordsRaw = ss M.! "NODE_COORD_SECTION"
        ws = words nodeCoordsRaw
        is = map (read) ws :: [Int]
        ts = triples is
        coordsList = map (\(x,y,z) -> (y,z)) ts
        coords = M.fromList $ zip [0..] coordsList
    in
         TSP { n = n, costOf = weight coords }

-- frequency :: Ord a => [a] -> [(Int,a)] 
-- frequency list = map (\l -> (length l, head l)) (L.group (L.sort list))

-- stats :: IO ()
-- stats = do
--     let
--         folder = "./ALL_tsp/"
--     files <- getDirectoryContents folder
--     let
--         filePaths = map (folder ++) files
--         problemFiles = filter (L.isSuffixOf ".tsp") filePaths
--     problemFilesContent <- mapM readFile problemFiles
--     let
--         problems = map sections problemFilesContent
--         types = map (M.! "EDGE_WEIGHT_TYPE") problems
--         freqs = frequency types
--         freqsSorted = L.reverse $ L.sortBy (comparing fst) freqs
--         top5 = take 5 freqsSorted
--         rest = drop 5 freqsSorted
--     print $ top5
--     print $ rest
