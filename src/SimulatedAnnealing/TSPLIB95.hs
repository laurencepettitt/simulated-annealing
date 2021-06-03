module SimulatedAnnealing.TSPLIB95 where

import SimulatedAnnealing
import SimulatedAnnealing.TSP
import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Data.Char
import qualified Data.Map as M
import qualified Data.List as L (intercalate, stripPrefix, sortBy)
import Data.Maybe ( catMaybes, isJust, listToMaybe )
import qualified Data.Text as T
import Data.Ord (comparing)
import System.Random (mkStdGen)
import qualified Data.Sequence as S
import Control.Monad.State (runState)
import Data.Foldable (toList)
import Debug.Trace (trace)
import Data.List (sortOn)

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
        maybeSplits = map (`spanOnPrefix` list) prefixes
        splits = catMaybes maybeSplits
    in
        listToMaybe splits

splitBy  :: ([a] -> Maybe ([a], [a]))-> [a] -> [[a]]
splitBy f [] = []
splitBy f s =
    let continue (p, s') = p : splitBy f s'
        search s'
          | isJust (f s') = ([], s')
          | null s' = ([], [])
          | otherwise = let (p, t) = (search $ tail s')
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
        prefixes = sortOn length keyWords
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
        tourSectionRaw = init fromTourSection
    in map ((\x -> x-1) . read) tourSectionRaw

readTSP :: String -> TSP
readTSP string =
    -- Extract sections, and then process into a TSP
    let
        ss = sections string
        n = read $ ss M.! "DIMENSION"
        nodeCoordsRaw = ss M.! "NODE_COORD_SECTION"
        ws = words nodeCoordsRaw
        is = map read ws :: [Int]
        ts = triples is
        coordsList = map (\(x,y,z) -> (y,z)) ts
        coords = M.fromList $ zip [0..] coordsList
    in
         TSP { n = n, costOf = weight coords }


newtype Parser a = Parser (String -> Maybe (a, String))

fail :: Parser a
fail = Parser (const Nothing)

anyChar :: Parser Char
anyChar = Parser (\s ->
    case s of
        "" -> Nothing
        c : cs -> Just (c, cs)
    )

apply :: Parser a -> String -> Maybe (a, String)
apply (Parser p) = p

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\s ->
    case apply p s of
        Just (x, s') -> Just (x, s')
        Nothing -> apply q s
    )

instance Monad Parser where
    return x = Parser (\s -> Just (x, s))

    p >>= q = Parser (\s -> do
            (x, s') <- apply p s
            (y, s'') <- apply (q x) s'
            return (y, s'')
        )

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    c <- anyChar
    if f c then return c else fail

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

space :: Parser Char
space = satisfy isSpace

alphaNum :: Parser Char
alphaNum = letter <|> digit

none :: Parser [a]
none = return []

many :: Parser a -> Parser [a]
many p = many1 p <|> none

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    return (x : xs)

digits :: Parser String
digits = many digit

digits1 :: Parser String
digits1 = many1 digit

int :: Parser Int
int = do read <$> digits1

token :: Parser a -> Parser a
token p = many space >> p

sym :: Char -> Parser Char
sym c = token (satisfy (==c))



-- data Specs = 

-- data TSPLIB95 = TSPLIB95 Specs Data


-- tsplib95 :: Parser a
-- tsplib95 = spec >> data