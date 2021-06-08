-- module SimulatedAnnealing.TSPLIB95

-- import Data.TSP
import Text.Parsec hiding (eol)
import Text.Parsec.String
import Text.Parsec.Perm

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

none :: Parser ()
none = return ()

eol :: Parser ()
eol = endOfLine >> none

blank :: Parser Char
blank = char ' ' <|> char '\t'

tok :: Parser p -> Parser p
tok p = many blank *> p

tok' :: Parser p -> Parser p
tok' p = tok p <* many blank

oneOfString :: [String] -> Parser String
oneOfString = choice . map (try . string)


{-
    Common Tokens & Symbols
-}

eofSym = string "EOF" *> none

endOfTSP :: Parser ()
endOfTSP = eof <|> eofSym

colon :: Parser Char
colon = char ':'

type Key = String

specKey = oneOfString [
    "NAME", "TYPE", "COMMENT", "DIMENSION", "CAPACITY", "EDGE_WEIGHT_TYPE",
    "EDGE_WEIGHT_FORMAT", "EDGE_DATA_FORMAT", "NODE_COORD_TYPE", "DISPLAY_DATA_TYPE"]
    <?> "specification keyword"

dataKey = oneOfString [
    "NODE_COORD_SECTION", "DEPOT_SECTION", "DEMAND_SECTION", "EDGE_DATA_SECTION",
    "FIXED_EDGES_SECTION", "DISPLAY_DATA_SECTION", "TOUR_SECTION", "EDGE_WEIGHT_SECTION"]
    <?> "data keyword"

anyKey = dataKey <|> specKey <?> "keyword"



{-
    Number parsers
-}

digit1 :: Parser String
digit1 = many1 digit

int :: Parser Int
int = read <$> many1 digit

-- TODO: improve (exponent notation, negative, etc)
real :: Parser Float
real = read <$> p where
    p = (++) <$> digit1 <*> option "" rest
    rest = (:) <$> char '.' <*> digit1 


{-
    Specification Part (TSPLIB95)
-}

data SpecEntry = StringEntry Key String | IntEntry Key Int

-- Parses the symbol that separates a spec key and its values
specSep :: Parser ()
specSep = tok' $ optional colon

specEntry :: Key -> Parser v -> Parser (Key, v)
specEntry s p = (,) <$> string s <*> (specSep *> p <* tok eol)

-- Reads a string until EOL (consumes EOL but discards it)
specStringVal :: Parser String
specStringVal = manyTill anyChar (lookAhead $ try eol)

stringEntry :: Key -> Parser SpecEntry
stringEntry s = uncurry StringEntry <$> specEntry s specStringVal

intEntry :: Key -> Parser SpecEntry
intEntry s = uncurry IntEntry <$> specEntry s int

name :: Parser SpecEntry
name = stringEntry "NAME"
comment :: Parser SpecEntry
comment = stringEntry "COMMENT"
dim :: Parser SpecEntry
dim = intEntry "DIMENSION"
typ' :: Parser SpecEntry
typ' = stringEntry "TYPE" >>= (\e -> case e of
    StringEntry k "TSP"     -> return e
    _                       -> fail "unsupported type")
weightType :: Parser SpecEntry
weightType = stringEntry "EDGE_WEIGHT_TYPE" >>= (\e -> case e of
    StringEntry k "EUC_2D"  -> return e
    _                       -> fail "unsupported edge weight type")

-- weightType = oneOfString [""]
-- weightType = entry "EDGE_WEIGHT_TYPE" $ choice

-- specEntry :: Parser SpecEntry
-- specEntry = permute 

data Spec = Spec SpecEntry SpecEntry SpecEntry SpecEntry SpecEntry

spec :: Parser Spec
spec = permute (Spec <$$> name <||> comment <||> typ' <||> dim <||> weightType)


{-
    Data Part (TSPLIB95)
-}

data DataEntry =
    VecI Key [Int]      | Mat2I Key [[Int]]     | Mat3I Key [[[Int]]]       |
    VecF Key [Float]    | Mat2F Key [[Float]]   | Mat3F Key [[[Float]]]
    deriving Show

-- Parses the symbol that separates a data key and its value
dataSep :: Parser ()
dataSep = tok' (optional colon) <* eol

dataEntry :: Key -> Parser v -> Parser (Key, v)
dataEntry k p = (,) <$> string k <*> (dataSep *> p)

-- cell = (,) <$> real <*> real

-- pair :: Parser c -> Parser (c, c)
-- pair p = (,) <$> p <*> p

-- triple :: Parser c -> Parser (c, c, c)
-- triple p = (,,) <$> p <*> p <*> p


-- row' :: Parser a -> Parser b -> Parser b

-- list :: Parser a -> Parser [a]
-- list cs = sepEndBy1 cs (many1 blank)

-- lines :: Parser a -> Parser [a]
-- lines p = sepEndBy1 p endOfLine

-- mat = 

-- Parses cs which is optionally surrounded by whitespace,
-- but must precede the end of line. EOL is consumed but discarded.
row :: Parser a -> Parser a
row cs = tok' cs <* eol

nodeCoordSection :: Int -> Parser DataEntry
nodeCoordSection dim =
    let coord n = int *> many blank *> count n (real <* many blank)
        coords d n = count dim $ row (coord n)
    in  uncurry Mat2F <$> dataEntry "NODE_COORD_SECTION" (coords dim 2)

tourSection :: Parser DataEntry
tourSection =
    let
        endTok = tok' $ string "-1" <* eol
        tour' = endBy1 (tok' int) eol
        tours = sepEndBy1 tour' endTok <* optional (try endTok)
    in uncurry Mat2I <$> dataEntry "TOUR_SECTION" tours

data Data = Data DataEntry
    deriving Show

tspData :: Spec -> Parser Data
tspData (Spec _ _ _ (IntEntry "DIMENSION" dim) _) = Data <$> nodeCoordSection dim


{-
    High level TSPLIB95 file parsing
-}

parseTSP :: FilePath -> IO ()
parseTSP filePath = do
    contents <- readFile filePath
    let 
        spec' = (Spec (StringEntry "NAME" "name") (StringEntry "COMMENT" "comment") (StringEntry "TYPE" "type") (IntEntry "DIMENSION" 280) (StringEntry "EDGE_WEIGHT_TYPE" "EUC_2D"))
        parser = (return spec') >>= tspData
    print $ parse parser filePath contents


main :: IO ()
main = do parseTSP "../../ALL_tsp/a280.tsp"


-- data Data = Data DataEntry

-- dat :: Int -> Parser Data
-- dat dim = permute (Data <$$> nodeCoordSection dim)


-- multi :: Parser [String]
-- multi = many endOfLine >> manyTill single (try endOfMultiValue)

--tspFile :: Parser [Entry]
--tspFile = do
    --r <- many1 entry
    --many space >> endOfTSP
    --return r



--parseTSP :: String -> Either ParseError (Int, CostFunc)
--parseTSP s =
    --do
        --entriesRaw <- (parse tspFile "(unknown)" s) 
        --let
            --entries = Map.fromList $ map (\(Entry (x,y)) -> (x,y)) entriesRaw
            --Just (SingleValue dimV) = Map.lookup "DIMENSION" entries
            --Just (MultiValue nodeCoordsV) = Map.lookup "NODE_COORD_SECTION" entries
            --dim = read dimV :: Int
        --coords <- parse (nodeCoords dim) "(unknown)" nodeCoordsV
        ---- if lookup "TYPE" entries then ParseError "Only \"TYPE: TSP\" is supported"
            ---- lookup "EDGE_WEIGHT_TYPE" entries != "EUC_2D" -> ParseError "Only \"EDGE_WEIGHT_TYPE: EUC_2D\" is supported"
            ---- otherwise -> (dim, coords)
        --return (dim, weight coords)

-- readTSP :: String -> (Int, CostFunc)
-- readTSP s = parseTSP

-- euclidian2d :: (Int, Int) -> (Int, Int) -> Integer
-- euclidian2d (x, y) (x', y') =
    ---- let
        --yd = toInteger y - toInteger y'
    --in round $ sqrt $ fromIntegral $ (xd * xd) + (yd * yd)

--weight :: IntMap.IntMap (Int, Int) -> (Node, Node) -> Integer
--weight coords (a, b) =
    --let coord = (coords IntMap.!)
--    in euclidian2d (coord a) (coord b)