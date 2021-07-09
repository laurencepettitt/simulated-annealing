{-# LANGUAGE OverloadedStrings #-}

{- |Module      :  TSPLIB95
 - 
    The module provides a TSPLIB95 parser.
-}
module TSPLIB95 where

import Prelude hiding (lookup)
import Control.Arrow (left)
import Control.Applicative ( Alternative )
import Control.Monad.State.Strict
import Data.Void ( Void )
import Data.Maybe ( fromJust, fromMaybe )
import qualified Data.Map as M
import Data.Scientific ( Scientific, toRealFloat )
import qualified Data.Text as T
import Text.Read ( readMaybe )
import Text.Megaparsec hiding ( State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import GTSP

basePath :: [Char]
basePath = "./src/TSPLIB95/" -- TODO: automatic path

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)

----------------------------------------------------------------------------
-- Types

type RealN = Scientific
type TextV = T.Text

type Coord2D = (RealN, RealN)
type Coord3D = (RealN, RealN, RealN)
data Coord = Coord2D Coord2D | Coord3D Coord3D
    deriving (Read, Show, Eq)

type Key = T.Text
data Value =  IntV Int
            | RealN RealN
            | ArrayV [Value]
            | TextV TextV
            | CoordSection [(Node, Coord)]
            | TourSection [[Node]]
            | EdgeDataSection [[Node]]
            deriving (Read, Show, Eq)

data TSP = TSP95 (M.Map Key Value)
    deriving Show

instance GTSP TSP where
    size tsp = let (IntV sz) = tsp ! kDIMENSION in sz
    nodes tsp = [1 .. size tsp]
    costFunc tsp = fromJust $ edgeWeightsFunc tsp

infixl 9 !
(!) :: TSP -> Key -> Value
(TSP95 tsp) ! k = tsp M.! k

lookup' :: Key -> TSP -> Maybe Value
lookup' k (TSP95 tsp) = M.lookup k tsp

insert :: Key -> Value -> TSP -> TSP
insert k v (TSP95 tsp) = TSP95 $ M.insert k v tsp

fromList :: [(Key, Value)] -> TSP
fromList xs = TSP95 $ M.fromList xs

emptyTSP :: TSP
emptyTSP = TSP95 M.empty

----------------------------------------------------------------------------
-- Constants defined by TSPLIB95

-- Specification section keys
kNAME = "NAME"
kTYPE = "TYPE"
kCOMMENT = "COMMENT"
kDIMENSION = "DIMENSION"
kCAPACITY = "CAPACITY"
kEDGE_WEIGHT_TYPE = "EDGE_WEIGHT_TYPE"
kEDGE_WEIGHT_FORMAT = "EDGE_WEIGHT_FORMAT"
kEDGE_DATA_FORMAT = "EDGE_DATA_FORMAT"
kNODE_COORD_TYPE = "NODE_COORD_TYPE"

specKeys :: [Key]
specKeys = [kNAME, kTYPE, kCOMMENT, kDIMENSION, kCAPACITY, kEDGE_WEIGHT_TYPE,
            kEDGE_WEIGHT_FORMAT, kEDGE_DATA_FORMAT, kNODE_COORD_TYPE]

-- Data section keys
kNODE_COORD_SECTION = "NODE_COORD_SECTION"
kDEPOT_SECTION = "DEPOT_SECTION"
kDEMAND_SECTION = "DEMAND_SECTION"
kEDGE_DATA_SECTION = "EDGE_DATA_SECTION"
kFIXED_EDGES_SECTION = "FIXED_EDGES_SECTION"
kDISPLAY_DATA_SECTION = "DISPLAY_DATA_SECTION"
kTOUR_SECTION = "TOUR_SECTION"
kEDGE_WEIGHT_SECTION = "EDGE_WEIGHT_SECTION"

dataKeys :: [Key]
dataKeys = [kNODE_COORD_SECTION, kDEPOT_SECTION, kDEMAND_SECTION, kEDGE_DATA_SECTION,
            kFIXED_EDGES_SECTION, kDISPLAY_DATA_SECTION, kTOUR_SECTION, kEDGE_WEIGHT_SECTION]

keys :: [Key]
keys = specKeys ++ dataKeys

----------------------------------------------------------------------------
-- Functions defined by TSPLIB95

euc :: Coord -> Coord -> Double
euc (Coord2D (x, y)) (Coord2D (x', y')) =
    let
        xd = x - x'
        yd = y - y'
    in sqrt $ toRealFloat $ (xd * xd) + (yd * yd)

----------------------------------------------------------------------------
-- Parser

type Parser = Parsec Void T.Text

type SParser = StateT TSP Parser
----------------------------------------------------------------------------
-- Primitive parsers

none :: Parser ()
none = return ()

eol':: Parser ()
eol' = try eol >> none

-- followedBy p parses if p would parse. Doesn't consume any input.
followedBy :: Parser a -> Parser a
followedBy p = lookAhead $ try p

----------------------------------------------------------------------------
-- Lexer stuff

-- | @'sc' consumes space
sc :: Parser ()
sc = L.space space1 empty empty

-- | @'lexeme' parses a lexeme, assumes no space before the lexeme and
-- consumes all space after.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

----------------------------------------------------------------------------
-- Symbols

eofSym :: Parser ()
eofSym = symbol "EOF" *> none

listTerm :: Parser ()
listTerm = symbol "-1" *> none

colonSym :: Parser ()
colonSym = symbol ":" *> none

endOfTSP :: Parser ()
endOfTSP = many eofSym *> eof <?> "end of input"
----------------------------------------------------------------------------
-- Text parsers

oneOfString :: [T.Text] -> Parser T.Text
oneOfString = choice . map (try . string)

nonSpace :: Parser (Token T.Text)
nonSpace = notFollowedBy spaceChar *> anySingle

safeReadP :: (Read a) => String -> Parser a
safeReadP x =   let fails = (fail $ "Failed to read value: \""++ x ++"\"")
                in  maybe fails return (readMaybe x)

read' :: (Read a, Show a) => Parser a
read' =  some nonSpace >>= safeReadP

----------------------------------------------------------------------------
-- Value parsers

endOfValue = eol' *> endOfField

intP :: Parser Int
intP = lexeme read'

floatP :: Parser RealN
floatP = L.signed sc (lexeme L.scientific)

list1D :: Parser a -> Parser [a]
list1D v = manyTill v listTerm

list2D :: Parser a -> Parser [[a]]
list2D v = manyTill (list1D v) (listTerm <|> endOfField)

-- Reads everything until EOL (but does not consume EOL)
strValue :: Parser Value
strValue = TextV . T.pack <$> lexeme (manyTill anySingle endOfValue)

----------------------------------------------------------------------------
-- Field parsers

failMissingDep a b = fail $ "Cannot parse \""++a++"\" without knowing \""++b++"\"."
failMissingDep' b = fail $ "Cannot parse this without knowing \""++b++"\"."

anyKey = lexeme (oneOfString keys)

-- endOfField only fails if there are more tokens to be parsed within the current field.
-- Never consumes any tokens. Does not expect a newline before the next field key.
endOfField :: Parser ()
endOfField = followedBy (endOfTSP <|> anyKey *> none) <?> "end of field"

env :: Key -> SParser Value
env k = do
    let fl = failMissingDep' (T.unpack k)
    mv <- gets (lookup' k)
    maybe fl return mv

insert' :: Key -> Value -> SParser ()
insert' k v = do
    modify (insert k v)

edgeWeightType :: SParser Value
edgeWeightType = do
    let weightToCoordType (TextV t) =
            TextV $ if T.isSuffixOf "3D" t then "THREED_COORDS" else "TWOD_COORDS"
    v <- lift strValue
    insert' kNODE_COORD_TYPE (weightToCoordType v)
    return v

nodeSectionMap :: [(Int, Coord)] -> M.Map Int Coord
nodeSectionMap = M.fromList

edgeWeightsFunc :: TSP -> Maybe ((Int, Int) -> Double)
edgeWeightsFunc tsp = do
    (CoordSection ns) <- lookup' kNODE_COORD_SECTION tsp
    let
        nm = M.fromList ns
        coord n = nm M.! n
    (TextV weightType) <- lookup' kEDGE_WEIGHT_TYPE tsp
    if T.isPrefixOf "EUC" weightType
        then Just (\(x,y) -> euc (coord x) (coord y))
        else Nothing

nodeCoordType :: SParser Value
nodeCoordType = do
    let
        fl = fail $ "Multiple definitions, possibly inferred, for \""
                    ++ show kNODE_COORD_TYPE ++ "\"."
    v <- lift strValue
    mv <- gets (lookup' kNODE_COORD_TYPE)
    maybe fl return (mv <|> Just v >>= ensure (==v))

nodeCoordSection :: SParser Value
nodeCoordSection = do
    (IntV n) <- env kDIMENSION
    (TextV t) <- env kNODE_COORD_TYPE
    let coordP = case t of
            "TWOD_COORDS"   -> Coord2D <$> ((,) <$> floatP <*> floatP)
            "THREED_COORDS" -> Coord3D <$> ((,,) <$> floatP <*> floatP <*> floatP)
            "NO_COORDS"     -> failMissingDep "NODE_COORD_SECTION" "NODE_COORD_TYPE"
    lift $ CoordSection <$> count n ((,) <$> intP <*> coordP)

tourSection :: SParser Value
tourSection = lift $ TourSection <$> list2D intP

edgeDataSection :: SParser Value
edgeDataSection = lift $ EdgeDataSection <$> list2D intP

keyParserMap :: M.Map Key (SParser Value)
keyParserMap = M.fromList
                    [(kNAME, lift strValue),
                    (kTYPE, lift strValue),
                    (kCOMMENT, lift strValue),
                    (kDIMENSION, lift $ IntV <$> intP),
                    (kNODE_COORD_TYPE, nodeCoordType),
                    (kEDGE_WEIGHT_TYPE, edgeWeightType),
                    (kNODE_COORD_SECTION, nodeCoordSection),
                    (kEDGE_DATA_SECTION, edgeDataSection),
                    (kTOUR_SECTION, tourSection)]

-- | @'key' k is a lexeme followed by an optional colon symbol.
fieldKey :: SParser Key
fieldKey =  lift (anyKey <* optional colonSym <?> "keyword")

fieldValue :: Key -> SParser Value
fieldValue k = fromMaybe (fail "Unsupported key.") (M.lookup k keyParserMap)

field :: SParser (Key, Value)
field = do
    k <- fieldKey
    v <- fieldValue k
    insert' k v
    lift endOfField
    return (k, v)

fieldsParser :: SParser TSP
fieldsParser = fromList <$> manyTill field (lift endOfTSP)

showError :: ParseErrorBundle T.Text Void -> String
showError = errorBundlePretty

----------------------------------------------------------------------------
-- High-level parsers

tspParser :: Parser TSP
tspParser = execStateT fieldsParser emptyTSP

parseTSP :: String -> String -> Either (ParseErrorBundle T.Text Void) TSP
parseTSP filePath contents = parse tspParser filePath (T.pack contents)

readEitherTSP :: String -> IO (Either (ParseErrorBundle T.Text Void) TSP)
readEitherTSP filePath = parseTSP filePath <$> readFile filePath

readMaybeTSP :: String -> IO (Maybe TSP)
readMaybeTSP filePath = either (const Nothing) Just <$> readEitherTSP filePath
