
import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Data.Char

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

char :: Char -> Parser ()
char c = do
    c <- satisfy (==c)
    return ()

string :: String -> Parser ()
string "" = return ()
string (c:cs) = char c >> string cs

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

sep :: Parser Char
sep = token $ sym ':'

-- nameKey = "NAME"
-- commentKey = "COMMENT"

-- data Specs = 
-- data Spec = Name {
    -- key :: String,
    -- value :: String
-- }
-- data EntryParser a = EntryParser {
    -- keyParser :: Parser String,
    -- valueParser :: Parser a
-- }

data Entry v = Entry {
    key :: String,
    value :: v
}

entryP :: String -> Parser v -> Parser v
entryP k p = string k >> sep >> token p

stringEntryP :: String -> Parser String
stringEntryP k = entryP k (many1 letter)

intEntryP :: String -> Parser Int
intEntryP k = entryP k int

nameP = stringEntryP "NAME"
dimensionP = intEntryP "DIMENSION"
typeP = stringEntryP "TYPE"

tspP = nameP <|> typeP

-- bigEntryP :: String -> Pars

main :: IO ()
main = do
    s <- getLine
    print (apply nameP s)



-- instance Functor Entry




-- name :: Parser String
-- name 

-- entry :: String -> (Parser a) -> Entry a

-- data TSPType = TSP | ATSP | SOP | HCP | CVRP | TOUR

-- data Entries = TSPLIB95 {
kNAME = "NAME"
type NAME = String
kTYPE = "TYPE"
type TYPE = String

type COMMENT = String
type DIMENSION = Int
type CAPACITY = Int
type EDGE_WEIGHT_TYPE = String
type EDGE_WEIGHT_FORMAT = String




-- }


-- data TSPLIB95 = TSPLIB95 Specs Data


-- tsplib95 :: Parser a
-- tsplib95 = spec >> data