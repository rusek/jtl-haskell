{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module JTL.Parsec (lexer, alpha, alphaNum, uint, digit, name, var, string, unumber, white, decode) where

import Data.Bits
import Data.Char (digitToInt, isControl)
import Data.Ratio
import Text.Parsec hiding (string, (<|>), many, token, alphaNum)
import qualified Text.Parsec as P
import Text.Parsec.String ( Parser )
import Control.Applicative hiding (optional)
import Control.Monad
import JTL.IR (Var(..))
import qualified JTL.Value as V
import JTL.Token

alpha :: Parser Char
alpha = satisfy (\c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_' || c == '$')

alphaNum :: Parser Char
alphaNum = alpha <|> digit

uint0' :: Parser Integer
uint0' = foldl (\x y -> 10 * x + toInteger (digitToInt y)) 0 <$> many1 digit

uint' :: Parser Integer
uint' = 0 <$ char '0' <|> uint0'

uint :: Parser Integer
uint = uint' <* notFollowedBy alpha

name :: Parser String
name = liftM2 (:) alpha (many alphaNum)

var' :: (String -> a) -> (Int -> a) -> Parser a
var' f g = f <$> name <|> (g . fromInteger) <$> uint

var :: Parser Var
var = char '@' *> var' VNamed VIndexed

isHighSurrogate :: Int -> Bool
isHighSurrogate x = 0xD800 <= x && x <= 0xDBFF

isLowSurrogate :: Int -> Bool
isLowSurrogate x = 0xDC00 <= x && x <= 0xDFFF

fromSurrogatePair :: Int -> Int -> Char
fromSurrogatePair high low = toEnum $ 0x10000 + (((high - 0xd800) `shift` 10) .|. (low - 0xdc00))

baseStringEsc :: Parser Char
baseStringEsc =
    '"'  <$ char '"'  <|>
    '\\' <$ char '\\' <|>
    '/'  <$ char '/'  <|>
    '\b' <$ char 'b'  <|>
    '\f' <$ char 'f'  <|>
    '\n' <$ char 'n'  <|>
    '\r' <$ char 'r'  <|>
    '\t' <$ char 't'  <|>
    char 'u' *> escUni
    where
    escUni = do
        x <- hex4
        if isHighSurrogate x then do
            y <- str "\\u" *> hex4
            if not $ isLowSurrogate y then
                fail "Expecting low surrogate"
            else
                return $ fromSurrogatePair x y
        else if isLowSurrogate x then
            unexpected "low surrogate"
        else
            return $ toEnum x
    hex4 = foldl (\x y -> x * 16 + y) 0 <$> count 4 (digitToInt <$> hexDigit)

stringEsc :: Parser Char
stringEsc = '\'' <$ char '\'' <|> baseStringEsc

stringChar' :: Parser Char -> Char -> Parser Char
stringChar' esc delim = (char '\\' *> esc) <|> satisfy (\c -> not (isControl c || c == delim))

string' :: Parser Char -> Char -> Parser V.String
string' esc delim = V.toString <$> between (char delim) (char delim) (many $ stringChar' esc delim)

string :: Parser V.String
string = string' stringEsc '"' <|> string' stringEsc '\''

baseString :: Parser V.String
baseString = string' baseStringEsc '"'

unumber :: Parser V.Number
unumber = do
    n <- toRational <$> uint'
    f <- (try (char '.' <* lookAhead digit) *> (prepF <$> many1 digit)) <|> pure (toRational 0)
    e <- oneOf "eE" *> ((negE <$ char '-' <|> posE <$ char '+' <|> pure posE) <*> uint0') <|> pure (toRational 1)
    notFollowedBy alpha
    return $ V.toNumber $ (n + f) * e
    
    where
        posE v = toRational $ 10 ^ v
        negE v = 1 % (10 ^ v)
        prepF ds = read ds % (10 ^ length ds)

baseNumber :: Parser V.Number
baseNumber = (negate <$ char '-' <|> pure id) *> unumber

white :: Parser ()
white = (space *> spaces) <|>
        (str "/*" >> skipUntil (str "*/")) <|>
        (str "//" >> skipUntil (eof <|> () <$ newline))

baseWs :: Parser ()
baseWs = skipMany $ oneOf "\x20\x09\x0A\x0D"

baseStruct :: Char -> Char -> Parser a -> Parser [a]
baseStruct l r p = between (char l) (char r) (baseWs *> ((p <* baseWs) `sepBy` (char ',' >> baseWs)))

baseArray :: Parser V.Array
baseArray = V.toArray <$> baseStruct '[' ']' baseValue

baseObject :: Parser V.Object
baseObject = V.toObject <$> baseStruct '{' '}' member where
    member = do
        k <- baseString
        baseWs >> char ':' >> baseWs
        v <- baseValue
        return (k, v)

baseValue :: Parser V.Value
baseValue =
    V.VNull <$ str "null" <|>
    V.VBoolean V.false <$ str "false" <|>
    V.VBoolean V.true <$ str "true" <|>
    V.VNumber <$> baseNumber <|>
    V.VString <$> baseString <|>
    V.VArray <$> baseArray <|>
    V.VObject <$> baseObject

decode :: V.ValueLike a => String -> Either String a
decode s = case runParser (baseWs *> baseValue <* baseWs) () "" s of
    Left e -> Left $ show e
    Right v -> V.tryFromValue v

refT :: Parser Token
refT = char '@' *> (var' TNamedVar TIndexedVar <|> return TContextRef)

nameT :: Parser Token
nameT = go <$> name where
    go "if"    = TIf
    go "then"  = TThen
    go "else"  = TElse
    go "let"   = TLet
    go "in"    = TIn
    go "not"   = TNot
    go "or"    = TOr
    go "and"   = TAnd
    go "null"  = TNull
    go "true"  = TTrue
    go "false" = TFalse
    go n       = TIdent n

symbolT :: Parser Token
symbolT = TLessEqual    <$ str "<=" <|>
          TGreaterEqual <$ str ">=" <|>
          TEqual        <$ str "==" <|>
          TNotEqual     <$ str "!=" <|>
          TDoubleColon  <$ str "::" <|>
          TDoubleBar    <$ str "||" <|>
          TDoubleAmp    <$ str "&&" <|>
          TPlus         <$ char '+' <|>
          TMinus        <$ char '-' <|>
          TStar         <$ char '*' <|>
          TSlash        <$ char '/' <|>
          TPercent      <$ char '%' <|>
          TLess         <$ char '<' <|>
          TGreater      <$ char '>' <|>
          TExclMark     <$ char '!' <|>
          TQuestMark    <$ char '?' <|>
          TAssign       <$ char '=' <|>
          TLeftParen    <$ char '(' <|>
          TRightParen   <$ char ')' <|>
          TLeftBrace    <$ char '{' <|>
          TRightBrace   <$ char '}' <|>
          TLeftBracket  <$ char '[' <|>
          TRightBracket <$ char ']' <|>
          TColon        <$ char ':' <|>
          TComma        <$ char ',' <|>
          TDot          <$ char '.'

token :: Parser Token
token = TString <$> string <|>
        TNumber <$> unumber <|>
        nameT <|>
        refT <|>
        symbolT

lexer :: (Token -> Parser a) -> Parser a
lexer cont = white *> lexer cont <|> (token >>= cont) <|> (eof >> cont TEOF)

str :: String -> Parser String
str = try . P.string

skipUntil :: Parser a -> Parser ()
skipUntil p = let go = () <$ p <|> anyChar *> go in go
