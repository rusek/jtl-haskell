{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Main where

import System.Console.Haskeline
import System.Console.Haskeline.MonadException
import System.Directory
import System.FilePath
import Control.Monad hiding (join)
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (join)
import Control.Exception hiding (catch)
import Control.Applicative hiding ((<|>), many)
import System.IO
import System.IO.Error
import System.Environment
import JTL.IR
import qualified JTL.Env as E
import qualified JTL.Value as V
import qualified JTL.Context as C
import qualified JTL.Runner as R
import Text.Parsec
import Text.Parsec.String (Parser)
import JTL.Parsec
import qualified JTL.Parser as P
import JTL.Utils (join)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

type Interpreter a = InputT (StateT E.Env IO) a

data Cmd = CLoad (Maybe Var) String | CSet (Maybe Var) String | CExplain String | CRun String | CQuit | CNone

wordBreakChars :: String
wordBreakChars = " \t\n\"\\'`@$><=;|&{("

str :: String -> Parser String
str = Text.Parsec.try . Text.Parsec.string

word :: Parser String
word = concat <$> many (squot <|> dquot <|> esc <|> chars) where
    squot = char '\'' *> many (satisfy (/= '\'')) <* char '\''
    dquot = char '"' *> many ('\\' <$ str "\\\\" <|> '"' <$ str "\\\"" <|> satisfy (/= '"')) <* char '"'
    chars = many1 $ satisfy $ not . (`elem` wordBreakChars)
    esc = return <$> (char '\\' *> anyChar)

cmdSpec :: Parser Cmd
cmdSpec = do
    n <- name
    if n == "q" || n == "quit" then
        spaces *> return CQuit
    else if n == "l" || n == "load" then do
        v <- spaces *> (Just <$> (var <* spaces) <|> return Nothing)
        path <- word <* spaces <* eof
        return $ CLoad v path
    else if n == "e" || n == "explain" then
        CExplain <$> many anyChar
    else if n == "s" || n == "set" then do
        v <- spaces *> (Just <$> (var <* spaces) <|> return Nothing)
        expr <- many anyChar
        return $ CSet v expr
    else
        fail $ "Unrecognized command " ++ n

cmdRun :: Parser Cmd
cmdRun = CRun <$> many1 anyChar

cmd :: Parser Cmd
cmd = spaces *> (char ':' *> cmdSpec <|> cmdRun <|> return CNone)

instance MonadState s m => MonadState s (InputT m) where
    get = lift get
    put = lift . put

parseExpr :: String -> (Expr -> Interpreter ()) -> Interpreter ()
parseExpr src act = case P.parse src of
    Right expr -> act expr
    Left msg -> outputStrLn msg

evaluateExpr :: String -> ([C.Context] -> Interpreter ()) -> Interpreter ()
evaluateExpr src act = parseExpr src $ \expr -> get >>= \env -> case R.evaluate expr env of
    Right ctxs -> act ctxs
    Left msg -> outputStrLn msg

loadValueIO :: String -> IO (Either String V.Value)
loadValueIO path = withFile path ReadMode process `catch` (return . Left . (show :: IOError -> String))
    where
        process h = do
        txt <- hGetContents h
        case decode txt of
            Left msg -> return $ Left msg
            Right v -> return $ Right v

loadValue :: String -> (V.Value -> Interpreter ()) -> Interpreter ()
loadValue path act = liftIO (loadValueIO path) >>= \result -> case result of
    Left msg -> outputStrLn msg
    Right v -> act v

fixPathIO :: FilePath -> IO FilePath
fixPathIO s = case trim s of
    "~" -> getHomeDirectory
    '~':c:cs | isPathSeparator c -> liftM (</> cs) getHomeDirectory
    cs -> return cs

trim :: String -> String
trim = let go = reverse . dropWhile isSpace in go . go

main :: IO ()
main = void $ runStateT (runInputT sett $ init >> loop) (E.fromDocument V.VNull) where
    sett = defaultSettings{historyFile = Just ".jtli-history"}
    init :: Interpreter ()
    init = liftIO getArgs >>= \args -> case args of
        [] -> return ()
        [path] -> loadValue path (put . E.fromDocument)
        _ -> outputStrLn "Invalid arguments"
    loop :: Interpreter ()
    loop = do
        inp <- getInputLine "> "
        case runParser cmd () "" $ fromMaybe "" inp of
            Left e -> outputStrLn (show e) >> loop
            Right CQuit -> return ()
            Right CNone -> loop
            Right (CLoad var path) -> liftIO (fixPathIO path) >>= \path -> loadValue path (\v -> case var of
                Nothing -> put $ E.fromDocument v
                Just (VNamed n) -> modify $ E.withNamedVar n $ C.fromValue v
                Just (VIndexed i) -> modify $ E.withIndexedVar i $ C.fromValue v) >> loop
            Right (CSet var expr) -> evaluateExpr expr (\ctxs -> case ctxs of
                [] -> outputStrLn "No value returned"
                [c] -> case var of
                    Nothing -> put $ E.fromDocument $ C.getValue c
                    Just (VNamed n) -> modify $ E.withNamedVar n c
                    Just (VIndexed i) -> modify $ E.withIndexedVar i c
                _ -> outputStrLn "Multiple values returned") >> loop
            Right (CExplain e) -> parseExpr e (outputStrLn . show) >> loop
            Right (CRun w) -> do
                evaluateExpr w $ \ctxs -> outputStrLn $ if null ctxs
                    then "undefined"
                    else join ", " $ map (showValue . C.getValue) ctxs
                loop

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving Enum

withColor :: Color -> String -> String
withColor c s = "\x1b[" ++ show (30 + fromEnum c) ++ "m" ++ s ++ "\x1b[0m"

showValue :: V.Value -> String
showValue (V.VArray a) = "[" ++ join ", " (map showValue $ V.fromArray a) ++ "]"
showValue (V.VObject o) = "{" ++ join ", " (map showMember $ V.fromObject o) ++ "}" where
    showMember (k, v) = showValue (V.VString k) ++ ": " ++ showValue v
showValue (V.VNumber n) = withColor Yellow $ show n
showValue (V.VString s) = withColor Green $ show s
showValue v = withColor Red $ show v
