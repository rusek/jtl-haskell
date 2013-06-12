module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Char
import System.Environment
import System.IO
import JTL.Parsec (decode)
import qualified JTL.Context as C
import qualified JTL.Value as V
import qualified JTL.Env as E
import qualified JTL.Parser as P
import qualified JTL.Runner as R

type TestRunner a = StateT Stats IO a

data Stats = Stats { totalTests :: Int, okTests :: Int }

lookupObject k = V.lookupObjectMember (V.toString k)

runQuery :: V.Value -> E.Env -> TestRunner (Either String [C.Context])
runQuery (V.VString src) env = case P.parse $ V.fromString src of
    Right expr -> return $ R.evaluate expr env
    Left msg -> return $ Left msg
runQuery _ _ = fail "Query must be a string"

processFile :: String -> TestRunner ()
processFile file = do
    testSet <- liftIO $ withFile file ReadMode $ \h -> liftIO (hGetContents h) >>= \c -> case decode c of
        Right v -> return v
        Left msg -> fail msg
    processTestSet 0 testSet E.empty

printStr :: String -> TestRunner ()
printStr = liftIO . putStr

printStrLn :: String -> TestRunner ()
printStrLn = liftIO . putStrLn

processTestSet :: Int -> V.Value -> E.Env -> TestRunner ()
processTestSet depth (V.VObject obj) env = do
    case lookupObject "label" obj of
        Just (V.VString label) -> putLabelPrefix >> printStrLn (V.fromString label)
        Just _ -> fail "Test set label must be a string"
        Nothing -> putLabelPrefix >> printStrLn "Unnamed test set"
    case lookupObject "testSets" obj of
        Just (V.VArray arr) -> forM_ (V.fromArray arr) $ \testSet -> processTestSet (depth + 1) testSet env'
        Just _ -> fail "Test subsets must be an array"
        Nothing -> return ()
    case lookupObject "tests" obj of
        Just (V.VArray arr) -> forM_ (V.fromArray arr) $ \test -> processTest test env'
        Just _ -> fail "Test set tests must be an array"
        Nothing -> return ()
    printStrLn ""
    where
        env' = setupEnv obj env
        putLabelPrefix = printStr (replicate (depth + 1) '=') >> printStr " "
processTestSet _ _ _ = fail "Test set must be an object"

processTest :: V.Value -> E.Env -> TestRunner ()
processTest (V.VObject obj) env = go (map (first V.fromString) (V.fromObject obj)) >> liftIO (hFlush stdout) where
    go (("assertTrue", q):_) = assertValue q (V.VBoolean V.true)
    go (("assertFalse", q):_) = assertValue q (V.VBoolean V.false)
    go (("assertError", q):_) = runQuery q env' >>= \result -> case result of
        Left _ -> putOk
        Right _ -> putError q "no error thrown"
    go (_:xs) = go xs
    go [] = fail "Could not found test method"
    
    env' = setupEnv obj env
    
    runJust query cont = runQuery query env' >>= \result -> case result of
        Left msg -> putError query $ "unexpected error" ++ msg
        Right [] -> putError query "no value returned"
        Right [ctx] -> cont ctx
        _ -> putError query "multiple values returned"
    
    runJustValue query cont = runJust query (cont . C.getValue)
    
    assertValue expr expected = runJustValue expr $ \actual ->
        if expected /= actual then
            putError expr $ "expecting " ++ show expected ++ ", but got " ++ show actual
        else putOk
    
    putError query msg = do
        printStrLn $ "\nIn " ++ show query ++ ": " ++ msg
        modify (\(Stats total ok) -> Stats (total + 1) ok)
    putOk = do
        printStr ". "
        modify (\(Stats total ok) -> Stats (total + 1) (ok + 1))
processTest _ _ = fail "Test must be an object"

setupEnv :: V.Object -> E.Env -> E.Env
setupEnv obj env = setupVariables . setupDocument $ env where
    setupDocument env = case V.lookupObjectMember (V.toString "document") obj of
        Nothing -> env
        Just v -> E.fromDocument v
    setupVariables env = case V.lookupObjectMember (V.toString "variables") obj of
        Just (V.VObject obj') -> foldl setupVariable env $ V.fromObject obj'
        _ -> env
    setupVariable env (k, v) =
        let k' = V.fromString k
            v' = C.fromValue v in
                if not (null k') && all isDigit k' then E.withIndexedVar (read k') v' env
                else E.withNamedVar k' v' env

main = unpackState $ do
    files <- liftIO getArgs
    mapM_ processFile files
    stats <- get
    printStrLn $ "Total tests run: " ++ show (totalTests stats) ++ " (" ++ show (okTests stats) ++ " ok / " ++
                 show (totalTests stats - okTests stats) ++ " errors)"
    
unpackState m = return . fst =<< runStateT m Stats{totalTests = 0, okTests = 0}
