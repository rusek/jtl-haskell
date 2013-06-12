{-# Language ExplicitForAll, RankNTypes, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts #-}

module JTL.Runner (evaluate) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class hiding (Error)
import Control.Monad.Reader.Class

import Data.Maybe (mapMaybe, catMaybes)
import qualified JTL.Env as E
import JTL.IR
import qualified JTL.Context as C
import qualified JTL.Value as V
import JTL.Sequence
import JTL.Utils (liftR, liftR2)

type Error = String
type ContextSequence = Sequence Error C.Context

newtype Comp a = Comp { runComp :: forall b. (a -> b) -> (Error -> b) -> b }

runCompEither c = runComp c Right Left

instance Monad Comp where
    return x = Comp $ \cont error -> cont x
    m >>= k = Comp $ \cont error -> runComp m (\x -> (runComp $ k x) cont error) error

instance MonadError Error Comp where
    throwError e = Comp $ \cont error -> error e
    catchError m h = Comp $ \cont error -> runComp m cont (\e -> runComp (h e) cont error)

instance Fail Error Comp where
    runFailCont = runComp

newtype Runner a = Runner { runRunner :: E.Env -> Comp a }

instance Functor Runner where
    fmap = liftM

instance Monad Runner where
    return x = Runner $ \env -> return x
    m >>= k = Runner $ \env -> runRunner m env >>= \x -> runRunner (k x) env
    fail msg = Runner $ \env -> fail msg

instance MonadReader E.Env Runner where
    ask = Runner $ \env -> return env
    local f m = Runner $ \env -> runRunner m $ f env

instance MonadError Error Runner where
    throwError e = Runner $ \env -> throwError e
    catchError m h = Runner $ \env -> runRunner m env `catchError` \e -> runRunner (h e) env

evaluate :: Expr -> E.Env -> Either Error [C.Context]
evaluate expr env = runCompEither $ runRunner (runList expr) env

makeComp1 :: (a -> Runner b) -> Runner (a -> Comp b)
makeComp1 f = do
    env <- ask
    return $ \x -> (runRunner $ f x) env

makeComp2 :: (a -> b -> Runner c) -> Runner (a -> b -> Comp c)
makeComp2 f = do
    env <- ask
    return $ \x y -> (runRunner $ f x y) env

makeSequence1 :: (a -> Runner ContextSequence) -> Runner (a -> ContextSequence)
makeSequence1 f = do
    env <- ask
    return $ \x -> failS $ runRunner (f x) env

makeSequenceMaybe1 :: (a -> Runner (Maybe C.Context)) -> Runner (a -> ContextSequence)
makeSequenceMaybe1 = undefined

membersS :: ContextSequence -> ContextSequence
membersS = (=<<) (\ctx -> listS $ map ((flip $ uncurry C.withMember) ctx) $ V.members $ C.getValue ctx)

pickSM :: ContextSequence -> (C.Context -> Runner Bool) -> Runner ContextSequence
pickSM s f = makeComp1 f >>= \f' -> return $ f' `filterS` s

runContext :: Expr -> Runner (Maybe C.Context)
runContext (EValue v) = return $ Just $ C.fromValue v
runContext EDocument = asks $ Just . E.getDocument
runContext EContext = asks $ Just . E.getContext
runContext (EVar v) = asks (lookup v) >>= go where
    go Nothing = throwError "Missing variable"
    go x = return x
    lookup (VNamed n) = E.lookupNamedVar n
    lookup (VIndexed i) = E.lookupIndexedVar i
runContext (EArray exprs) = mapM runValue exprs >>= \vs ->
    return $ Just $ C.fromValue $ catMaybes vs
runContext (EObject members) = mapM runObjectMember members >>= \vs ->
    return $ Just $ C.fromValue $ V.toObject $ catMaybes vs
runContext (EUnOp op expr) = runValue expr >>= \v -> fmap C.fromValue `liftM` go op v where
    go :: UnOp -> Maybe V.Value -> Runner (Maybe V.Value)
    go ONot v = (Just . V.VBoolean . V.toBoolean) `liftM` castMaybeToBool v
    go ONeg (Just (V.VNumber n)) = return $ Just $ V.VNumber (-n)
    go ONeg Nothing = return Nothing
    go ONeg _ = throwError "Expecting number"
runContext (ECall "error" [expr]) = runContext expr >>= throwError . show
runContext (ECall _ _) = throwError "Invalid function"
runContext (EBinOp OAnd left right) = do
    x <- runContext left
    b <- castMaybeToBool $ fmap C.getValue x
    if b then runContext right else return x
runContext (EBinOp OOr left right) = do
    x <- runContext left
    b <- castMaybeToBool $ fmap C.getValue x
    if b then return x else runContext right
runContext (EBinOp op left right) = do
    x <- runValue left
    y <- runValue right
    fmap C.fromValue `liftM` go op x y
    where
    go OAdd = addMaybe
    go OSub = subtractMaybe
    go OMul = multiplyMaybe
    go ODiv = divideMaybe
    go OMod = moduloMaybe
runContext (ECmpOp left rights) = runValue left >>= \x -> go x rights where
    go _ [] = return $ Just $ C.fromValue $ V.VBoolean V.true
    go leftVal ((op, right):rights) = runValue right >>= \rightVal ->
        if goOp op leftVal rightVal then go rightVal rights
        else return $ Just $ C.fromValue $ V.VBoolean V.false
    goOp op = case op of
        OEq -> goO True False (==)
        ONe -> goO False True (/=)
        OLt -> goO False False (<)
        OLe -> goO True False (<=)
        OGt -> goO False False (>)
        OGe -> goO True False (>=)
    goO fb2 fb1 op Nothing Nothing = fb2
    goO fb2 fb1 op Nothing _ = fb1
    goO fb2 fb1 op _ Nothing = fb1
    goO fb2 fb1 op (Just x) (Just y) = x `op` y
runContext e = runContexts e >>= takeSM 2 >>= \items -> case items of
    [] -> return Nothing
    [x] -> return $ Just x
    _ -> throwError "Multiple values returned"

runContexts :: Expr -> Runner ContextSequence
runContexts (ESequence es) = joinS <$> mapM runContexts es
runContexts (ETrans "members" src []) = membersS <$> runContexts src
runContexts (ETrans "members" src [expr]) = do
    it <- runContexts src
    pickSM (membersS it) $ \x -> runValueWith x expr >>= \x' -> case x' of
        Just (V.VString _) -> return $ x' == C.lookupKey x
        Just (V.VNumber _) -> return $ x' == C.lookupKey x
        _ -> castMaybeToBool x'
runContexts (ETrans "first" src []) = truncateS 1 <$> runContexts src
runContexts (ETrans "first" src [expr]) = maybeS <$> (runContexts src >>= findSM (`runBoolWith` expr))
runContexts (ETrans "last" src []) = runFold1S (\x y -> return y) src
runContexts (ETrans "last" src [expr]) = fmap maybeS $ runContexts src >>= (findSM (`runBoolWith` expr) . reverseS)
runContexts (ETrans "count" src []) = runList src >>= (\xs -> return $ singletonS $ C.fromValue $ length xs)
runContexts (ETrans "count" src [expr]) = do
    it <- runContexts src
    n <- foldSM (\x y -> runBoolWith y expr >>= \b -> return $ if b then x + 1 else x) (0 :: Int) it
    return $ singletonS $ C.fromValue $ n
runContexts (ETrans "min" src []) = runFold1S (\x y -> return $ if C.getValue y < C.getValue x then y else x) src
runContexts (ETrans "max" src []) = runFold1S (\x y -> return $ if C.getValue y > C.getValue x then y else x) src
runContexts (ETrans "sum" src []) = runFoldSValue (\x y -> add x $ C.getValue y) (V.VNumber V.zero) src
runContexts (ETrans "avg" src []) = do
    it <- runContexts src
    (sum, count) <- foldSM (\(sum, count) ctx -> add sum ctx >>= \sum' -> return (sum', count + 1))
        (V.VNumber V.zero, 0 :: Int) (fmap C.getValue it)
    avg <- if count == 0 then return $ V.VNumber V.zero else divide sum (V.VNumber $ V.toNumber count)
    return . singletonS . C.fromValue $ avg
runContexts (ETrans "array" src []) = runList src >>= \xs -> return $ singletonS $ C.fromValue $ map C.getValue xs
runContexts (ETrans "array" src [expr]) = do
    s <- runContexts src
    vs <- mapSM (`runValueWith` expr) s
    return $ singletonS $ C.fromValue $ catMaybes vs
runContexts (ETrans "object" src []) = do
    s <- runContexts src
    ms <- mapSM (\ctx -> case C.lookupKey ctx of
        Nothing -> return Nothing
        Just k -> castToString k >>= \k -> return (Just (k, C.getValue ctx))) s
    return $ singletonS $ C.fromValue $ V.toObject $ catMaybes ms
runContexts (ETrans "object" src [expr]) = do
    s <- runContexts src
    ms <- mapSM (\ctx -> case C.lookupKey ctx of
        Nothing -> return Nothing
        Just k -> castToString k >>= \k -> runValueWith ctx expr >>= \v -> case v of
            Nothing -> return Nothing
            Just v -> return $ Just (k, v)) s
    return $ singletonS $ C.fromValue $ V.toObject $ catMaybes ms
runContexts (ETrans "object" src [key, value]) = do
    members <- runContexts src >>= mapSM (`runObjectMemberWith` (key, value))
    return $ singletonS $ C.fromValue $ V.toObject $ catMaybes members
runContexts (ETrans "select" src args) = error "Not implemented yet"
runContexts (ETrans "reverse" src []) = reverseS <$> runContexts src

runContexts (ETrans "filter" src [expr]) = runContexts src >>= (`pickSM` (`runBoolWith` expr))
runContexts (ETrans "map" src [expr]) = liftM2 (>>=)
    (runContexts src)
    (makeSequence1 $ \x -> fmap (maybeS . fmap (`C.withValue` x)) $ runValueWith x expr)

runContexts (ETrans _ _ _) = throwError "Invalid transformation"
runContexts (EIf test ifTrue ifFalse) = runBool test >>= \b -> runContexts (if b then ifTrue else ifFalse)
runContexts (ELet v e1 e2) = runContext e1 >>= \ctx -> case ctx of
    Nothing -> return emptyS
    Just ctx -> local (withVar v ctx) (runContexts e2)
runContexts e = runContext e >>= go where
    go Nothing = return emptyS
    go (Just ctx) = return $ singletonS ctx

runValue :: Expr -> Runner (Maybe V.Value)
runValue e = fmap C.getValue `liftM` runContext e

runValueWith :: C.Context -> Expr -> Runner (Maybe V.Value)
runValueWith ctx = local (E.withContext ctx) . runValue

runBool :: Expr -> Runner Bool
runBool e = runValue e >>= castMaybeToBool

runBoolWith :: C.Context -> Expr -> Runner Bool
runBoolWith ctx = local (E.withContext ctx) . runBool

runList :: Expr -> Runner [C.Context]
runList e = runContexts e >>= unwrapSM

runObjectMember :: (Expr, Expr) -> Runner (Maybe (V.String, V.Value))
runObjectMember (k, v) = runValue k >>= \k -> case k of
    Nothing -> return Nothing
    Just k -> castToString k >>= \k -> runValue v >>= \v -> case v of
        Nothing -> return Nothing
        Just v -> return $ Just (k, v)

runObjectMemberWith :: C.Context -> (Expr, Expr) -> Runner (Maybe (V.String, V.Value))
runObjectMemberWith ctx = local (E.withContext ctx) . runObjectMember

runFold1S :: (C.Context -> C.Context -> Runner C.Context) -> Expr -> Runner ContextSequence -- TODO poprawić
runFold1S f e = runContexts e >>= fold1SM f >>= liftR maybeS

runFoldSValue :: (V.Value -> C.Context -> Runner V.Value) -> V.Value -> Expr -> Runner ContextSequence -- TODO poprawić
runFoldSValue f x e = runContexts e >>= foldSM f x >>= return . singletonS . C.fromValue

castMaybeToBool :: Maybe V.Value -> Runner Bool
castMaybeToBool Nothing = return False
castMaybeToBool (Just v) = castToBool v

castToBool :: V.Value -> Runner Bool
castToBool V.VNull = return False
castToBool (V.VBoolean b) = return $ V.fromBoolean b
castToBool (V.VNumber n) = return $ n /= V.zero
castToBool (V.VString s) = return $ s /= V.epsilon
castToBool (V.VArray _) = throwError "Cannot convert array to boolean"
castToBool (V.VObject _) = throwError "Cannot convert object to boolean"

castToString :: MonadError Error m => V.Value -> m V.String
castToString V.VNull = return $ V.toString $ show V.VNull
castToString (V.VBoolean b) = return $ V.toString $ show b
castToString (V.VNumber n) = return $ V.toString $ show n
castToString (V.VString s) = return s
castToString (V.VArray _) = throwError "Cannot convert array to string"
castToString (V.VObject _) = throwError "Cannot convert object to string"

add :: MonadError Error m => V.Value -> V.Value -> m V.Value
add (V.VString x) (V.VString y) = return $ V.VString $ V.concat x y
add (V.VNumber x) (V.VNumber y) = return $ V.VNumber $ x + y
add _ _ = throwError "Expecting string or number"

addMaybe :: MonadError Error m => Maybe V.Value -> Maybe V.Value -> m (Maybe V.Value)
addMaybe (Just x) (Just y) = Just `liftM` add x y
addMaybe (Just (V.VString _)) Nothing = return Nothing
addMaybe (Just (V.VNumber _)) Nothing = return Nothing
addMaybe Nothing (Just (V.VString _)) = return Nothing
addMaybe Nothing (Just (V.VNumber _)) = return Nothing
addMaybe Nothing Nothing = return Nothing
addMaybe _ _ = throwError "Expecting string or number"

numberBinOp :: MonadError Error m => (V.Number -> V.Number -> m V.Number) -> V.Value -> V.Value -> m V.Value
numberBinOp f (V.VNumber x) (V.VNumber y) = f x y >>= \z -> return $ V.VNumber z
numberBinOp f _ _ = throwError "Expecting number"

numberBinOpMaybe :: MonadError Error m => (V.Number -> V.Number -> m V.Number) -> Maybe V.Value -> Maybe V.Value ->
                                          m (Maybe V.Value)
numberBinOpMaybe f (Just x) (Just y) = Just `liftM` numberBinOp f x y
numberBinOpMaybe f (Just (V.VNumber _)) Nothing = return Nothing
numberBinOpMaybe f Nothing (Just (V.VNumber _)) = return Nothing
numberBinOpMaybe f Nothing Nothing = return Nothing
numberBinOpMaybe f _ _ = throwError "Expecting number"

subtractMaybe :: MonadError Error m => Maybe V.Value -> Maybe V.Value -> m (Maybe V.Value)
subtractMaybe = numberBinOpMaybe $ liftR2 (-)

multiplyMaybe :: MonadError Error m => Maybe V.Value -> Maybe V.Value -> m (Maybe V.Value)
multiplyMaybe = numberBinOpMaybe $ liftR2 (*)

divideNumber :: MonadError Error m => V.Number -> V.Number -> m V.Number
divideNumber x y
    | y == V.zero = throwError "Zero division"
    | otherwise   = return $ x / y

divide :: MonadError Error m => V.Value -> V.Value -> m V.Value
divide = numberBinOp divideNumber

divideMaybe :: MonadError Error m => Maybe V.Value -> Maybe V.Value -> m (Maybe V.Value)
divideMaybe = numberBinOpMaybe divideNumber

moduloMaybe :: MonadError Error m => Maybe V.Value -> Maybe V.Value -> m (Maybe V.Value)
moduloMaybe = numberBinOpMaybe (\x y -> if y == V.zero then throwError "Zero modulo" else return $ x `V.mod` y)

withVar :: Var -> C.Context -> E.Env -> E.Env
withVar (VNamed name) = E.withNamedVar name
withVar (VIndexed index) = E.withIndexedVar index
