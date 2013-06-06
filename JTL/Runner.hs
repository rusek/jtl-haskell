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
    m >>= k = Comp $ \cont error -> (runComp m) (\x -> (runComp $ k x) cont error) error

instance MonadError Error Comp where
    throwError e = Comp $ \cont error -> error e
    catchError m h = Comp $ \cont error -> (runComp m) cont (\e -> runComp (h e) cont error)

instance Fail Error Comp where
    runFailCont = runComp

newtype Runner a = Runner { runRunner :: E.Env -> Comp a }

instance Functor Runner where
    fmap f m = m >>= return . f

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

pickSM :: ContextSequence -> (C.Context -> Runner Bool) -> Runner ContextSequence
pickSM s f = makeComp1 f >>= \f' -> return $ f' `filterS` s

--forSM :: Sequence a -> (a -> Runner b) -> Runner [b]
--forSM = undefined

--liftComp1 :: (a -> Comp b) -> (a -> Runner b)
--liftComp1 f x = Runner $ \env -> f x

returnJust = return . Just
returnValue = returnJust . C.fromValue

runSg :: Expr -> Runner (Maybe C.Context)
runSg (EValue v) = returnValue v
runSg EDocument = asks $ Just . E.getDocument
runSg EContext = asks $ Just . E.getContext
runSg (EVar v) = asks (lookup v) >>= go where
    go Nothing = throwError "Missing var"
    go x = return x
    lookup (VNamed n) = E.lookupNamedVar n
    lookup (VIndexed i) = E.lookupIndexedVar i

runSg (EArray exprs) = mapM runSgValue exprs >>= \vs -> return $ Just $ C.fromValue $
        V.VArray $ V.toArray $ catMaybes vs
runSg (EObject members) = mapM runMember members >>= \vs -> return $ Just $ C.fromValue $
        V.VObject $ V.toObject $ catMaybes vs where
    runMember :: (Expr, Expr) -> Runner (Maybe (V.String, V.Value))
    runMember (k, v) = runSgValue k >>= \k -> case k of
        Nothing -> return Nothing
        Just k -> castValueToString k >>= \k -> runSgValue v >>= \v -> case v of
            Nothing -> return Nothing
            Just v -> return $ Just (k, v)

runSg (EUnOp op expr) = runSgValue expr >>= \v -> fmap C.fromValue `liftM` go op v where
    go :: UnOp -> Maybe V.Value -> Runner (Maybe V.Value)
    go ONot (Just v) = (Just . V.VBoolean . V.toBoolean) `liftM` castValueToBool v -- co z not undefined
    go ONeg (Just (V.VNumber n)) = return $ Just $ V.VNumber (-n)
    go _ Nothing = return Nothing

runSg (ECall "error" [expr]) = runSg expr >>= throwError . show
runSg (ECall _ _) = throwError "Invalid function"
-- TODO call

runSg (EBinOp OAnd left right) = do
    x <- runSg left
    b <- castMaybeValueToBool $ fmap C.getValue x
    if b then runSg right else return x
runSg (EBinOp OOr left right) = do
    x <- runSg left
    b <- castMaybeValueToBool $ fmap C.getValue x
    if b then return x else runSg right
runSg (EBinOp op left right) = do
        x <- runSgValue left
        y <- runSgValue right
        fmap C.fromValue `liftM` (go op x y) where
    go OAdd = addMaybe
    go OSub = subtractMaybe
    go OMul = multiplyMaybe
    go ODiv = divideMaybe
    go OMod = moduloMaybe

runSg (ECmpOp left rights) = runSgValue left >>= \x -> go x rights where
    go _ [] = return $ Just $ C.fromValue $ V.VBoolean V.true
    go leftVal ((op, right):rights) = runSgValue right >>= \rightVal -> if goOp op leftVal rightVal then go rightVal rights else return $ Just $ C.fromValue $ V.VBoolean V.false
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

runSg e = do
    items <- takeSM 2 =<< runIt e
    case items of
        [] -> return Nothing
        [x] -> return $ Just x
        _ -> throwError "Multiple values returned"

runSgValue :: Expr -> Runner (Maybe V.Value)
runSgValue e = fmap C.getValue `liftM` runSg e

runBool :: Expr -> Runner Bool
runBool e = runSgValue e >>= castMaybeValueToBool

runBoolWith :: C.Context -> Expr -> Runner Bool
runBoolWith ctx expr = local (E.withContext ctx) $ runBool expr

castMaybeValueToBool :: Maybe V.Value -> Runner Bool
castMaybeValueToBool Nothing = return False
castMaybeValueToBool (Just v) = castValueToBool v

castValueToBool :: V.Value -> Runner Bool
castValueToBool V.VNull = return False
castValueToBool (V.VBoolean b) = return $ V.fromBoolean b
castValueToBool (V.VNumber n) = return $ n /= V.zero
castValueToBool (V.VString s) = return $ s /= V.epsilon
castValueToBool (V.VArray _) = throwError "Cannot convert array to boolean"
castValueToBool (V.VObject _) = throwError "Cannot convert object to boolean"

castValueToString :: V.Value -> Runner V.String
castValueToString V.VNull = return $ V.toString $ show V.VNull
castValueToString (V.VBoolean b) = return $ V.toString $ show b
castValueToString (V.VNumber n) = return $ V.toString $ show n
castValueToString (V.VString s) = return s
castValueToString (V.VArray _) = throwError "Cannot convert array to string"
castValueToString (V.VObject _) = throwError "Cannot convert object to string"

membersIt :: ContextSequence -> ContextSequence
membersIt = (=<<) (\ctx -> listS $ map ((flip $ uncurry C.withMember) ctx) $ V.members $ C.getValue ctx)

runIt :: Expr -> Runner ContextSequence
runIt (ESequence es) = mapM runIt es >>= return . joinS
runIt (ETrans "members" src []) = runIt src >>= return . membersIt
runIt (ETrans "members" src [expr]) = do
    it <- runIt src
    pickSM (membersIt it) $ \x -> local (E.withContext x) (runSgValue expr) >>= \x' -> case x' of
        Just (V.VString _) -> return $ x' == C.lookupKey x
        Just (V.VNumber _) -> return $ x' == C.lookupKey x
        _ -> castMaybeValueToBool x'
runIt (ETrans "first" src []) = truncateS 1 <$> runIt src
runIt (ETrans "first" src [expr]) = runIt src >>= findSM (flip runBoolWith expr) >>= return . maybeS
runIt (ETrans "last" src []) = runFold1S (\x y -> return y) src
runIt (ETrans "last" src [expr]) = runIt src >>= (findSM (flip runBoolWith expr) . reverseS) >>= return . maybeS
runIt (ETrans "count" src []) = runList src >>= (\xs -> return $ singletonS $ C.fromValue $ V.VNumber $ V.toNumber $ length xs)
runIt (ETrans "count" src [expr]) = do
    it <- runIt src
    n <- foldSM (\x y -> runBoolWith y expr >>= \b -> return $ if b then x + 1 else x) (0 :: Int) it
    return $ singletonS $ C.fromValue $ V.VNumber $ V.toNumber n
runIt (ETrans "min" src []) = runFold1S (\x y -> return $ if C.getValue y < C.getValue x then y else x) src
runIt (ETrans "max" src []) = runFold1S (\x y -> return $ if C.getValue y > C.getValue x then y else x) src
runIt (ETrans "sum" src []) = runFoldSValue (\x y -> add x $ C.getValue y) (V.VNumber V.zero) src
runIt (ETrans "array" src []) = runList src >>= \xs -> return $ singletonS $ C.fromValue $ V.VArray $ V.toArray $ map C.getValue xs
runIt (ETrans "reverse" src []) = reverseS <$> runIt src
----runIt (ETrans "object" src []) = do
--    

runIt (ETrans "filter" src [expr]) = do
    it <- runIt src
    pickSM it $ flip runBoolWith expr
runIt (ETrans "map" src [expr]) = do
    it <- runIt src
    f <- makeSequence1 $ \x -> do
        x' <- local (E.withContext x) (runSgValue expr)
        return $ maybeS $ fmap (flip C.withValue x) x'
    return $ it >>= f

runIt (ETrans _ _ _) = throwError "Invalid transformation"

runIt (EIf test yes no) = runBool test >>= \b -> runIt (if b then yes else no)

runIt (ELet v e1 e2) = runSg e1 >>= \ctx -> case ctx of
    Nothing -> return emptyS
    Just ctx -> local (withVar v ctx) (runIt e2)

runIt e = runSg e >>= go where
    go Nothing = return emptyS
    go (Just ctx) = return $ singletonS ctx

runFold1S :: (C.Context -> C.Context -> Runner C.Context) -> Expr -> Runner ContextSequence
runFold1S f e = runIt e >>= fold1SM f >>= liftR maybeS

runFoldSValue :: (V.Value -> C.Context -> Runner V.Value) -> V.Value -> Expr -> Runner ContextSequence
runFoldSValue f x e = runIt e >>= foldSM f x >>= return . singletonS . C.fromValue

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

divideMaybe :: MonadError Error m => Maybe V.Value -> Maybe V.Value -> m (Maybe V.Value)
divideMaybe = numberBinOpMaybe (\x y -> if y == V.zero then throwError "Zero division" else return $ x / y)

moduloMaybe :: MonadError Error m => Maybe V.Value -> Maybe V.Value -> m (Maybe V.Value)
moduloMaybe = numberBinOpMaybe (\x y -> if y == V.zero then throwError "Zero modulo" else return $ x `V.mod` y)

runList e = runIt e >>= unwrapSM

withVar :: Var -> C.Context -> E.Env -> E.Env
withVar (VNamed name) = E.withNamedVar name
withVar (VIndexed index) = E.withIndexedVar index
