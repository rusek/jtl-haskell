module JTL.Env (
    Env,
    fromDocument, empty, withContext, withNamedVar, withIndexedVar,
    getDocument, getContext, lookupIndexedVar, getIndexedVar, lookupNamedVar, getNamedVar
    ) where

import qualified JTL.Context as C
import qualified Data.Map as M
import qualified JTL.Value as V

data Env = Env { document :: C.Context, context :: C.Context,
                 indexedVars :: (M.Map Int C.Context), namedVars :: (M.Map String C.Context)}

fromDocument d = Env (C.fromDocument d) (C.fromDocument d) M.empty M.empty
empty = fromDocument V.VNull
withContext c e = e { context = c }
withNamedVar n v e = e { namedVars = M.insert n v $ namedVars e }
withIndexedVar i v e = e { indexedVars = M.insert i v $ indexedVars e }

getDocument = document
getContext = context
lookupIndexedVar i = M.lookup i . indexedVars
lookupNamedVar n = M.lookup n . namedVars
getIndexedVar i = (M.! i) . indexedVars
getNamedVar n = (M.! n) . namedVars
