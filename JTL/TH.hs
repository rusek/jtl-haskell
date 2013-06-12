module JTL.TH where

import Control.Monad (replicateM)
import qualified Data.Set as S
import JTL.IR ( Expr(..), Var(..) )
import JTL.Parser
import JTL.ParserMonad
import JTL.Runner
import qualified JTL.Context as C
import qualified JTL.Value as V
import qualified JTL.Env as E

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TS
import Language.Haskell.TH.Quote as QQ


runTH :: (V.ValueLike a, V.ValueLike b) => Expr -> [V.Value] -> a -> b
runTH expr indexed doc = case evaluate expr env of
        Left msg -> error msg
        Right ctxs -> toOut ctxs
    where
        env = foldl (flip $ uncurry E.withIndexedVar) (E.fromDocument $ fromIn doc)
            (zip [0..] $ map C.fromValue indexed) 

fromIn :: V.ValueLike a => a -> V.Value
fromIn = V.toValue

toOut :: V.ValueLike a => [C.Context] -> a
toOut ctxs = V.fromValue $ justValue ctxs

maybeValue [] = Nothing
maybeValue [ctx] = Just $ C.getValue ctx

justValue [ctx] = C.getValue ctx

freeIndexedVars (EBinOp _ left right) = freeIndexedVars left `S.union` freeIndexedVars right
freeIndexedVars (EUnOp _ expr) = freeIndexedVars expr
freeIndexedVars (ECmpOp expr rights) = freeIndexedVars expr `S.union` freeIndexedVarsL $ map snd rights
freeIndexedVars (EVar (VIndexed i)) = S.singleton i
freeIndexedVars EContext = S.empty
freeIndexedVars EDocument = S.empty
freeIndexedVars (ETrans _ src args) = freeIndexedVars src `S.union` freeIndexedVarsL args
freeIndexedVars (ECall _ args) = freeIndexedVarsL args
freeIndexedVars (EArray members) = freeIndexedVarsL members
freeIndexedVars (EObject members) = S.unions $ map (\(k, v) -> S.union (freeIndexedVars k) (freeIndexedVars v)) members
freeIndexedVars (EValue _) = S.empty
freeIndexedVars (ELet (VIndexed i) e1 e2) = freeIndexedVars e1 `S.union` S.delete i (freeIndexedVars e2)
freeIndexedVars (EIf e1 e2 e3) = freeIndexedVars e1 `S.union` freeIndexedVars e2 `S.union` freeIndexedVars e3
freeIndexedVars (ESequence es) = freeIndexedVarsL es

freeIndexedVarsL xs = S.unions $ map freeIndexedVars xs

indexedVarCount e = let iv = freeIndexedVars e in if S.null iv then 0 else S.findMax iv + 1

expr :: QQ.QuasiQuoter
expr = QQ.QuasiQuoter { quoteExp = quoteExpr, quotePat = undefined, quoteDec = undefined, quoteType = undefined }

quoteExpr s = case runParser parser $ toLexerInput s of
    Ok e -> do
        -- mkNameG_d
        let runTHName = TS.mkNameG_v "main" "JTL.TH" "runTH"
        let fromInName = TS.mkNameG_v "main" "JTL.TH" "fromIn"
--        let fromInName = TS.mkName "JTL.TH.fromIn"
--        let runTHName = TS.mkName "JTL.TH.runTH"
        exp <- dataToExpQ (const Nothing) e
        
--        let (TS.AppE (TS.ConE (TS.Name coto (TS.NameG TS.DataName _ _))) _) = exp
--        fail $ TS.occString coto
        varNames <- replicateM (indexedVarCount e) $ TS.newName "a"
        return $ TS.LamE
            (map TS.VarP varNames)
            (TS.AppE 
                (TS.AppE (TS.VarE runTHName) exp)
                (TS.ListE $ map (TS.AppE (TS.VarE fromInName). TS.VarE) varNames))
    Err _ msg -> error msg

