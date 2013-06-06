{-# LANGUAGE DeriveDataTypeable #-}

module JTL.IR (BinOp(..), UnOp(..), CmpOp(..), Expr(..), Var(..)) where

import Data.Generics (Data)
import Data.Typeable (Typeable)
import qualified JTL.Value as V

data BinOp = OAdd | OSub | OMul | ODiv | OMod | OAnd | OOr deriving (Show, Typeable, Data)
data UnOp = ONeg | ONot deriving (Show, Typeable, Data)

data CmpOp = OEq | ONe | OLt | OLe | OGt | OGe deriving (Show, Typeable, Data)

data Var = VNamed String | VIndexed Int deriving (Show, Typeable, Data)

data Expr =
      EBinOp BinOp Expr Expr
    | EUnOp UnOp Expr
    | ECmpOp Expr [(CmpOp, Expr)]
    | EVar Var
    | EContext
    | EDocument
    | ETrans String Expr [Expr]
    | ECall String [Expr]
    | EArray [Expr]
    | EObject [(Expr, Expr)]
    | EValue V.Value
    | ELet Var Expr Expr
    | EIf Expr Expr Expr
    | ESequence [Expr]
    deriving (Show, Data, Typeable)
