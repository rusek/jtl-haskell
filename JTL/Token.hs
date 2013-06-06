module JTL.Token where

import qualified JTL.Value as V

data Token =
      TPlus
    | TMinus
    | TStar
    | TSlash
    | TPercent
    | TDoubleColon
    | TEqual
    | TNotEqual
    | TLess
    | TLessEqual
    | TGreater
    | TGreaterEqual
    | TAssign
    | TLet
    | TIn
    | TIf
    | TThen
    | TElse
    | TAnd
    | TOr
    | TNot
    | TEOF
    | TDot
    | TNull
    | TTrue
    | TFalse
    | TComma
    | TColon
    | TQuestMark
    | TExclMark
    | TIdent String
    | TNumber V.Number
    | TString V.String
    | TLeftParen
    | TRightParen
    | TLeftBracket
    | TRightBracket
    | TLeftBrace
    | TRightBrace
    | TContextRef
    | TNamedVar String
    | TIndexedVar Int

instance Show Token where
    show TTrue = "true"
    show TFalse = "false"
    show TNull = "null"
    show TPlus = "+"
    show TMinus = "-"
    show TStar = "*"
    show TSlash = "/"
    show TPercent = "%"
    show TLess = "<"
    show TLessEqual = "<="
    show TGreater = ">"
    show TGreaterEqual = ">="
    show TEqual = "=="
    show TNotEqual = "!="
    show TAnd = "and"
    show TOr = "or"
    show TNot = "not"
    show TColon = ":"
    show TDoubleColon = "::"
    show TComma = ","
    show TDot = "."
    show TLeftBrace = "{"
    show TRightBrace = "}"
    show TLeftBracket = "["
    show TRightBracket = "]"
    show TLeftParen = "("
    show TRightParen = ")"
    show TEOF = "<eof>"
    show (TNumber n) = show n
    show (TString s) = show s
    show TContextRef = "@"
    show (TNamedVar n) = '@':n
    show (TIndexedVar i) = '@':show i

