{
module JTL.Parser where

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import JTL.Parsec (lexer)
import JTL.Token
import JTL.IR
import qualified JTL.Value as V
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    '+'          { TPlus }
    '-'          { TMinus }
    '*'          { TStar }
    '/'          { TSlash }
    '%'          { TPercent }
    
    let          { TLet }
    in           { TIn }
    if           { TIf }
    then         { TThen }
    else         { TElse }
    
    and          { TAnd }
    or           { TOr }
    not          { TNot }
    true         { TTrue }
    false        { TFalse }
    null         { TNull }
    ident        { TIdent $$ }
    
    '@'          { TContextRef }
    namedVar     { TNamedVar $$ }
    indexedVar   { TIndexedVar $$ }
    
    '<'          { TLess }
    '<='         { TLessEqual }
    '>'          { TGreater }
    '>='         { TGreaterEqual }
    '=='         { TEqual }
    '!='         { TNotEqual }
    '&&'         { TDoubleAmp }
    '||'         { TDoubleBar }
    
    '?'          { TQuestMark }
    '='          { TAssign }
    '.'          { TDot }
    ','          { TComma }
    ':'          { TColon }
    '::'         { TDoubleColon }
    '('          { TLeftParen}
    ')'          { TRightParen}
    '['          { TLeftBracket}
    ']'          { TRightBracket}
    '{'          { TLeftBrace}
    '}'          { TRightBrace}
    
    number       { TNumber $$ }
    string       { TString $$ }
    
%monad { Parser }
%lexer { lexer } { TEOF }

%%

Expr : if Expr then Expr else Expr { EIf $2 $4 $6 }
     | let Var '=' Expr in Expr { ELet $2 $4 $6 }
     | CIf { $1 }

CIf  : AndOr '?' CIf ':' CIf { EIf $1 $3 $5 }
     | AndOr { $1 }

AndOr : AndOr AndOrOp Not { EBinOp $2 $1 $3 }
      | Not { $1 }

AndOrOp : and { OAnd }
        | '&&' { OAnd }
        | '||' { OOr }
        | or  { OOr }

Not : not Not { EUnOp ONot $2 }
    | Cmp { $1 }

Cmp : Additive { $1 }
    | Additive Cmp1Rev { ECmpOp $1 $ reverse $2 }

Cmp1Rev : Cmp1Rev CmpOp Additive { CExpr $2 $3:$1 }
        | CmpOp Additive { [CExpr $1 $2] }

CmpOp : '==' { OEq }
      | '!=' { ONe }
      | '<' { OLt }
      | '<=' { OLe }
      | '>' { OGt }
      | '>=' { OGe }

Additive : Additive AdditiveOp Multiplicative { EBinOp $2 $1 $3 }
         | Multiplicative { $1 }

AdditiveOp : '+' { OAdd }
           | '-' { OSub }

Multiplicative : Multiplicative MultiplicativeOp Neg { EBinOp $2 $1 $3 }
               | Neg { $1 }

MultiplicativeOp : '*' { OMul }
                 | '/' { ODiv }
                 | '%' { OMod }

Neg : '-' Neg { EUnOp ONeg $2 }
    | Trans   { $1 }

Trans : Trans '.' Name { ETrans "members" $1 [EValue $ V.VString $ V.toString $3] }
      | Trans '.' '*' { ETrans "members" $1 [] }
      | Trans '[' Expr ']' { ETrans "members" $1 [$3] }
      | Trans '::' ident '(' Exprs ')' { ETrans $3 $1 $5 }
      | Trans '::' ident { ETrans $3 $1 [] }
      | Trans '::' '[' ']' { ETrans "array" $1 [] }
      | Trans '::' '[' Expr ']' { ETrans "array" $1 [$4] }
      | Trans '::' '{' '}' { ETrans "object" $1 [] }
      | Trans '::' '{' Expr ':' Expr '}' { ETrans "object" $1 [$4, $6] }
      | Naked { $1 }

Naked : Var             { EVar $1 }
      | '@'             { EContext }
      | ident           { ETrans "members" EDocument [EValue $ V.VString $ V.toString $1] }
      | '*'             { ETrans "members" EDocument [] }
      | number          { EValue $ V.VNumber $1 }
      | true            { EValue $ V.VBoolean V.true }
      | false           { EValue $ V.VBoolean V.false }
      | null            { EValue V.VNull }
      | string          { EValue $ V.VString $1 }
      | '{' ObjectMembers '}' { EObject $2 }
      | '[' Exprs ']' { EArray $2 }
      | '(' Exprs ')' { ESequence $2 }
      | ident '(' Exprs ')' { ECall $1 $3 }

ObjectMembers : { [] }
              | ObjectMembers1R { reverse $1 }

ObjectMembers1R : ObjectMembers1R ',' ObjectMember { $3:$1 }
                | ObjectMember { [$1] }

ObjectMember : Expr ':' Expr { ($1, $3) }

Exprs : { [] }
      | Exprs1R { reverse $1 }

Exprs1R : Exprs1R ',' Expr { $3:$1 }
        | Expr { [$1] }

Var : namedVar { VNamed $1 }
    | indexedVar { VIndexed $1 }

Name : ident { $1 }

{
parseError _ = fail "Parse error"

parse :: String -> Either String Expr
parse inp = case runParser parser () "" inp of
    Left err -> Left $ showError err
    Right expr -> Right expr

showError e = "Line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg where
    pos = errorPos e
    line = sourceLine pos
    col = sourceColumn pos
    msgs = errorMessages e
    msg = findMsg msgs $ findUnExpect msgs $ "Unknown error"

    findMsg (Message msg:_) fb = msg
    findMsg (_:xs) fb = findMsg xs fb
    findMsg [] fb = fb
    
    findUnExpect (UnExpect msg:_) fb = "Unexpected " ++ msg
    findUnExpect (_:xs) fb = findUnExpect xs fb
    findUnExpect [] fb = fb
}
