-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParE where
import AbsE
import LexE
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '%%' { PT _ (TS _ 4) }
  '%%=' { PT _ (TS _ 5) }
  '%=' { PT _ (TS _ 6) }
  '&' { PT _ (TS _ 7) }
  '&&' { PT _ (TS _ 8) }
  '&=' { PT _ (TS _ 9) }
  '(' { PT _ (TS _ 10) }
  ')' { PT _ (TS _ 11) }
  '*' { PT _ (TS _ 12) }
  '**=' { PT _ (TS _ 13) }
  '*=' { PT _ (TS _ 14) }
  '+' { PT _ (TS _ 15) }
  '+=' { PT _ (TS _ 16) }
  ',' { PT _ (TS _ 17) }
  '-' { PT _ (TS _ 18) }
  '-=' { PT _ (TS _ 19) }
  '..' { PT _ (TS _ 20) }
  '..!' { PT _ (TS _ 21) }
  '/' { PT _ (TS _ 22) }
  '//' { PT _ (TS _ 23) }
  '//=' { PT _ (TS _ 24) }
  '/=' { PT _ (TS _ 25) }
  ':' { PT _ (TS _ 26) }
  ':=' { PT _ (TS _ 27) }
  ':]' { PT _ (TS _ 28) }
  ';' { PT _ (TS _ 29) }
  '<' { PT _ (TS _ 30) }
  '<=' { PT _ (TS _ 31) }
  '==' { PT _ (TS _ 32) }
  '>' { PT _ (TS _ 33) }
  '>=' { PT _ (TS _ 34) }
  '[' { PT _ (TS _ 35) }
  ']' { PT _ (TS _ 36) }
  'bool' { PT _ (TS _ 37) }
  'break' { PT _ (TS _ 38) }
  'char' { PT _ (TS _ 39) }
  'continue' { PT _ (TS _ 40) }
  'def' { PT _ (TS _ 41) }
  'double' { PT _ (TS _ 42) }
  'else' { PT _ (TS _ 43) }
  'false' { PT _ (TS _ 44) }
  'for' { PT _ (TS _ 45) }
  'if' { PT _ (TS _ 46) }
  'in' { PT _ (TS _ 47) }
  'int' { PT _ (TS _ 48) }
  'match' { PT _ (TS _ 49) }
  'match _' { PT _ (TS _ 50) }
  'return' { PT _ (TS _ 51) }
  'string' { PT _ (TS _ 52) }
  'switch' { PT _ (TS _ 53) }
  'true' { PT _ (TS _ 54) }
  'var' { PT _ (TS _ 55) }
  'void' { PT _ (TS _ 56) }
  'while' { PT _ (TS _ 57) }
  '{' { PT _ (TS _ 58) }
  '|=' { PT _ (TS _ 59) }
  '||' { PT _ (TS _ 60) }
  '}' { PT _ (TS _ 61) }

L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }
L_charac { PT _ (TC $$) }
L_quoted { PT _ (TL $$) }
L_PIdent { PT _ (T_PIdent _) }


%%

Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
Char    :: { Char }    : L_charac { (read ( $1)) :: Char }
String  :: { String }  : L_quoted {  $1 }
PIdent    :: { PIdent} : L_PIdent { PIdent (mkPosToken $1)}

Program :: { Program }
Program : ListDecl { AbsE.PDefs (reverse $1) }
ListDecl :: { [Decl] }
ListDecl : {- empty -} { [] } | ListDecl Decl { flip (:) $1 $2 }
ListAnnotatedDecl :: { [AnnotatedDecl] }
ListAnnotatedDecl : {- empty -} { [] }
                  | ListAnnotatedDecl AnnotatedDecl { flip (:) $1 $2 }
AnnotatedDecl :: { AnnotatedDecl }
AnnotatedDecl : Decl { AbsE.UntypedDecl $1 }
              | '[' Type ':]' Decl { AbsE.TypedDecl $2 $4 }
Decl :: { Decl }
Decl : 'def' LExpr '(' ListArg ')' Guard '{' ListStmt '}' { AbsE.DeclFun $2 $4 $6 (reverse $8) }
     | Stmt { AbsE.DeclStmt $1 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
Arg :: { Arg }
Arg : Modality PIdent Guard { AbsE.ArgDecl $1 $2 $3 }
Modality :: { Modality }
Modality : {- empty -} { AbsE.ModEmpty }
         | 'var' { AbsE.ModVar }
         | 'def' { AbsE.ModDef }
Guard :: { Guard }
Guard : {- empty -} { AbsE.GuardVoid }
      | ':' Type { AbsE.GuardType $2 }
Stmt :: { Stmt }
Stmt : Expr EndLine { AbsE.StmtExpr $1 $2 }
     | 'var' LExpr Guard EndLine { AbsE.StmtDecl $2 $3 $4 }
     | 'var' '[' LExpr ']' Guard EndLine { AbsE.StmtIterDecl $3 $5 $6 }
     | 'var' LExpr Guard ':=' Expr EndLine { AbsE.StmtVarInit $2 $3 $5 $6 }
     | 'def' LExpr Guard ':=' Expr EndLine { AbsE.StmtDefInit $2 $3 $5 $6 }
     | 'return' ListExpr EndLine { AbsE.StmtReturn $2 $3 }
     | '{' ListDecl '}' { AbsE.StmtBlock (reverse $2) }
     | 'if' '(' Expr ')' Stmt 'else' Stmt { AbsE.StmtIfElse $3 $5 $7 }
     | 'if' '(' Expr ')' Stmt { AbsE.StmtIfNoElse $3 $5 }
     | 'switch' '(' Expr ')' '{' ListNormCase ListDfltCase '}' { AbsE.SSwitchCase $3 (reverse $6) (reverse $7) }
     | 'break' { AbsE.StmtBreak }
     | 'continue' { AbsE.StmtContinue }
     | 'while' '(' Expr ')' Stmt { AbsE.StmtWhile $3 $5 }
     | 'for' PIdent 'in' TypeIter Stmt { AbsE.StmtFor $2 $4 $5 }
NormCase :: { NormCase }
NormCase : 'match' Expr Stmt { AbsE.CaseNormal $2 $3 }
DfltCase :: { DfltCase }
DfltCase : 'match _' Stmt { AbsE.CaseDefault $2 }
ListNormCase :: { [NormCase] }
ListNormCase : {- empty -} { [] }
             | ListNormCase NormCase { flip (:) $1 $2 }
ListDfltCase :: { [DfltCase] }
ListDfltCase : {- empty -} { [] }
             | ListDfltCase DfltCase { flip (:) $1 $2 }
Expr :: { Expr }
Expr : LExpr AssignOperator Expr1 { AbsE.StmtAssign $1 $2 $3 }
     | Expr1 { $1 }
LExpr :: { LExpr }
LExpr : PIdent { AbsE.LExprId $1 } | Ref { AbsE.LExprRef $1 }
Ref :: { Ref }
Ref : '*' LExpr { AbsE.RefExpr $2 }
Expr17 :: { Expr }
Expr17 : LExpr { AbsE.LeftExpr $1 } | '(' Expr ')' { $2 }
Expr16 :: { Expr }
Expr16 : Integer { AbsE.ExprInt $1 }
       | Double { AbsE.ExprDouble $1 }
       | Char { AbsE.ExprChar $1 }
       | String { AbsE.ExprString $1 }
       | 'true' { AbsE.ExprTrue }
       | 'false' { AbsE.ExprFalse }
       | Expr17 { $1 }
Expr15 :: { Expr }
Expr15 : PIdent '(' ListArg ')' { AbsE.ExprFunCall $1 $3 }
       | Expr16 { $1 }
Expr14 :: { Expr }
Expr14 : '!' Expr15 { AbsE.ExprBoolNot $2 }
       | '&' LExpr { AbsE.ExprDeref $2 }
       | '-' Expr15 { AbsE.ExprNegation $2 }
       | '+' Expr15 { AbsE.ExprAddition $2 }
       | Expr15 { $1 }
Expr12 :: { Expr }
Expr12 : Expr12 '*' Expr13 { AbsE.ExprMul $1 $3 }
       | Expr12 '/' Expr13 { AbsE.ExprFloatDiv $1 $3 }
       | Expr12 '//' Expr13 { AbsE.ExprIntDiv $1 $3 }
       | Expr12 '%' Expr13 { AbsE.ExprReminder $1 $3 }
       | Expr12 '%%' Expr13 { AbsE.ExprModulo $1 $3 }
       | Expr13 { $1 }
Expr11 :: { Expr }
Expr11 : Expr11 '+' Expr12 { AbsE.ExprPlus $1 $3 }
       | Expr11 '-' Expr12 { AbsE.ExprMinus $1 $3 }
       | Expr12 { $1 }
Expr10 :: { Expr }
Expr10 : Expr10 '..' Expr11 { AbsE.ExprIntInc $1 $3 }
       | Expr10 '..!' Expr11 { AbsE.ExprIntExc $1 $3 }
       | Expr11 { $1 }
Expr9 :: { Expr }
Expr9 : Expr9 '<' Expr10 { AbsE.ExprLt $1 $3 }
      | Expr9 '>' Expr10 { AbsE.ExprGt $1 $3 }
      | Expr9 '<=' Expr10 { AbsE.ExprLtEq $1 $3 }
      | Expr9 '>=' Expr10 { AbsE.ExprGtEq $1 $3 }
      | Expr10 { $1 }
Expr8 :: { Expr }
Expr8 : Expr8 '==' Expr9 { AbsE.ExprEq $1 $3 }
      | Expr8 '!=' Expr9 { AbsE.ExprNeq $1 $3 }
      | Expr9 { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 '&&' Expr5 { AbsE.ExprAnd $1 $3 } | Expr5 { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 '||' Expr4 { AbsE.ExprOr $1 $3 } | Expr4 { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 { $1 }
Expr2 :: { Expr }
Expr2 : Expr3 { $1 }
Expr5 :: { Expr }
Expr5 : Expr6 { $1 }
Expr6 :: { Expr }
Expr6 : Expr7 { $1 }
Expr7 :: { Expr }
Expr7 : Expr8 { $1 }
Expr13 :: { Expr }
Expr13 : Expr14 { $1 }
ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }
AssignOperator :: { AssignOperator }
AssignOperator : ':=' { AbsE.OpAssign }
               | '|=' { AbsE.OpOr }
               | '&=' { AbsE.OpAnd }
               | '+=' { AbsE.OpPlus }
               | '-=' { AbsE.OpMinus }
               | '*=' { AbsE.OpMul }
               | '//=' { AbsE.OpIntDiv }
               | '/=' { AbsE.OpFloatDiv }
               | '%=' { AbsE.OpRemainder }
               | '%%=' { AbsE.OpModulo }
               | '**=' { AbsE.OpPower }
Type :: { Type }
Type : 'bool' { AbsE.TypeBool }
     | 'double' { AbsE.TypeDouble }
     | 'int' { AbsE.TypeInt }
     | 'void' { AbsE.TypeVoid }
     | 'char' { AbsE.TypeChar }
     | 'string' { AbsE.TypeString }
     | CompoundType { AbsE.TypeCompound $1 }
CompoundType :: { CompoundType }
CompoundType : Type '*' { AbsE.TypePointer $1 }
             | TypeIter { AbsE.TypeIterable $1 }
TypeIter :: { TypeIter }
TypeIter : Expr10 { AbsE.TypeIterInterval $1 }
         | '[' ListExpr ']' { AbsE.TypeIterArray $2 }
EndLine :: { EndLine }
EndLine : ';' { AbsE.Semicolon }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

