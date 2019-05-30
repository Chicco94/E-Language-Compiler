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
  '*=' { PT _ (TS _ 13) }
  '+' { PT _ (TS _ 14) }
  '+=' { PT _ (TS _ 15) }
  ',' { PT _ (TS _ 16) }
  '-' { PT _ (TS _ 17) }
  '-=' { PT _ (TS _ 18) }
  '..' { PT _ (TS _ 19) }
  '/' { PT _ (TS _ 20) }
  '//' { PT _ (TS _ 21) }
  '//=' { PT _ (TS _ 22) }
  '/=' { PT _ (TS _ 23) }
  ':' { PT _ (TS _ 24) }
  ':=' { PT _ (TS _ 25) }
  ':]' { PT _ (TS _ 26) }
  ';' { PT _ (TS _ 27) }
  '<' { PT _ (TS _ 28) }
  '<=' { PT _ (TS _ 29) }
  '==' { PT _ (TS _ 30) }
  '>' { PT _ (TS _ 31) }
  '>=' { PT _ (TS _ 32) }
  '[' { PT _ (TS _ 33) }
  ']' { PT _ (TS _ 34) }
  '^' { PT _ (TS _ 35) }
  '^=' { PT _ (TS _ 36) }
  'bool' { PT _ (TS _ 37) }
  'char' { PT _ (TS _ 38) }
  'def' { PT _ (TS _ 39) }
  'else' { PT _ (TS _ 40) }
  'float' { PT _ (TS _ 41) }
  'for' { PT _ (TS _ 42) }
  'if' { PT _ (TS _ 43) }
  'in' { PT _ (TS _ 44) }
  'int' { PT _ (TS _ 45) }
  'match' { PT _ (TS _ 46) }
  'match _' { PT _ (TS _ 47) }
  'string' { PT _ (TS _ 48) }
  'switch' { PT _ (TS _ 49) }
  'var' { PT _ (TS _ 50) }
  'void' { PT _ (TS _ 51) }
  'while' { PT _ (TS _ 52) }
  '{' { PT _ (TS _ 53) }
  '|=' { PT _ (TS _ 54) }
  '||' { PT _ (TS _ 55) }
  '}' { PT _ (TS _ 56) }

L_PTrue { PT _ (T_PTrue _) }
L_PFalse { PT _ (T_PFalse _) }
L_PReturn { PT _ (T_PReturn _) }
L_PContinue { PT _ (T_PContinue _) }
L_PBreak { PT _ (T_PBreak _) }
L_PIdent { PT _ (T_PIdent _) }
L_PInteger { PT _ (T_PInteger _) }
L_PFloat { PT _ (T_PFloat _) }
L_PChar { PT _ (T_PChar _) }
L_PString { PT _ (T_PString _) }


%%

PTrue    :: { PTrue} : L_PTrue { PTrue (mkPosToken $1)}
PFalse    :: { PFalse} : L_PFalse { PFalse (mkPosToken $1)}
PReturn    :: { PReturn} : L_PReturn { PReturn (mkPosToken $1)}
PContinue    :: { PContinue} : L_PContinue { PContinue (mkPosToken $1)}
PBreak    :: { PBreak} : L_PBreak { PBreak (mkPosToken $1)}
PIdent    :: { PIdent} : L_PIdent { PIdent (mkPosToken $1)}
PInteger    :: { PInteger} : L_PInteger { PInteger (mkPosToken $1)}
PFloat    :: { PFloat} : L_PFloat { PFloat (mkPosToken $1)}
PChar    :: { PChar} : L_PChar { PChar (mkPosToken $1)}
PString    :: { PString} : L_PString { PString (mkPosToken $1)}

Program :: { Program }
Program : ListDecl { AbsE.PDefs (reverse $1) }
ListDecl :: { [Decl] }
ListDecl : {- empty -} { [] } | ListDecl Decl { flip (:) $1 $2 }
Decl :: { Decl }
Decl : 'def' LExpr '(' ListArg ')' Guard CompStmt { AbsE.DeclFun $2 $4 $6 $7 }
     | Stmt { AbsE.DeclStmt $1 }
AnnotatedDecl :: { AnnotatedDecl }
AnnotatedDecl : '[' Type ':]' Decl { AbsE.ADecl $2 $4 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
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
Stmt : Expr ';' { AbsE.StmtExpr $1 }
     | 'var' PIdent Guard ':=' ComplexExpr ';' { AbsE.StmtVarInit $2 $3 $5 }
     | 'def' PIdent Guard ':=' ComplexExpr ';' { AbsE.StmtDefInit $2 $3 $5 }
     | PReturn '(' Expr ')' ';' { AbsE.StmtReturn $1 $3 }
     | PReturn ';' { AbsE.StmtNoReturn $1 }
     | CompStmt { AbsE.SComp $1 }
     | 'if' '(' Expr ')' CompStmt 'else' CompStmt { AbsE.StmtIfThenElse $3 $5 $7 }
     | 'if' '(' Expr ')' CompStmt { AbsE.StmtIfThen $3 $5 }
     | 'switch' '(' Expr ')' '{' ListNormCase ListDfltCase '}' { AbsE.StmtSwitchCase $3 (reverse $6) (reverse $7) }
     | PBreak ';' { AbsE.StmtBreak $1 }
     | PContinue ';' { AbsE.StmtContinue $1 }
     | 'while' '(' Expr ')' CompStmt { AbsE.StmtWhile $3 $5 }
     | 'for' PIdent 'in' Range CompStmt { AbsE.StmtFor $2 $4 $5 }
ComplexExpr :: { ComplexExpr }
ComplexExpr : Expr { AbsE.ExprSimple $1 }
            | '[' ListComplexExpr ']' { AbsE.ExprArray $2 }
ListComplexExpr :: { [ComplexExpr] }
ListComplexExpr : ComplexExpr { (:[]) $1 }
                | ComplexExpr ',' ListComplexExpr { (:) $1 $3 }
ListPInteger :: { [PInteger] }
ListPInteger : {- empty -} { [] }
             | PInteger { (:[]) $1 }
             | PInteger ',' ListPInteger { (:) $1 $3 }
CompStmt :: { CompStmt }
CompStmt : '{' ListDecl '}' { AbsE.StmtBlock (reverse $2) }
NormCase :: { NormCase }
NormCase : 'match' Expr CompStmt { AbsE.CaseNormal $2 $3 }
DfltCase :: { DfltCase }
DfltCase : 'match _' CompStmt { AbsE.CaseDefault $2 }
ListNormCase :: { [NormCase] }
ListNormCase : {- empty -} { [] }
             | ListNormCase NormCase { flip (:) $1 $2 }
ListDfltCase :: { [DfltCase] }
ListDfltCase : {- empty -} { [] }
             | ListDfltCase DfltCase { flip (:) $1 $2 }
Range :: { Range }
Range : ForId '..' ForId { AbsE.ExprRange $1 $3 }
ForId :: { ForId }
ForId : PIdent { AbsE.ForIdent $1 }
      | PInteger { AbsE.ForInteger $1 }
Expr :: { Expr }
Expr : LExpr AssignOperator Expr1 { AbsE.ExprAssign $1 $2 $3 }
     | Expr1 { $1 }
LExpr :: { LExpr }
LExpr : PIdent { AbsE.LExprId $1 }
      | Ref { AbsE.LExprRef $1 }
      | Arr { AbsE.LExprArr $1 }
Ref :: { Ref }
Ref : '*' LExpr { AbsE.LRefExpr $2 }
Arr :: { Arr }
Arr : PIdent '[' AExpr ']' { AbsE.LArrExpr $1 $3 }
AExpr :: { AExpr }
AExpr : PInteger { AbsE.ArrSing $1 }
      | AExpr ',' PInteger { AbsE.ArrMul $1 $3 }
Expr17 :: { Expr }
Expr17 : LExpr { AbsE.ExprLeft $1 } | '(' Expr ')' { $2 }
Expr16 :: { Expr }
Expr16 : PInteger { AbsE.ExprInt $1 }
       | PFloat { AbsE.ExprFloat $1 }
       | PChar { AbsE.ExprChar $1 }
       | PString { AbsE.ExprString $1 }
       | PTrue { AbsE.ExprTrue $1 }
       | PFalse { AbsE.ExprFalse $1 }
       | Expr17 { $1 }
Expr15 :: { Expr }
Expr15 : PIdent '(' ListExpr ')' { AbsE.ExprFunCall $1 $3 }
       | Expr16 { $1 }
Expr14 :: { Expr }
Expr14 : '!' Expr15 { AbsE.ExprBoolNot $2 }
       | '-' Expr15 { AbsE.ExprNegation $2 }
       | '+' Expr15 { AbsE.ExprAddition $2 }
       | Expr15 { $1 }
Expr13 :: { Expr }
Expr13 : Expr14 '^' Expr13 { AbsE.ExprPower $1 $3 } | Expr14 { $1 }
Expr12 :: { Expr }
Expr12 : Expr12 '*' Expr13 { AbsE.ExprMul $1 $3 }
       | Expr12 '/' Expr13 { AbsE.ExprFloatDiv $1 $3 }
       | Expr12 '//' Expr13 { AbsE.ExprIntDiv $1 $3 }
       | Expr12 '%' Expr13 { AbsE.ExprReminder $1 $3 }
       | Expr12 '%%' Expr13 { AbsE.ExprModulo $1 $3 }
       | Expr13 { $1 }
Expr11 :: { Expr }
Expr11 : '&' LExpr { AbsE.ExprReference $2 }
       | Expr11 '+' Expr12 { AbsE.ExprPlus $1 $3 }
       | Expr11 '-' Expr12 { AbsE.ExprMinus $1 $3 }
       | Expr12 { $1 }
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
Expr10 :: { Expr }
Expr10 : Expr11 { $1 }
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
               | '^=' { AbsE.OpPower }
Type :: { Type }
Type : BasicType { AbsE.TypeBasicType $1 }
     | CompoundType { AbsE.TypeCompoundType $1 }
BasicType :: { BasicType }
BasicType : 'bool' { AbsE.TypeBool }
          | 'float' { AbsE.TypeFloat }
          | 'int' { AbsE.TypeInt }
          | 'void' { AbsE.TypeVoid }
          | 'char' { AbsE.TypeChar }
          | 'string' { AbsE.TypeString }
CompoundType :: { CompoundType }
CompoundType : ArrayType { AbsE.CompoundTypeArrayType $1 }
             | Ptr { AbsE.CompoundTypePtr $1 }
ArrayType :: { ArrayType }
ArrayType : '[' ListPInteger ']' BasicType { AbsE.ArrDefBase $2 $4 }
          | '[' ListPInteger ']' Ptr { AbsE.ArrDefPtr $2 $4 }
Ptr :: { Ptr }
Ptr : BasicType '*' { AbsE.Pointer $1 }
    | Ptr '*' { AbsE.Pointer2Pointer $1 }
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

