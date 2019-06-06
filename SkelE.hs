module SkelE where

-- Haskell module generated by the BNF converter

import AbsE
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transPTrue :: PTrue -> Result
transPTrue x = case x of
  PTrue string -> failure x
transPFalse :: PFalse -> Result
transPFalse x = case x of
  PFalse string -> failure x
transPReturn :: PReturn -> Result
transPReturn x = case x of
  PReturn string -> failure x
transPContinue :: PContinue -> Result
transPContinue x = case x of
  PContinue string -> failure x
transPBreak :: PBreak -> Result
transPBreak x = case x of
  PBreak string -> failure x
transPIdent :: PIdent -> Result
transPIdent x = case x of
  PIdent string -> failure x
transPInteger :: PInteger -> Result
transPInteger x = case x of
  PInteger string -> failure x
transPFloat :: PFloat -> Result
transPFloat x = case x of
  PFloat string -> failure x
transPChar :: PChar -> Result
transPChar x = case x of
  PChar string -> failure x
transPString :: PString -> Result
transPString x = case x of
  PString string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  PDefs decls -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  TypedDecl annotateddecl -> failure x
  DeclFun lexpr args guard compstmt -> failure x
  DeclStmt stmt -> failure x
transAnnotatedDecl :: AnnotatedDecl -> Result
transAnnotatedDecl x = case x of
  ADecl type_ decl -> failure x
transArg :: Arg -> Result
transArg x = case x of
  ArgDecl modality pident guard -> failure x
transModality :: Modality -> Result
transModality x = case x of
  ModEmpty -> failure x
  ModVar -> failure x
  ModDef -> failure x
transGuard :: Guard -> Result
transGuard x = case x of
  GuardVoid -> failure x
  GuardType type_ -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  StmtExpr expr -> failure x
  StmtVarInit pident guard complexexpr -> failure x
  StmtDefInit pident guard complexexpr -> failure x
  StmtReturn preturn expr -> failure x
  StmtNoReturn preturn -> failure x
  SComp compstmt -> failure x
  StmtIfThenElse expr compstmt1 compstmt2 -> failure x
  StmtIfThen expr compstmt -> failure x
  StmtTryCatch compstmt1 compstmt2 -> failure x
  StmtSwitchCase expr normcases dfltcases -> failure x
  StmtBreak pbreak -> failure x
  StmtContinue pcontinue -> failure x
  StmtWhile expr compstmt -> failure x
  StmtLoopUntil compstmt expr -> failure x
  StmtFor pident range compstmt -> failure x
transComplexExpr :: ComplexExpr -> Result
transComplexExpr x = case x of
  ExprSimple expr -> failure x
  ExprArray complexexprs -> failure x
transCompStmt :: CompStmt -> Result
transCompStmt x = case x of
  StmtBlock decls -> failure x
transNormCase :: NormCase -> Result
transNormCase x = case x of
  CaseNormal expr compstmt -> failure x
transDfltCase :: DfltCase -> Result
transDfltCase x = case x of
  CaseDefault compstmt -> failure x
transRange :: Range -> Result
transRange x = case x of
  ExprRange expr1 expr2 -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  ExprAssign lexpr assignoperator expr -> failure x
  ExprLeft lexpr -> failure x
  ExprInt pinteger -> failure x
  ExprFloat pfloat -> failure x
  ExprChar pchar -> failure x
  ExprString pstring -> failure x
  ExprTrue ptrue -> failure x
  ExprFalse pfalse -> failure x
  ExprFunCall pident exprs -> failure x
  ExprTernaryIf expr1 expr2 expr3 -> failure x
  ExprBoolNot expr -> failure x
  ExprNegation expr -> failure x
  ExprAddition expr -> failure x
  ExprPower expr1 expr2 -> failure x
  ExprMul expr1 expr2 -> failure x
  ExprFloatDiv expr1 expr2 -> failure x
  ExprIntDiv expr1 expr2 -> failure x
  ExprReminder expr1 expr2 -> failure x
  ExprModulo expr1 expr2 -> failure x
  ExprReference lexpr -> failure x
  ExprPlus expr1 expr2 -> failure x
  ExprMinus expr1 expr2 -> failure x
  ExprLt expr1 expr2 -> failure x
  ExprGt expr1 expr2 -> failure x
  ExprLtEq expr1 expr2 -> failure x
  ExprGtEq expr1 expr2 -> failure x
  ExprEq expr1 expr2 -> failure x
  ExprNeq expr1 expr2 -> failure x
  ExprAnd expr1 expr2 -> failure x
  ExprOr expr1 expr2 -> failure x
transLExpr :: LExpr -> Result
transLExpr x = case x of
  LExprId pident -> failure x
  LExprRef ref -> failure x
  LExprArr arr -> failure x
transRef :: Ref -> Result
transRef x = case x of
  LRefExpr lexpr -> failure x
transArr :: Arr -> Result
transArr x = case x of
  LArrExpr pident aexpr -> failure x
transAExpr :: AExpr -> Result
transAExpr x = case x of
  ArrSing expr -> failure x
  ArrMul aexpr expr -> failure x
transAssignOperator :: AssignOperator -> Result
transAssignOperator x = case x of
  OpAssign -> failure x
  OpOr -> failure x
  OpAnd -> failure x
  OpPlus -> failure x
  OpMinus -> failure x
  OpMul -> failure x
  OpIntDiv -> failure x
  OpFloatDiv -> failure x
  OpRemainder -> failure x
  OpModulo -> failure x
  OpPower -> failure x
transType :: Type -> Result
transType x = case x of
  TypeBasicType basictype -> failure x
  TypeCompoundType compoundtype -> failure x
transBasicType :: BasicType -> Result
transBasicType x = case x of
  TypeBool -> failure x
  TypeFloat -> failure x
  TypeInt -> failure x
  TypeVoid -> failure x
  TypeChar -> failure x
  TypeString -> failure x
transCompoundType :: CompoundType -> Result
transCompoundType x = case x of
  CompoundTypeArrayType arraytype -> failure x
  CompoundTypePtr ptr -> failure x
transArrayType :: ArrayType -> Result
transArrayType x = case x of
  ArrDefBase pintegers basictype -> failure x
  ArrDefBaseC pintegers basictype -> failure x
  ArrDefPtr pintegers ptr -> failure x
  ArrDefPtrC pintegers ptr -> failure x
transPtr :: Ptr -> Result
transPtr x = case x of
  Pointer basictype -> failure x
  Pointer2Pointer ptr -> failure x

