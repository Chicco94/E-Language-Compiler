module TypeChecker where

import Control.Monad

import qualified Data.Map as Map
import qualified Data.List as List

import AbsE
import PrintE
import ErrM

type Env       = ([Sig], [(BlockType, Context)])           -- Environment
type Sig       = Map.Map String ([(Modality, Type)], Type) -- Functions signature: String -> ([(Modality, Type], Type) 
type Context   = Map.Map String (Mutability, Type)         -- Variables context: String -> (Mutability, Type)

-- Mutability of the variables (i.e., constants or variables).
data Mutability = MutConst | MutVar
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data BlockType = NormBlock | FunBlock PIdent Type | IterBlock | SelBlock
  deriving(Eq, Ord, Show, Read)

-- #######################################
-- 					Core function:
-- #######################################

-- Type checking function.
typeCheck :: Program -> Err (Env, [Program])
typeCheck prog@(PDefs def) = foldM checkDecl (initEnv, []) def

-- #######################################
-- 					Check functions:
-- #######################################

-- Check declaration. 
checkDecl :: (Env, [Program]) -> Decl -> Err (Env,[Program])
checkDecl (env@(sig@(x:xs), (block:_)), prog) def = 
  case def of
    DeclFun lexpr args guard compstmt -> checkFunDecl (env, prog) lexpr args guard compstmt
    DeclStmt stmt -> checkDeclStmt (env, prog) stmt
    _ -> Ok (env, prog)

-- Check function declaration.
checkFunDecl :: (Env, [Program]) -> LExpr -> [Arg] -> Guard -> CompStmt -> Err (Env, [Program])
checkFunDecl (env@(sig@(x:xs), blocks@(block:_)), prog) lexpr@(LExprId pident@(PIdent (p, fname))) args guard compstmt = do
  case guard of
    GuardType t ->
      case lookupFun sig fname of
        Ok t  -> fail $ show p ++ ": function " ++ printTree fname ++ " already declared"
        _ -> do case checkArgDecl (env, prog) args of
              -- call extendEnv putting an empty Signature and a FunBlock block
              -- TODO: add parameters to FunBlock 
                  Ok _ -> do case extendEnv (((Map.empty:(addFun x fname args guard):xs), (((FunBlock pident t), Map.empty):blocks)), prog) compstmt of
                               Ok (e', p') -> Ok (e', postAttach (PDefs [TypedDecl (ADecl t (DeclFun lexpr args guard (StmtBlock (getDecls p'))))]) prog)
                               Bad s -> Bad s
                  Bad s -> Bad s
    GuardVoid -> fail $ show p ++ ": the function " ++ printTree fname ++ " must have a type to be well defined" 

-- Check argument(s) declaration.
-- TODO: check that each expression is declared only once.
checkArgDecl :: (Env, [Program]) -> [Arg] -> Err (Env, [Program])  
checkArgDecl (env, prog) [] = Ok (env, prog) 
checkArgDecl (env@(sig@(x:xs), blocks), prog) (arg@(ArgDecl mod (PIdent pident@(p,ident)) guard):args) = do
  case guard of
    GuardVoid    -> do case lookupVar blocks ident of
                         Ok _ -> checkArgDecl (env, prog) args
                         _    -> fail $ show p ++ ": argument " ++ printTree ident ++ " is not declared!"
    GuardType t1 -> case lookupVar blocks ident of
                     Ok (mut,t2) -> do if mod `isCompatibleWithMutability` mut
                                         then if t1 `isCompatibleWith` t2
                                                then checkArgDecl (env, prog) args
                                                else fail $ show p ++ ": the type of argument " ++ printTree ident ++ " (" ++ show t1 ++ ") is not compatible with " ++ show t2
                                         else fail $ show p ++ ": the modality of " ++ printTree ident ++ " (" ++ show mod ++ ") is not compatible with " ++ show mut
                     _           -> fail $ show p ++ ": argument " ++ printTree ident ++ " is not declared!"

-- Check statement declaration.
checkDeclStmt :: (Env, [Program]) -> Stmt -> Err (Env, [Program])
checkDeclStmt (env, prog) stmt =
  case stmt of
    StmtExpr expr                -> checkExpr (env, prog) expr

    StmtVarDecl lexpr guard         -> checkStmtDecl (env, prog) lexpr guard MutVar
    StmtDefDecl lexpr guard         -> checkStmtDecl (env, prog) lexpr guard MutConst
    StmtVarInit lexpr guard expr    -> checkStmtInit (env, prog) lexpr guard expr MutVar
    StmtDefInit lexpr guard expr    -> checkStmtInit (env, prog) lexpr guard expr MutConst
    StmtVarArrDeclNoDim lexpr guard -> checkStmtArrDeclNoDim (env, prog) lexpr guard MutVar
    StmtDefArrDeclNoDim lexpr guard -> checkStmtArrDeclNoDim (env, prog) lexpr guard MutConst  
    StmtVarArrDecl expr lexpr guard -> checkStmtArrDecl (env, prog) expr lexpr guard MutVar
    StmtDefArrDecl expr lexpr guard -> checkStmtArrDecl (env, prog) expr lexpr guard MutConst  
    --StmtVarArrInit lexpr guard arr  -> checkStmtArrInit (env, prog) lexpr guard arr MutVar
    --StmtDefArrInit lexpr guard arr  -> checkStmtArrInit (env, prog) lexpr guard arr MutConst

    StmtReturn preturn expr      -> checkReturn (env, prog) preturn expr
    StmtNoReturn preturn         -> checkNoReturn (env, prog) preturn

    StmtBreak pbreak             -> checkBreak (env, prog) pbreak
    StmtContinue pcontinue       -> checkContinue (env, prog) pcontinue
    
    SComp compstmt               -> checkSComp (env, prog) compstmt

    StmtIfThenElse expr cstmt1 cstmt2 -> checkIfThenElse (env, prog) expr cstmt1 cstmt2
    StmtIfThen expr cstmt -> checkIfThen (env, prog) expr cstmt

    StmtWhile expr cstmt         -> checkWhile (env, prog) expr cstmt
    _ -> Bad "checkStmt: fatal error!"

-- Check if-then-else statement.
checkIfThenElse :: (Env, [Program]) -> Expr -> CompStmt -> CompStmt -> Err (Env, [Program]) 
checkIfThenElse (env@(sig@(x:xs), blocks), prog) expr cstmt1 cstmt2 = do
  texpr <- inferExpr env expr
  if texpr == TypeBool 
    then do
      (env1, p1) <- extendEnv ((sig, ((SelBlock, Map.empty):blocks)), prog) cstmt1
      (env2, p2) <- extendEnv ((sig, ((SelBlock, Map.empty):blocks)), p1) cstmt2
      Ok (env, postAttach (PDefs [DeclStmt (StmtIfThenElse expr (StmtBlock (getDecls p1)) (StmtBlock (getDecls p2)))]) prog)
    else
      fail $ show (getExprPosition expr) ++ ": the expression " ++ printTree expr ++ " must be boolean"

-- Check if-then statement.
checkIfThen :: (Env, [Program]) -> Expr -> CompStmt -> Err (Env, [Program]) 
checkIfThen (env@(sig@(x:xs), blocks), prog) expr cstmt = do
  texpr <- inferExpr env expr
  if texpr == TypeBool 
    then do
      (env1, p1) <- extendEnv ((sig, ((SelBlock, Map.empty):blocks)), prog) cstmt
      Ok (env, postAttach (PDefs [DeclStmt (StmtIfThen expr (StmtBlock (getDecls p1)))]) prog)
    else
      fail $ show (getExprPosition expr) ++ ": the expression " ++ printTree expr ++ " must be boolean"

-- Check return with expression statement.
checkReturn :: (Env, [Program]) -> PReturn -> Expr -> Err (Env, [Program])
checkReturn (env@(sigs, blocks), prog) preturn@(PReturn (pr, _)) expr = do
  texpr <- inferExpr env expr
  case findFunBlockAndType blocks of
    Ok(PIdent (pf, fname), tfun) -> 
      if (texpr `isCompatibleWith` tfun) 
        -- If the type of the returned expression (texpr) is compatible with the type of the function (tfun), 
        -- then the returned type is tfun 
        then Ok (env, postAttach (PDefs [TypedDecl (ADecl tfun (DeclStmt (StmtReturn preturn expr)))]) prog)
        else fail $ show pr ++ ": the returned type is " ++ show texpr ++ ", but function " ++ printTree fname ++ " (declared at " ++ show pf ++ ") has return type " ++ show tfun
    _                            -> fail $ show pr ++ ": the return statement must be inside a function block"

-- Check return without expression statement.
checkNoReturn :: (Env, [Program]) -> PReturn -> Err (Env, [Program])
checkNoReturn (env@(sigs, blocks), prog) preturn@(PReturn (pr, _)) = do
  case findFunBlockAndType blocks of
    Ok (PIdent (pf, fname), tfun) ->
      if (tfun == TypeVoid)
        then Ok (env, postAttach (PDefs [TypedDecl (ADecl tfun (DeclStmt (StmtNoReturn preturn)))]) prog)
        else fail $ show pr ++ ": the returned type is " ++ show TypeVoid ++ ", but function " ++ printTree fname ++ " (declared at " ++ show pf ++ ") has return type " ++ show tfun
    _                            -> fail $ show pr ++ ": the return statement must be inside a function declaration"

-- Check break statement.
checkBreak :: (Env, [Program]) -> PBreak -> Err (Env, [Program])
checkBreak (env@(sigs, blocks), prog) pbreak@(PBreak (pb, _)) = do
  case findIterBlock blocks of
    Ok _ -> Ok (env, postAttach (PDefs [DeclStmt (StmtBreak pbreak)]) prog)
    _    -> fail $ show pb ++ ": the break statement must be inside an iteration block"

-- Check continue statement.
checkContinue :: (Env, [Program]) -> PContinue -> Err (Env, [Program])
checkContinue (env@(sigs, blocks), prog) pcontinue@(PContinue (pc, _)) = do
  case findIterBlock blocks of
    Ok _ -> Ok (env, postAttach (PDefs [DeclStmt (StmtContinue pcontinue)]) prog)
    _    -> fail $ show pc ++ ": the continue statement must be inside an iteration block"

-- Check compound statement.
checkSComp :: (Env, [Program]) -> CompStmt -> Err (Env, [Program])
checkSComp (env@(sigs, blocks), prog) compstmt =
  case extendEnv (((Map.empty:sigs), ((NormBlock, Map.empty):blocks)), prog) compstmt of
    Ok (e', p') -> Ok (e', postAttach (PDefs [DeclStmt (SComp (StmtBlock (getDecls p')))]) prog)
    Bad s       -> Bad s

-- Check while statement.
checkWhile :: (Env, [Program]) -> Expr -> CompStmt -> Err (Env, [Program])
checkWhile (env@(xs, blocks@((blockType, context):ys)), prog) expr compstmt = do
  t <- inferExpr env expr
  if (t == TypeBool)
    then case extendEnv (((Map.empty:xs), ((IterBlock, Map.empty):blocks)), prog) compstmt of
           Ok (e', p') -> Ok (e', postAttach (PDefs [DeclStmt (StmtWhile expr (StmtBlock (getDecls p')))]) prog)
           Bad s       -> Bad s
    else fail $ show (getExprPosition expr) ++ ": the expression " ++ printTree expr ++ " must be boolean"

-- Check constant/variable declaration statement.
checkStmtDecl :: (Env, [Program]) -> LExpr -> Guard -> Mutability -> Err (Env, [Program])
checkStmtDecl (env@(sig, blocks@((blockType, context):xs)), prog) lexpr@(LExprId (PIdent (p, ident))) guard mut = do
  case guard of
    GuardType t -> do
      if t == TypeVoid
        then fail $ show (getLExprPosition lexpr) ++ ": variable " ++ printTree ident ++ " must have a type (" ++ show TypeVoid ++ " is not allowed)"
        else
          case lookupVar blocks ident of
            Ok (m,t) -> fail $ show p ++ ": variable " ++ printTree ident ++ " already declared"
            _        -> do case mut of
                             MutVar   -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtVarDecl lexpr guard)))]) prog)
                             MutConst -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtDefDecl lexpr guard)))]) prog)
    GuardVoid -> fail $ show (getLExprPosition lexpr) ++ ": variable " ++ printTree ident ++ " must have a type (" ++ show TypeVoid ++ " is not allowed)"

-- Check constant/variable initialization statement.
checkStmtInit :: (Env, [Program]) -> LExpr -> Guard -> Expr -> Mutability -> Err (Env, [Program])
checkStmtInit (env@(sig, blocks@((blockType, context):xs)), prog) lexpr@(LExprId (PIdent (p, ident))) guard expr mut = do
  case guard of
    GuardType t -> do
      if t == TypeVoid
        then fail $ show (getLExprPosition lexpr) ++ ": variable " ++ printTree ident ++ " must have a type (" ++ show TypeVoid ++ " is not allowed)"
        else 
          case lookupVar blocks ident of
            Ok (m,t) -> fail $ show p ++ ": variable " ++ printTree ident ++ " already declared"
            _        -> do case mut of
                             MutVar   -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtVarInit lexpr guard expr)))]) prog)
                             MutConst -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtDefInit lexpr guard expr)))]) prog)
    GuardVoid -> fail $ show (getLExprPosition lexpr) ++ ": variable " ++ printTree ident ++ " must have a type (" ++ show TypeVoid ++ " is not allowed)"

-- Check constant/variable array without dimension declaration statement.
checkStmtArrDeclNoDim :: (Env, [Program]) -> LExpr -> Guard -> Mutability -> Err (Env, [Program])
checkStmtArrDeclNoDim (env@(sig, blocks@((blockType, context):xs)), prog) lexpr@(LExprId (PIdent (p, ident))) guard mut = do
  case guard of
    GuardType t -> do
      if t == TypeVoid 
        then fail $ show (getLExprPosition lexpr) ++ ": variable " ++ printTree ident ++ " must have a type (" ++ show TypeVoid ++ " is not allowed)"
        else
          case lookupVar blocks ident of
            Ok (m,t) -> fail $ show p ++ ": variable " ++ printTree ident ++ " already declared"
            _        -> do case mut of
                             MutVar   -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtVarArrDeclNoDim lexpr guard)))]) prog)
                             MutConst -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtDefArrDeclNoDim lexpr guard)))]) prog)
    GuardVoid -> fail $ show (getLExprPosition lexpr) ++ ": variable " ++ printTree ident ++ " must have a type (" ++ show TypeVoid ++ " is not allowed)"

-- Check constant/variable array with dimension declaration statement.
checkStmtArrDecl :: (Env, [Program]) -> Expr -> LExpr -> Guard -> Mutability -> Err (Env, [Program])
checkStmtArrDecl (env@(sig, blocks@((blockType, context):xs)), prog) expr lexpr@(LExprId (PIdent (p, ident))) guard mut = do
  case inferExpr env expr of
    Ok TypeInt -> do 
      case guard of
        GuardType t -> do
          if t == TypeVoid 
            then fail $ show p ++ ": variable " ++ printTree ident ++ " must have a type (" ++ show TypeVoid ++ " is not allowed)"
            else
              case lookupVar blocks ident of
                Ok (m,t) -> fail $ show p ++ ": variable " ++ printTree ident ++ " already declared"
                _        -> do case mut of
                                 MutVar   -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtVarArrDecl expr lexpr guard)))]) prog)
                                 MutConst -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtDefArrDecl expr lexpr guard)))]) prog)
        GuardVoid -> fail $ show p ++ ": variable " ++ printTree ident ++ " must have a type (" ++ show TypeVoid ++ " is not allowed)"
    _ -> fail $ show (getExprPosition expr) ++ ": the expression " ++ printTree expr ++ " must have type " ++ show TypeInt

--checkStmtArrInit (env, prog) lexpr guard arr MutConst
--checkStmtArrInit :: (Env, [Program]) -> LExpr -> Guard -> Array -> Mutability -> Err (Env, [Program])
--checkStmtArrInit (env@(sig, blocks@((blockType, context):xs)), prog) lexpr@(LExprId (PIdent (p, ident))) guard arr mut = do


-- Check assignment.
checkAssign :: (Env, [Program]) -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
checkAssign (env, prog) lexpr op expr = do
  t1 <- inferLExpr env lexpr
  case op of
    -- Normal assignment (i.e., ':=').
    OpAssign      -> do checkAssignOp (env, prog)  t1 lexpr op expr

    -- Assignment with boolean operator (i.e., '|=', '&=').
    OpOr          -> do checkAssignBoolOp (env, prog) t1 lexpr op expr
    OpAnd         -> do checkAssignBoolOp (env, prog) t1 lexpr op expr
    
    -- Assignment with arithmetic operator (e.g., '+=', '%%=').
    OpPlus        -> do checkAssignOp (env, prog)  t1 lexpr op expr
    OpMinus       -> do checkAssignOp (env, prog)  t1 lexpr op expr
    OpMul         -> do checkAssignOp (env, prog)  t1 lexpr op expr
    OpIntDiv      -> do checkAssignDiv (env, prog)  t1 TypeInt lexpr op expr
    OpDoubleDiv    -> do checkAssignDiv (env, prog)  t1 TypeDouble lexpr op expr
    OpRemainder   -> do checkAssignOp (env, prog)  t1 lexpr op expr
    OpModulo      -> do checkAssignOp (env, prog)  t1 lexpr op expr
    OpPower       -> do checkAssignOp (env, prog)  t1 lexpr op expr  

-- Check assignment with boolean operator.
checkAssignBoolOp :: (Env, [Program]) -> Type -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
checkAssignBoolOp (env, prog) t1 lexpr op expr = do
  t2 <- inferExpr env expr
  if (t2 `isCompatibleWith` t1) -- t2 has to be compatible with t1 (i.e., the r-expr with the l-expr)
    then Ok (env, postAttach (PDefs [TypedDecl (ADecl TypeBool (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))]) prog)
    else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ show t1 ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ show t2 

-- Check assignment operator.
checkAssignOp :: (Env, [Program]) -> Type -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
checkAssignOp (env, prog) t1 lexpr op expr = do
  t2 <- inferExpr env expr
  if (t2 `isCompatibleWith` t1)
    then Ok (env, postAttach (PDefs [TypedDecl (ADecl t1 (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))]) prog)
    else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ show t1 ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ show t2 

-- Check assignment operator.
checkAssignDiv :: (Env, [Program]) -> Type -> Type -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
checkAssignDiv (env, prog) t1 top lexpr op expr = do
  t2 <- inferExpr env expr
  if (t1 `isCompatibleWith` top) && 
     (t2 `isCompatibleWith` top) && 
     areCompatible t1 t2 -- it is necessary?
    then Ok (env, postAttach (PDefs [TypedDecl (ADecl t1 (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))]) prog)
    else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ show t1 ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ show t2 

-- Check function call.
checkFunCall :: (Env, [Program]) -> Expr -> Err (Env, [Program])
checkFunCall (env@(sig, _), prog) (ExprFunCall (PIdent pident@(p,ident)) args) = do
  case lookupFun sig ident of
    Ok funDef@(mods,t) -> if length args == length mods -- !! doesn't work..
                          then Ok (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr (ExprFunCall (PIdent pident) args))))]) prog)
                          else fail $ show p ++ ": function '" ++ printTree ident ++ " doesn't match its parameters"
    _ -> fail $ show p ++ ": function '" ++ printTree ident ++ "' is not defined!" 

-- Check (right) expression.
checkExpr :: (Env, [Program]) -> Expr -> Err (Env, [Program])
checkExpr (env, prog) expr = do
  case expr of
    -- Assignment statement (i.e., lexpr operator expr).
    ExprAssign lexpr op expr -> do checkAssign (env, prog) lexpr op expr

    -- Left expressions.
    LeftExpr lexpr           -> do t <- inferLExpr env lexpr
                                   return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr (LeftExpr lexpr))))])prog) 

    -- Base expressions (i.e., integers, floats, chars, strings, booleans)
    ExprInt int              -> return (env, postAttach (PDefs [TypedDecl (ADecl TypeInt (DeclStmt (StmtExpr (ExprInt int))))]) prog)
    ExprDouble dbl           -> return (env, postAttach (PDefs [TypedDecl (ADecl TypeDouble (DeclStmt (StmtExpr (ExprDouble dbl))))]) prog)
    ExprChar chr             -> return (env, postAttach (PDefs [TypedDecl (ADecl TypeChar (DeclStmt (StmtExpr (ExprChar chr))))]) prog)
    ExprString str           -> return (env, postAttach (PDefs [TypedDecl (ADecl TypeString (DeclStmt (StmtExpr (ExprString str))))]) prog)
    ExprTrue ptrue           -> return (env, postAttach (PDefs [TypedDecl (ADecl TypeBool (DeclStmt (StmtExpr (ExprTrue ptrue))))]) prog)
    ExprFalse pfalse         -> return (env, postAttach (PDefs [TypedDecl (ADecl TypeBool (DeclStmt (StmtExpr (ExprFalse pfalse))))]) prog)

    -- Function call.
    ExprFunCall pident args  -> do checkFunCall (env, prog) expr

    -- Boolean not.
    ExprBoolNot e            -> do t <- inferBoolUnOp env expr e
                                   return (env, postAttach (PDefs [TypedDecl (ADecl TypeBool (DeclStmt (StmtExpr expr)))]) prog)

    -- Arithmetic unary operators.
    ExprAddition e           -> do t <- inferArithUnOp env expr e
                                   return (env, postAttach (PDefs [TypedDecl (ADecl TypeBool (DeclStmt (StmtExpr expr)))]) prog)  
    ExprNegation e           -> do t <- inferArithUnOp env expr e
                                   return (env, postAttach (PDefs [TypedDecl (ADecl TypeBool (DeclStmt (StmtExpr expr)))]) prog)    

    -- Arithmetic binary operators.
    ExprPower e1 e2    -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprMul e1 e2      -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprDoubleDiv e1 e2 -> do t <- inferArithBinOpDiv env TypeDouble expr e1 e2
                              return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprIntDiv e1 e2   -> do t <- inferArithBinOpDiv env TypeInt expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprReminder e1 e2 -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprModulo e1 e2   -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprPlus e1 e2     -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprMinus e1 e2    -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)

    -- Interval creation. (Recall that an interval is an iterable.)
    --ExprIntInc e1 e2   -> checkInterval (env, prog) expr e1 e2
    --ExprIntExc e1 e2   -> checkInterval (env, prog) expr e1 e2

    -- Relational binary operators.
    ExprLt e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprGt e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprLtEq e1 e2     -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprGtEq e1 e2     -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprEq e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprNeq e1 e2      -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)

    -- Boolean binary operator.
    ExprAnd e1 e2      -> do t <- inferBoolBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)
    ExprOr e1 e2       -> do t <- inferBoolBinOp env expr e1 e2
                             return (env, postAttach (PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))]) prog)

-- Check interval creation.
{- checkInterval :: (Env, [Program]) -> Expr -> Expr -> Expr -> Err (Env, [Program])
checkInterval (env, prog) expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 == TypeInt) && (t2 == TypeInt) -- only ints are allowed.
    then Ok (env, postAttach (PDefs [TypedDecl (ADecl t1 (DeclStmt (StmtExpr expr)))]) prog)
    else fail $ "only integer intervals allowed!" -}

-- #######################################
-- 					Inference functions:
-- #######################################

-- Infer (right) expression.
inferExpr :: Env -> Expr -> Err Type
inferExpr env expr =
  case expr of
    -- Left expressions. 
    LeftExpr lexpr -> inferLExpr env lexpr 

    -- Base expressions (i.e., integers, floats, chars, strings, booleans) 
    ExprTrue _    -> Ok TypeBool
    ExprFalse _   -> Ok TypeBool
    ExprInt _     -> Ok TypeInt
    ExprDouble _  -> Ok TypeDouble
    ExprChar _    -> Ok TypeChar
    ExprString _  -> Ok TypeString
    
    -- Function call.
    ExprFunCall pident args -> inferFunCall env expr

    -- Boolean not.
    ExprBoolNot e      -> inferBoolUnOp env expr e

    -- Arithmetic unary operators.
    ExprAddition e           -> inferArithUnOp env expr e
    ExprNegation e           -> inferArithUnOp env expr e

    -- Arithmetic binary operators.
    ExprPower e1 e2    -> inferArithBinOp env expr e1 e2
    ExprMul e1 e2      -> inferArithBinOp env expr e1 e2
    ExprDoubleDiv e1 e2 -> inferArithBinOpDiv env TypeDouble expr e1 e2
    ExprIntDiv e1 e2   -> inferArithBinOpDiv env TypeInt expr e1 e2
    ExprReminder e1 e2 -> inferArithBinOp env expr e1 e2
    ExprModulo e1 e2   -> inferArithBinOp env expr e1 e2
    ExprPlus e1 e2     -> inferArithBinOp env expr e1 e2
    ExprMinus e1 e2    -> inferArithBinOp env expr e1 e2

    -- Relational binary operators.
    ExprLt e1 e2       -> inferRelBinOp env expr e1 e2
    ExprGt e1 e2       -> inferRelBinOp env expr e1 e2
    ExprLtEq e1 e2     -> inferRelBinOp env expr e1 e2
    ExprGtEq e1 e2     -> inferRelBinOp env expr e1 e2
    ExprEq e1 e2       -> inferRelBinOp env expr e1 e2

   -- Boolean binary operator.
    ExprAnd e1 e2      -> inferBoolBinOp env expr e1 e2
    ExprOr e1 e2       -> inferBoolBinOp env expr e1 e2
    _ -> fail $ "inferExpr: fatal error"

-- Infer function call.
inferFunCall :: Env -> Expr -> Err Type
inferFunCall env@(sig, blocks) (ExprFunCall (PIdent pident@(p,ident)) args) = do
  case lookupFun sig ident of
    Ok (mods,t) -> do
      if length args == length mods
        then Ok t
        else fail $ show p ++ ": function " ++ printTree ident ++ " doesn't match its parameters"
    _ -> fail $ show p ++ ": function " ++ printTree ident ++ " not defined!" 

-- Infer left expression.
inferLExpr :: Env -> LExpr -> Err Type
inferLExpr env@(sig, blocks) lexpr = do
  case lexpr of
    LExprId (PIdent (p, ident)) -> 
      case lookupVar blocks ident of
        Ok (_,t) -> Ok t
        _ -> fail $ show p ++ ": variable " ++ printTree ident ++ " non defined!"

    LExprDeref (LDerefExpr le)   -> inferLExpr env le
    LExprRef (LRefExpr le)       -> inferLExpr env le
    LExprArr (LArrExpr le re)    -> do tle <- inferLExpr env le
                                       tre <- inferExpr env re
                                       if (tre == TypeInt)
                                         then Ok tle
                                         else fail $ show (getLExprPosition le) ++ ": the type of the array " ++ printTree le ++ " is " ++ show tle ++ " but the inner expression " ++ printTree re ++ " has type " ++ show tre
   -- _                                  -> fail $ "inferLEXpr: fatal error"

-- Infer arithmetic unary operator.
inferArithUnOp :: Env -> Expr -> Expr -> Err Type
inferArithUnOp env expr e = do
  t <- inferExpr env e
  if (t == TypeInt) || (t == TypeDouble) -- only ints and floats are allowed
    then Ok t
    else fail $ show (getExprPosition e) ++ ": " ++ printTree e ++ " has type " ++ show t ++ " which must be " ++ show [TypeInt, TypeDouble]

-- Infer arithmetic binary operator.
inferArithBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
inferArithBinOp env expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 `isCompatibleWithAny` [TypeInt, TypeDouble]) && -- only ints and floats are allowed
     (t2 `isCompatibleWithAny` [TypeInt, TypeDouble]) &&
     areCompatible t1 t2
     then getMostGeneric t1 t2
     else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ show t1 ++ " and " ++ printTree e2 ++ " " ++ show (getExprPosition e2) ++ " having type " ++ show t2

-- Infer arithmetic integer division or float division operator.
inferArithBinOpDiv :: Env -> Type -> Expr -> Expr -> Expr -> Err Type
inferArithBinOpDiv env top expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 `isCompatibleWithAny` [TypeInt, TypeDouble]) && -- only ints and floats in divisions
     (t2 `isCompatibleWithAny` [TypeInt, TypeDouble]) &&
     areCompatible t1 t2
     then Ok top
     else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ show t1 ++ " and " ++ printTree e2 ++ " " ++ show (getExprPosition e2) ++ " having type " ++ show t2

-- Infer relational binary operator.
inferRelBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
inferRelBinOp env expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 `isCompatibleWithAny` [TypeInt, TypeDouble, TypeChar, TypeString]) &&  -- only ints, floats, chars and strings are allowed
     (t2 `isCompatibleWithAny` [TypeInt, TypeDouble, TypeChar, TypeString]) &&  -- the same as above
     areCompatible t1 t2
     then Ok TypeBool
     else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ show t1 ++ " and " ++ printTree e2 ++ " " ++ show (getExprPosition e2) ++ " having type " ++ show t2

-- Infer boolean unary operator.
inferBoolUnOp :: Env -> Expr -> Expr -> Err Type
inferBoolUnOp env expr e = do
  t <- inferExpr env e
  if (t == TypeBool) -- only booleans are allowed
    then Ok t
    else fail $ show (getExprPosition e) ++ ": " ++ printTree e ++ " has type " ++ show t ++ " which must be boolean"

-- Infer boolean binary operator.
inferBoolBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
inferBoolBinOp env expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 == TypeBool) && (t2 == TypeBool) -- only booleans are allowed
    then Ok TypeBool  
    else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ show t1 ++ " and " ++ printTree e2 ++ " " ++ show (getExprPosition e2) ++ " having type " ++ show t2


-- #######################################
-- 					Auxiliary functions:
-- #######################################

-- Default given primitives.
defaultPrimitives :: Sig
defaultPrimitives = 
  Map.insert "writeInt"    ([(ModEmpty, TypeInt)], TypeVoid) $     -- void writeInt(int i)
  Map.insert "writeFloat"  ([(ModEmpty, TypeDouble)], TypeVoid) $  -- void writeFloat(double d)
  Map.insert "writeChar"   ([(ModEmpty, TypeChar)], TypeVoid) $    -- void writeChar(char c)
  Map.insert "writeString" ([(ModEmpty, TypeString)], TypeVoid) $  -- void writeString(string s)
  Map.insert "readInt"     ([], TypeInt) $                         -- int readInt()
  Map.insert "readDouble"  ([], TypeDouble) $                      -- double readFloat()
  Map.insert "readChar"    ([], TypeChar) $                        -- char readChar()
  Map.insert "readString"  ([], TypeString) Map.empty              -- string readString()

-- Initialize the environment with the default primitives.
initEnv :: Env
initEnv = ([defaultPrimitives], [(NormBlock, Map.empty)])

-- Given an element 'a' and a list, add this element at the end of the list.
postAttach :: a -> [a] -> [a]
postAttach a [] = [a]
postAttach a (x:xs) = x : postAttach a xs

-- Look for function in signature.
lookupFun :: [Sig] -> String -> Err ([(Modality, Type)], Type)
lookupFun [] _ = Bad "function is not declared!"
lookupFun sig@(x:xs) ident =
  case Map.lookup ident x of
    Just t -> Ok t
    Nothing -> lookupFun xs ident

-- Look for variable/constant in blocks.
lookupVar :: [(BlockType, Context)] -> String -> Err (Mutability, Type)
lookupVar [] _ = Bad "variable is not declared!"
lookupVar ((blockType, context):xs) ident = 
  case Map.lookup ident context of
    Just (m,t) -> Ok (m,t)
    Nothing -> lookupVar xs ident

-- Add function to signature.
addFun :: Sig -> String -> [Arg] -> Guard -> Sig
addFun sig fname args guard = 
  case guard of
    GuardType retType -> Map.insert fname ([(mod, t) | (ArgDecl mod _ (GuardType t)) <- args], retType) sig 
    GuardVoid -> Map.insert fname ([(mod, t) | (ArgDecl mod _ (GuardType t)) <- args], TypeVoid) sig -- procedures

-- Add variable/constant to signature.
addVar :: Context -> (Mutability, String) -> Guard -> Context
addVar context (m,ident) (GuardType t) = Map.insert ident (m,t) context

-- Type compatibilities:
-- bool < char < int < float
--             < string

-- T1 is compatible with T2?
isCompatibleWith :: Type -> Type -> Bool
t1 `isCompatibleWith` t2 
  | t1 == t2       = True
  | t1 == TypeBool = elem t2 [TypeChar, TypeString, TypeInt, TypeDouble]
  | t1 == TypeChar = elem t2 [TypeString, TypeInt, TypeDouble]
  | t1 == TypeInt  = t2 == TypeDouble
  | otherwise      = False

-- T is compatible with any of [T1,..Tn]?
isCompatibleWithAny :: Type -> [Type] -> Bool
t `isCompatibleWithAny` types = any (isCompatibleWith t) types

-- Are T1 and T2 compatible ?
areCompatible :: Type -> Type -> Bool
areCompatible t1 t2 = (t1 `isCompatibleWith` t2) || (t2 `isCompatibleWith` t1)

-- Get most generic type in between T1 and T2.
getMostGeneric :: Type -> Type -> Err Type
getMostGeneric t1 t2
  | t1 == t2                  = Ok t1
  | TypeDouble `elem` [t1,t2] = Ok TypeDouble
  | TypeInt `elem` [t1,t2]    = Ok TypeInt
  | TypeString `elem` [t1,t2] = Ok TypeString
  | TypeChar `elem` [t1,t2]   = Ok TypeChar

-- MOD is compatible with mutability MUT?
isCompatibleWithMutability :: Modality -> Mutability -> Bool
mod `isCompatibleWithMutability` mut
  | mod == ModEmpty                  = True  -- if the argument has no modality, then it can be anything
  | mod == ModVar                    = True  -- if the argument is a variable, then it can be anything
  | mod == ModDef && mut == MutConst = True  -- if the argument is a constant defined as constant, then it is ok
  | mod == ModDef && mut == MutVar   = True  -- if the argument is a constant that is defined as variable, then it is ok
  | otherwise                        = False -- otherwise, not compatible

getDecls :: [Program] -> [Decl]
getDecls [] = []
getDecls (prog:progs) = case prog of
  PDefs [TypedDecl (ADecl t s)] -> (TypedDecl (ADecl t s)) : getDecls progs
  PDefs [DeclStmt stmt] -> (DeclStmt stmt) : getDecls progs 
  _ -> []

getExprPosition :: Expr -> (Int, Int)
getExprPosition e =
  case e of
    ExprInt (PInteger (p, _))   -> p
    ExprDouble (PDouble (p, _)) -> p
    ExprChar (PChar (p, _))     -> p
    ExprString (PString (p, _)) -> p
    ExprTrue (PTrue (p, _))     -> p
    ExprFalse (PFalse (p, _))   -> p

    ExprAssign lexpr op rexpr -> getLExprPosition lexpr
    LeftExpr lexpr            -> getLExprPosition lexpr

    ExprBoolNot rexpr         -> getExprPosition rexpr
    ExprNegation rexpr        -> getExprPosition rexpr
    ExprAddition rexpr        -> getExprPosition rexpr
    
    ExprPower e1 e2           -> getExprPosition e1
    ExprMul e1 e2             -> getExprPosition e1
    ExprDoubleDiv e1 e2       -> getExprPosition e1
    ExprIntDiv e1 e2          -> getExprPosition e1
    ExprReminder e1 e2        -> getExprPosition e1
    ExprModulo e1 e2          -> getExprPosition e1

    ExprPlus e1 e2            -> getExprPosition e1
    ExprMinus e1 e2           -> getExprPosition e1
 
    ExprLt e1 e2              -> getExprPosition e1
    ExprGt e1 e2              -> getExprPosition e1
    ExprLtEq e1 e2            -> getExprPosition e1
    ExprGtEq e1 e2            -> getExprPosition e1

    ExprEq e1 e2              -> getExprPosition e1
    ExprNeq e1 e2             -> getExprPosition e1

    ExprAnd e1 e2             -> getExprPosition e1
    ExprOr e1 e2              -> getExprPosition e1
 
getLExprPosition :: LExpr -> (Int, Int)
getLExprPosition e =
  case e of
    LExprId (PIdent (p, ident)) -> p
    LExprDeref (LDerefExpr le)  -> getLExprPosition le
    LExprRef (LRefExpr le)      -> getLExprPosition le
    LExprArr (LArrExpr le re)   -> getLExprPosition le    

-- Find function block and its type, if exists.
findFunBlockAndType :: [(BlockType, Context)] -> Err (PIdent, Type)
findFunBlockAndType [] = fail $ "there is no function block declared"
findFunBlockAndType (block@(blockType, context):blocks) = 
  case blockType of
    FunBlock pident t -> Ok (pident, t)
    _                 -> findFunBlockAndType blocks

-- Find iteration block, if exists.
findIterBlock :: [(BlockType, Context)] -> Err ()
findIterBlock [] = fail $ "there is no iteration block declared"
findIterBlock (block@(blockType, context):blocks) = 
  case blockType of
    IterBlock -> Ok ()
    _         -> findIterBlock blocks

extendEnv :: (Env, [Program]) -> CompStmt -> Err (Env, [Program])
extendEnv (env@(s, y), prog) (StmtBlock decls) = do 
  (e@((sig:sigs), b@(block:blocks)), p) <- foldM checkDecl (env, prog) decls
  Ok ((sigs,blocks), p List.\\ prog) -- pop & list difference   
