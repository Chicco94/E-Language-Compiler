module TypeChecker where

import Control.Monad

--import Data.Map (Map)
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

data BlockType = NormBlock | FunBlock | IterBlock
  deriving(Eq, Ord, Show, Read, Enum, Bounded)

--emptyEnv :: Env
--emptyEnv = ([Map.empty], [(NormBlock, Map.empty)]) :: ([Sig], [(BlockType, Context)])

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

-- Type checking function.
typeCheck :: Program -> Err (Env, [Program])
typeCheck prog@(PDefs def) = foldM checkDecl (initEnv, []) def

-- Check declaration. 
checkDecl :: (Env, [Program]) -> Decl -> Err (Env,[Program])
checkDecl (env@(sig@(x:xs), (block:_)), prog) def = 
  case def of
    DeclFun lexpr args guard compstmt -> checkFunDecl (env, prog) lexpr args guard compstmt
    DeclStmt stmt -> checkDeclStmt (env, prog) stmt
    --_ -> Ok (env, prog)

-- Check function declaration.
checkFunDecl :: (Env, [Program]) -> LExpr -> [Arg] -> Guard -> CompStmt -> Err (Env, [Program])
checkFunDecl (env@(sig@(x:xs), blocks@(block:_)), prog) lexpr@(LExprId (PIdent (p, fname))) args guard compstmt = do
  case guard of
    GuardType t ->
      case lookupFun sig fname of
        Ok t  -> fail $ show p ++ ": function " ++ printTree fname ++ " already declared"
        _ -> do case checkArgDecl (env, prog) args of
              -- call extendEnv putting an empty Signature and a FunBlock block
              -- TODO: add parameters to FunBlock 
                  Ok _ -> do case extendEnv (((Map.empty:(addFun x fname args guard):xs), ((FunBlock, Map.empty):blocks)), prog) compstmt of
                               Ok (e', p') -> Ok (e', postAttach (PTDefs [ADecl t (DeclFun lexpr args guard (StmtBlock (getDecls p')))]) prog)
                               Bad s -> Bad s
                               _ -> fail $ "checkFunDecl: fatal error!"
                  Bad s -> Bad s
    GuardVoid -> fail $ show p ++ ": the function " ++ printTree fname ++ " has to have a type to be well defined!"





    --_ -> Ok (([addFun x fname args guard], [block]), postAttach (PTDefs [TypedDecl t (DeclFun lexpr args guard stmts)]) prog)

              --Ok _  -> Ok (([addFun x fname args guard], [block]), postAttach (PTDefs [TypedDecl t (DeclFun lexpr args guard stmts)]) prog) 
              {-Ok _  -> do Ok (env', postAttach (PTDefs [TypedDecl t (DeclFun lexpr args guard [labeledstmts])]) prog)
                         where --env' = (addFun x fname args guard, ((FunBlock, Map.empty):blocks))
                               Ok (env',(PTDefs [TypedDecl t (DeclStmt labeledstmts)])) = foldM checkDeclStmt (([addFun x fname args guard], ((FunBlock, Map.empty):blocks)), prog) stmts-}

--case extendEnv (([addFun x fname args guard], ((FunBlock, Map.empty):blocks)), prog) stmts of
                           --Ok (e',p'@([PTDefs [TypedDecl t' (DeclStmt lbl)]])) -> Ok (e',[PTDefs [TypedDecl t' (DeclFun lexpr args guard [lbl])]])
--getProgramType :: [Program] -> Type

getDecls :: [Program] -> [Decl]
getDecls [] = []
getDecls (prog:progs) = case prog of
  PTDefs [ADecl t s] -> (TypedDecl (ADecl t s)) : getDecls progs
  _ -> []

{- getStatements :: [Program] -> [Decl]
getStatements [] = []
getStatements (prog:progs) = case prog of
  PTDefs [TypedDecl t (DeclStmt (SComp (StmtBlock [DeclStmt s])))] -> s : getStatements progs
  _ -> [] -}
 
--let env' = (addFun x fname args guard, ((FunBlock, Map.empty):blocks)) -- add function to the environment
-- the argument declaration succeeded, add the function to the signature (TODO: add block, etc.)

extendEnv :: (Env, [Program]) -> CompStmt -> Err (Env, [Program])
extendEnv (env@(s, y), prog) compstmt = do
  case compstmt of
    StmtBlock decls         -> do (e@((sig:sigs), b@(block:blocks)), p) <- foldM checkDecl (env, prog) decls
                                  Ok ((sigs,blocks), p List.\\ prog) -- pop & list difference
                                  --Ok ((sigs,blocks), p)
                                  --Ok (e, p List.\\ prog)      

-- Check argument(s) declaration.
checkArgDecl :: (Env, [Program]) -> [Arg] -> Err (Env, [Program])  
checkArgDecl (env, prog) [] = Ok (env, prog) 
checkArgDecl (env@(sig@(x:xs), blocks), prog) (arg@(ArgDecl mod (PIdent pident@(p,ident)) guard):args) = do
  case guard of
    GuardVoid    -> do case lookupVar blocks ident of
                         Ok _ -> checkArgDecl (env, prog) args
                         _    -> fail $ show p ++ ": argument " ++ printTree ident ++ " is not declared!"
--fail $ show p ++ ": argument " ++ printTree ident ++ " cannot be declared as void!" -- TODO: if it is void, check for type if it exists
    GuardType t1 -> case lookupVar blocks ident of
                     Ok (mut,t2) -> do if mod `isCompatibleWithMutability` mut
                                         then if t1 `isCompatibleWith` t2
                                                then checkArgDecl (env, prog) args
                                                else fail $ show p ++ ": the type of argument " ++ printTree ident ++ " (" ++ show t1 ++ ") is not compatible with " ++ show t2
                                         else fail $ show p ++ ": the modality of " ++ printTree ident ++ " (" ++ show mod ++ ") is not compatible with " ++ show mut
                     _           -> fail $ show p ++ ": argument " ++ printTree ident ++ " is not declared!"

-- MOD is compatible with mutability MUT?
isCompatibleWithMutability :: Modality -> Mutability -> Bool
mod `isCompatibleWithMutability` mut
  | mod == ModEmpty                  = True  -- if the argument has no modality, then it can be anything
  | mod == ModVar                    = True  -- if the argument is a variable, then it can be anything
  | mod == ModDef && mut == MutConst = True  -- if the argument is a constant defined as constant, then it is ok
  | mod == ModDef && mut == MutVar   = True  -- if the argument is a constant that is defined as variable, then it is ok
  | otherwise                        = False -- otherwise, not compatible

--(([addFun x fname args guard], [block]), postAttach (PTDefs [TypedDecl t (DeclFun lexpr args guard stmts)]) prog)

-- Check statement declaration.
checkDeclStmt :: (Env, [Program]) -> Stmt -> Err (Env, [Program])
checkDeclStmt (env@(sigs, blocks), prog) stmt =
  case stmt of
    StmtExpr expr                -> checkExpr (env, prog) expr
    StmtVarInit lexpr guard expr -> checkStmtInit (env, prog) lexpr guard expr MutVar
    StmtDefInit lexpr guard expr -> checkStmtInit (env, prog) lexpr guard expr MutConst
    StmtReturn expr              -> checkReturn (env, prog) expr
    
    SComp compstmt               -> checkSComp (env, prog) compstmt --extendEnv (((Map.empty:sigs), ((NormBlock, Map.empty):blocks)), prog) compstmt

    StmtWhile expr compstmt      -> checkWhile (env, prog) expr compstmt
    _ -> Bad "checkStmt: fatal error!"

checkReturn :: (Env, [Program]) -> Expr -> Err (Env, [Program])
checkReturn (env@(sigs, blocks), prog) expr = do
  texpr <- inferExpr env expr
  (PIdent (p,fname),tfun)  <- findFunType prog
  if (texpr == tfun)
    then Ok (env, postAttach (PTDefs [ADecl texpr (DeclStmt (StmtReturn expr))]) prog)
    else fail $ "the returned type (" ++ show texpr ++ ") of function " ++ printTree fname ++ " " ++ show p ++" doesn't match with the function's type " ++ show tfun

findFunType :: [Program] -> Err (PIdent, Type)
findFunType [] = fail $ "there is no function block declared, the return is invalid"
findFunType (prog:progs) =
  case prog of
    PTDefs [ADecl t (DeclFun (LExprId pident) _ _ _)] -> Ok (pident, t)
    _ -> findFunType progs
  {- if blockType == NormBlock
    then Ok t
    else findFunType sigs blocks  
      where (id,(_,t)) = head . Map.toList $ sig -}

checkSComp :: (Env, [Program]) -> CompStmt -> Err (Env, [Program])
checkSComp (env@(sigs, blocks), prog) compstmt =
  case extendEnv (((Map.empty:sigs), ((NormBlock, Map.empty):blocks)), prog) compstmt of
    Ok (e', p') -> Ok (e', postAttach (PTDefs [ADecl TypeBool (DeclStmt (SComp (StmtBlock (getDecls p'))))]) prog)
                   --Ok (e', postAttach (PDefs [DeclStmt (SComp (StmtBlock (getDecls p')))]) prog)
    Bad s       -> Bad s

checkWhile :: (Env, [Program]) -> Expr -> CompStmt -> Err (Env, [Program])
checkWhile (env@(xs, blocks@((blockType, context):ys)), prog) expr compstmt = do
  t <- inferExpr env expr
  if (t == TypeBool)
    then case extendEnv (((Map.empty:xs), ((IterBlock, Map.empty):blocks)), prog) compstmt of
           -- TODO: the type is necessary?
           Ok (e', p') -> Ok (e', postAttach (PTDefs [ADecl t (DeclStmt (StmtWhile expr (StmtBlock (getDecls p'))))]) prog)
                          --Ok (e', postAttach (PDefs [DeclStmt (StmtWhile expr (StmtBlock (getDecls p')))]) prog) -- if is not necessary, but it does not work, atm
                          --Ok (e', postAttach (PDefs (getDecls p')) prog)
           Bad s       -> Bad s
    else fail $ "the expression type in the while statement must be boolean"

extendEnv2 :: (Env, [Program]) -> Stmt -> Err (Env, [Program])
extendEnv2 (env@(s, y), prog) stmt = do
  case stmt of
    StmtExpr expr -> do (e@((sig:sigs), b@(block:blocks)), p) <- checkExpr (env, prog) expr
                        Ok ((sigs,blocks), p List.\\ prog) -- list difference
    _ -> fail $ "extendEnv2: fatal error!"

checkStmtInit :: (Env, [Program]) -> LExpr -> Guard -> Expr -> Mutability -> Err (Env, [Program])
checkStmtInit (env@(sig, blocks@((blockType, context):xs)), prog) lexpr@(LExprId (PIdent (p, ident))) guard expr mut = do
  case guard of
    GuardType t ->
      case lookupVar blocks ident of
        Ok (m,t) -> fail $ show p ++ ": variable " ++ printTree ident ++ " already declared"
        _        -> Ok $ ((sig, ((blockType, addVar context (mut, ident) guard):blocks)), postAttach (PTDefs [ADecl t (DeclStmt (StmtVarInit lexpr guard expr))]) prog)
    GuardVoid -> fail $ show p ++ ": variable " ++ printTree ident ++ " declared as void! this is not allowed!"
  

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
    --OpIntDiv      -> do checkAssignIntOp (env, prog)  t1 lexpr op expr
    --OpFloatDiv    -> do checkAssignOp (env, prog)  t1 lexpr op expr
    OpRemainder   -> do checkAssignOp (env, prog)  t1 lexpr op expr
    OpModulo      -> do checkAssignOp (env, prog)  t1 lexpr op expr
    OpPower       -> do checkAssignOp (env, prog)  t1 lexpr op expr  

-- Check assignment with boolean operator.
checkAssignBoolOp :: (Env, [Program]) -> Type -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
checkAssignBoolOp (env, prog) t1 lexpr op expr = do
  t2 <- inferExpr env expr
  if (t2 `isCompatibleWith` t1)
    then Ok (env, postAttach (PTDefs [ADecl TypeBool (DeclStmt (StmtExpr (StmtAssign lexpr op expr)))]) prog)
    else fail $ printTree t2 ++ " is not compatible with " ++ printTree t1

-- Check assignment operator.
checkAssignOp :: (Env, [Program]) -> Type -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
checkAssignOp (env, prog) t1 lexpr op expr = do
  t2 <- inferExpr env expr
  if (t2 `isCompatibleWith` t1)
    then Ok (env, postAttach (PTDefs [ADecl t1 (DeclStmt (StmtExpr (StmtAssign lexpr op expr)))]) prog)
    else fail $ printTree t2 ++ " is not compatible with " ++ printTree t1

-- Check function call.
checkFunCall :: (Env, [Program]) -> Expr -> Err (Env, [Program])
checkFunCall (env@(sig, _), prog) (ExprFunCall (PIdent pident@(p,ident)) args) = do
  case lookupFun sig ident of
    Ok funDef@(mods,t) -> if length args == length mods -- !! doesn't work..
                          then Ok (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr (ExprFunCall (PIdent pident) args)))]) prog)
                          else fail $ show p ++ ": function '" ++ printTree ident ++ " doesn't match its parameters"
    _ -> fail $ show p ++ ": function '" ++ printTree ident ++ "' is not defined!" 

-- Check (right) expression.
checkExpr :: (Env, [Program]) -> Expr -> Err (Env, [Program])
checkExpr (env, prog) expr = do
  case expr of
    -- Assignment statement (i.e., lexpr operator expr).
    StmtAssign lexpr op expr -> do checkAssign (env, prog) lexpr op expr

    -- Left expressions.
    LeftExpr lexpr           -> do t <- inferLExpr env lexpr
                                   return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr (LeftExpr lexpr)))])prog) 

    -- Base expressions (i.e., integers, floats, chars, strings, booleans)
    ExprInt int              -> return (env, postAttach (PTDefs [ADecl TypeInt (DeclStmt (StmtExpr (ExprInt int)))]) prog)
    ExprDouble dbl           -> return (env, postAttach (PTDefs [ADecl TypeDouble (DeclStmt (StmtExpr (ExprDouble dbl)))]) prog)
    ExprChar chr             -> return (env, postAttach (PTDefs [ADecl TypeChar (DeclStmt (StmtExpr (ExprChar chr)))]) prog)
    ExprString str           -> return (env, postAttach (PTDefs [ADecl TypeString (DeclStmt (StmtExpr (ExprString str)))]) prog)
    ExprTrue                 -> return (env, postAttach (PTDefs [ADecl TypeBool (DeclStmt (StmtExpr ExprTrue))]) prog)
    ExprFalse                -> return (env, postAttach (PTDefs [ADecl TypeBool (DeclStmt (StmtExpr ExprFalse))]) prog)

    -- Function call.
    ExprFunCall pident args  -> do checkFunCall (env, prog) expr

    -- Boolean not.
    ExprBoolNot e            -> do t <- inferBoolUnOp env expr e
                                   return (env, postAttach (PTDefs [ADecl TypeBool (DeclStmt (StmtExpr expr))]) prog)

    -- Arithmetic unary operators.
    ExprAddition e           -> do t <- inferArithUnOp env expr e
                                   return (env, postAttach (PTDefs [ADecl TypeBool (DeclStmt (StmtExpr expr))]) prog)  
    ExprNegation e           -> do t <- inferArithUnOp env expr e
                                   return (env, postAttach (PTDefs [ADecl TypeBool (DeclStmt (StmtExpr expr))]) prog)    

    -- Arithmetic binary operators.
    ExprPower e1 e2    -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprMul e1 e2      -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    --ExprFloatDiv e1 e2 -> do t <- inferArithBinDoubleOp env expr e1 e2
    --                         return (env, postAttach (PTDefs [TypedDecl t (DeclStmt (StmtExpr expr))]) prog)
    --ExprIntDiv e1 e2 -> do t <- inferArithBinIntOp env expr e1 e2
    --                            return (env, postAttach (PTDefs [TypedDecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprReminder e1 e2 -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprModulo e1 e2   -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprPlus e1 e2     -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprMinus e1 e2    -> do t <- inferArithBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)

    -- Interval creation. (Recall that an interval is an iterable.)
    ExprIntInc e1 e2   -> checkInterval (env, prog) expr e1 e2
    ExprIntExc e1 e2   -> checkInterval (env, prog) expr e1 e2

    -- Relational binary operators.
    ExprLt e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprGt e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprLtEq e1 e2     -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprGtEq e1 e2     -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprEq e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprNeq e1 e2      -> do t <- inferRelBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)

    -- Boolean binary operator.
    ExprAnd e1 e2      -> do t <- inferBoolBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    ExprOr e1 e2       -> do t <- inferBoolBinOp env expr e1 e2
                             return (env, postAttach (PTDefs [ADecl t (DeclStmt (StmtExpr expr))]) prog)
    _ -> return (env, prog)

-- Check interval creation.
checkInterval :: (Env, [Program]) -> Expr -> Expr -> Expr -> Err (Env, [Program])
checkInterval (env, prog) expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 == TypeInt) && (t2 == TypeInt) -- only ints are allowed.
    then Ok (env, postAttach (PTDefs [ADecl t1 (DeclStmt (StmtExpr expr))]) prog)
    else fail $ "only integer intervals allowed!"
  
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

-- Infer (right) expression.
inferExpr :: Env -> Expr -> Err Type
inferExpr env expr =
  case expr of
    -- Left expressions. 
    LeftExpr lexpr -> inferLExpr env lexpr 

    -- Base expressions (i.e., integers, floats, chars, strings, booleans) 
    ExprTrue     -> Ok TypeBool
    ExprFalse    -> Ok TypeBool
    ExprInt _    -> Ok TypeInt
    ExprDouble _ -> Ok TypeDouble
    ExprChar _   -> Ok TypeChar
    ExprString _ -> Ok TypeString
    
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
    --ExprFloatDiv e1 e2 -> inferArithBinDoubleOp env expr e1 e2
    --ExprIntDiv e1 e2   -> inferArithBinIntOp env expr e1 e2
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
        else fail $ show p ++ ": function '" ++ printTree ident ++ "' doesn't match its parameters"
    _ -> fail $ show p ++ ": function '" ++ printTree ident ++ "' not defined!" 

-- Infer left expression.
inferLExpr :: Env -> LExpr -> Err Type
inferLExpr env@(sig, blocks) lexpr = do
  case lexpr of
    LExprId (PIdent (p, ident)) -> 
      case lookupVar blocks ident of
        Ok (_,t) -> Ok t
        _ -> fail $ show p ++ ": variable '" ++ printTree ident ++ "' non defined!"
    _ -> fail $ "inferLEXpr: fatal error"

-- Infer arithmetic unary operator.
inferArithUnOp :: Env -> Expr -> Expr -> Err Type
inferArithUnOp env expr e = do
  t <- inferExpr env e
  if (t == TypeInt) || (t == TypeDouble) -- only ints and floats are allowed
    then Ok t
    else fail $ printTree t ++ " is not " ++ show [TypeInt, TypeDouble]

-- Infer arithmetic binary operator.
inferArithBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
inferArithBinOp env expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 `isCompatibleWithAny` [TypeInt, TypeDouble]) && -- only ints and floats are allowed
     (t2 `isCompatibleWithAny` [TypeInt, TypeDouble]) &&
     areCompatible t1 t2
     then getMostGeneric t1 t2
     else fail $ "\n\t- invalid operands in expressions (" ++ filter (/= '\n') (printTree e1) ++
       ") and (" ++ filter (/= '\n') (printTree e2) ++ ") of types '" ++ printTree t1 ++ "' and '" ++ printTree t2 ++
       "'\n\t- in expression: (" ++ filter (/= '\n') (printTree expr) ++ ")" 

-- Infer relational binary operator.
inferRelBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
inferRelBinOp env expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 `isCompatibleWithAny` [TypeInt, TypeDouble, TypeChar, TypeString]) &&  -- only ints, floats, chars and strings are allowed
     (t2 `isCompatibleWithAny` [TypeInt, TypeDouble, TypeChar, TypeString]) &&
     areCompatible t1 t2
     then Ok TypeBool
     else fail $ "\n\t- invalid operands in expressions (" ++ filter (/= '\n') (printTree e1) ++
       ") and (" ++ filter (/= '\n') (printTree e2) ++ ") of types '" ++ printTree t1 ++ "' and '" ++ printTree t2 ++
       "'\n\t- in expression: (" ++ filter (/= '\n') (printTree expr) ++ ")"

-- Infer boolean unary operator.
inferBoolUnOp :: Env -> Expr -> Expr -> Err Type
inferBoolUnOp env expr e = do
  t <- inferExpr env e
  if (t == TypeBool) -- only booleans are allowed
    then Ok t
    else fail $ printTree t ++ " is not Boolean!"

-- Infer boolean binary operator.
inferBoolBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
inferBoolBinOp env expr e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  if (t1 == TypeBool) && (t2 == TypeBool) -- only booleans are allowed
    then Ok TypeBool  
    else fail $ "\n\t- invalid operands in expressions (" ++ filter (/= '\n') (printTree e1) ++
      ") and (" ++ filter (/= '\n') (printTree e2) ++ ") of types '" ++ printTree t1 ++ "' and '" ++ printTree t2 ++
      "'\n\t- in expression: (" ++ filter (/= '\n') (printTree expr) ++ ")"

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
  | otherwise                 = fail $ "getMostGeneric: fatal error!"

