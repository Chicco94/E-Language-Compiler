module ThreeAddressCode where

--import Control.Monad.State

import AbsE
import PrintE

-- Environment = (Program, tempValue, labels)
type Env = ([TAC], Int)


 -- Initialize the tacprogram with an empty list.
generateTAC :: [Program] -> Env
generateTAC progs = generateTAC_int (PDefs (initTAC progs))

initTAC :: [Program] -> [Decl]
initTAC [] = []
initTAC (prog@(PDefs defs):progs) = defs ++ initTAC progs 


generateTAC_int :: Program -> Env
generateTAC_int prog@(PDefs decls) = foldl generateInstruction ([],0) decls

-- potrei avere delle istruzioni non tipate per errore, le gestico mettendole a void
generateInstruction :: Env -> Decl -> Env
generateInstruction env decl = generateDecl env Nothing decl

generateDecl :: Env -> Maybe Type -> Decl -> Env 
generateDecl env maybe_type decl =
    case decl of
        TypedDecl (ADecl type_ decl') -> generateDecl env (Just type_) decl'
        -- DeclFun   fdecl@(ADecl t decl) -> generateDeclStmt env t decl
        DeclStmt (stmt) -> generateStmt env new_type stmt
          where new_type = findType maybe_type
        _ -> env --fail "generateInstruction" -- TODO

findType :: Maybe Type -> Type
findType Nothing = TypeVoid
findType (Just t) = t

generateStmt :: Env  -> Type -> Stmt -> Env 
generateStmt env@(program, tempCount) type_ stmt =
    case stmt of
        StmtInit stmt@(LExprId (PIdent (pos,name))) guard (ExprInt val) -> (tacInstructions ++ program, tempCount)
          where tacInstructions = [AssignIntVar (Var (name,pos,type_)) val]
        StmtExpr expr -> generateExpr env type_ expr
        _ -> env --


generateExpr :: Env -> Type -> Expr -> Env
generateExpr env type_ expr = 
    case expr of
        --StmtAssign   le op re    -> generateAssign env type_ le op re 
        --ExprTrue    expr        -> "true"
        --ExprFalse   expr        -> "false"
        ExprOr       expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpOr
        ExprAnd      expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpAnd
        ExprPlus     expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpPlus
        ExprMinus    expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpMinus
        ExprMul      expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpMul
        ExprIntDiv   expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpIntDiv
        ExprFloatDiv expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpFloatDiv
        ExprReminder expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpRemainder
        ExprModulo   expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpModulo
        ExprPower    expr1 expr2 -> binaryExpr env type_ expr1 expr2 BOpPower
        _ -> env 

--TODO case Type
binaryExpr :: Env -> Type -> Expr -> Expr -> BinaryOperator -> Env
binaryExpr env@(program, tempCount) type_ expr1@(ExprInt val1) expr2@(ExprInt val2) op =
  case type_ of
    TypeInt    -> ([ BinOp op (Temp (tempCount+2,type_)) (Temp (tempCount,type_)) (Temp (tempCount+1,type_)), AssignIntTemp   (Temp (tempCount+1,type_)) val2 , AssignIntTemp   (Temp (tempCount,type_)) val1] ++ program, (tempCount+3))
    --TypeChar   -> [BinOp op (Temp (3,type_)) (Temp (1,type_)) (Temp (2,type_)), AssignChrTemp   (Temp (2,type_)) val2 , AssignChrTemp   (Temp (1,type_)) val1] ++ env
    --TypeString -> [BinOp op (Temp (3,type_)) (Temp (1,type_)) (Temp (2,type_)), AssignStrTemp   (Temp (2,type_)) val2 , AssignStrTemp   (Temp (1,type_)) val1] ++ env
    --TypeBool   -> [BinOp op (Temp (3,type_)) (Temp (1,type_)) (Temp (2,type_)), AssignBoolTemp  (Temp (2,type_)) val2 , AssignBoolTemp  (Temp (1,type_)) val1] ++ env
    --TypeDouble -> [BinOp op (Temp (3,type_)) (Temp (1,type_)) (Temp (2,type_)), AssignFloatTemp (Temp (2,type_)) val2 , AssignFloatTemp (Temp (1,type_)) val1] ++ env
    _ -> env

{-
--StmtAssign LExpr AssignOperator Expr
generateAssign :: Env -> Type -> LExpr -> AssignOperator -> Expr -> Env
generateAssign env type_ lexpr@(LExprId (PIdent (pos,name))) op rexpr = 
  case op of
    OpAssign -> ([AssignT2V  (Var  (name,pos,type_)) (Temp (tempCount,type_))] ++ program, (tempCount))
    _ -> env
-}

-- DeclFun LExpr [Arg] Guard CompStmt
{-
generateDeclFunc :: Context -> LExpr -> [Arg] -> Guard -> [Stmt] -> Context
generateDeclFunc context lexpr@(LExprId (PIdent (pos, fname))) args guard@(GuardType t) stmts = context
-}