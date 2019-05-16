module ThreeAddressCode where

import Control.Monad
-- suppress prelude conflicts
import qualified Data.Map as Map

import AbsE
import PrintE
import ErrM

  -- Given an element 'a' and a list, add this element at the end of the list.
postAttach :: a -> [a] -> [a]
postAttach a [] = [a]
postAttach a (x:xs) = x : postAttach a xs

 -- Initialize the tacprogram with an empty list.
generateTAC :: [Program] -> Err [Program]
generateTAC progs = generateTAC_int (PDefs (initTAC progs))

initTAC :: [Program] -> [Decl]
initTAC [] = []
initTAC (prog@(PDefs defs):progs) = defs ++ initTAC progs 


generateTAC_int :: Program -> Err [Program]
generateTAC_int prog@(PDefs decls) = foldM generateInstruction [] decls

generateInstruction :: [Program] -> Decl -> Err [Program]
generateInstruction program decl = generateDecl program Nothing decl

generateDecl :: [Program] -> Maybe Type -> Decl -> Err [Program]
generateDecl program maybe_type decl =
    case decl of
        TypedDecl (ADecl type_ decl') -> generateDecl program (Just type_) decl'
        -- DeclFun   fdecl@(ADecl t decl) -> generateDeclStmt program t decl
        DeclStmt (stmt) -> generateStmt program new_type stmt
          where new_type = findType maybe_type
        _ -> Ok $ program --fail "generateInstruction" -- TODO

findType :: Maybe Type -> Type
findType Nothing = TypeVoid
findType (Just t) = t

generateStmt :: [Program] -> Type -> Stmt -> Err [Program]
generateStmt program type_ stmt =
    case stmt of
        StmtInit stmt@(LExprId (PIdent (pos,name))) guard (ExprInt val) -> Ok $ postAttach (TACProgram (tacInstructions)) program
          where tacInstructions = [AssignIntVar (Var (name,pos,type_)) val]
        StmtExpr expr -> generateExpr program type_ expr
        _ -> Ok $ program --


generateExpr :: [Program] -> Type -> Expr -> Err [Program]
generateExpr program type_ expr = 
    case expr of
        --StmtAssign  expr        -> generateAssign program type_ expr
        --ExprTrue    expr        -> Ok $ "true"
        --ExprFalse   expr        -> Ok $ "false"
        ExprPlus    expr1 expr2 -> binaryExpr program type_ expr1 expr2 BOpPlus
        _ -> Ok $ program 
        
--TODO case Type
binaryExpr :: [Program] -> Type -> Expr -> Expr -> BinaryOperator -> Err [Program]
binaryExpr program type_ (ExprInt expr1) (ExprInt expr2) op =
  case type_ of
    TypeInt -> Ok $ postAttach (TACProgram ([AssignIntTemp (Temp (1,type_)) expr1, AssignIntTemp (Temp (2,type_)) expr2, BinOp op (Temp (3,type_)) (Temp (1,type_)) (Temp (2,type_))])) program
    _ -> Ok $ program

{-
--StmtAssign LExpr AssignOperator Expr
generateAssign :: [Program] -> Type -> Expr -> Err [Program]
generateAssign program type_ lexpr@(LExprId (PIdent (pos,name)))) op rexpr = do
    --lexpr <- findVariable name
    case rexpr of
        ExprPlus expr1 expr2 -> Ok $ postAttach (TACProgram (tacInstructions)) program
            where tacInstructions = 
-}

-- DeclFun LExpr [Arg] Guard CompStmt
{-
generateDeclFunc :: Context -> LExpr -> [Arg] -> Guard -> [Stmt] -> Context
generateDeclFunc context lexpr@(LExprId (PIdent (pos, fname))) args guard@(GuardType t) stmts = context
-}