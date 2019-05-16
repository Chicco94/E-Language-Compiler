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
generateTAC_int prog@(PDefs defs) = Ok $ [prog] --foldM generateInstruction [] defs



{-
generateInstruction :: [Program] -> Decl -> Err [Program]
generateInstruction program ins = 
    case ins of
        TypedDecl adecl@(ADecl type_ decl) -> generateDeclStmt program type_ decl
        --DeclFun   fdecl@(ADecl t decl) -> generateDeclStmt program t decl
        --DeclStmt  sdecl@(ADecl t decl) -> generateDeclStmt program t decl
        _ -> fail "generateInstruction" -- TODO

generateDeclStmt :: [Program] -> Type -> Decl -> Err [Program]
generateDeclStmt program type_ decl =
    case decl of
        DeclStmt(StmtInit lexpr@(LExprId (PIdent (pos,name))) guard e@(ExprInt val)) -> Ok $ (ProgramTAC [ (AssignIntVar (Var (name,pos,type_)) val)]) ++ program
        _ -> fail "generateDeclStmt"


-- TODO? define function declaretion more efficiently
generateDeclFunc :: Context -> LExpr -> [Arg] -> Guard -> [Stmt] -> Context
generateDeclFunc context lexpr@(LExprId (PIdent (pos, fname))) args guard@(GuardType t) stmts = context
-}