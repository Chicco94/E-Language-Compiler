module ThreeAddressCode where

-- suppress prelude conflicts
import qualified Data.Map as Map

import AbsE
import AbsETAC

-- contiene cosa ho valorizzato finora
-- test 1, solo interi
type Context   = Map.Map String Integer

 -- Initialize the context with an empty map.
generateTAC :: Program -> TACProgram
generateTAC prog@(PDefs def) = foldl generateInstruction(Map.empty) def


generateInstruction :: Context -> Decl -> [TAC]
generateInstruction context instruction =
    case instruction of
        --DeclFun fun -> generateDeclFunc context fun
        --DeclStmt stmt -> generateDeclStmt context stmt
        _ -> show instruction -- TODO


generateDeclStmt :: Context -> Stmt -> [TAC]
generateDeclStmt context stmt =
    case stmt of
        StmtVarInit lexpr@(LExprId (PIdent (pos,name))) guard expr -> do 
            case guard of
                Integer -> map
        


-- TODO? define function declaretion more efficiently
generateDeclFunc :: Context -> LExpr -> [Arg] -> Guard -> [Stmt] -> [TAC]
generateDeclFunc lexpr@(LExprId (PIdent (pos, fname))) args guard@(GuardType t) stmts = 