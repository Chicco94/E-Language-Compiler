module ThreeAddressCode where

    import Control.Monad
    -- suppress prelude conflicts
    import qualified Data.List as List
    
    import AbsE
    import AbsETAC
    import PrintE
    
    
    -- Initialize the tac program with an empty map.
    generateProgramTAC :: Program -> ProgramTAC
    generateProgramTAC prog@(PDefs def) = foldl generateInstruction([]) def
    
    
    generateInstruction :: ProgramTAC -> Decl -> ProgramTAC
    generateInstruction tacprog instruction = 
        case instruction of
            --DeclFun fun -> generateDeclFunc tacprog fun
            DeclStmt stmt -> generateDeclStmt tacprog stmt
            --StmtExpr stmt -> generateExprStmt tacprog stmt
            _ ->tacprog-- TODO
    
    
    generateDeclStmt :: ProgramTAC -> Stmt -> ProgramTAC
    generateDeclStmt tacprog stmt =
        case stmt of
            StmtVarInit lexpr@(LExprId (PIdent (pos,name))) guard expr -> tacprog
            _ -> tacprog

    generateExprStmt :: ProgramTAC -> Stmt -> ProgramTAC
    generateExprStmt tacprog stmt =
        case stmt of
            _ -> tacprog

    
    -- TODO? define function declaretion more efficiently
    generateDeclFunc :: ProgramTAC -> LExpr -> [Arg] -> Guard -> [Stmt] -> ProgramTAC
    generateDeclFunc tacprog lexpr@(LExprId (PIdent (pos, fname))) args guard@(GuardType t) stmts = tacprog