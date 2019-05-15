module ThreeAddressCode where

    import Control.Monad
    -- suppress prelude conflicts
    import qualified Data.Map as Map
    
    import AbsE
    import AbsETAC
    import PrintE
    import ErrM
    
    -- contiene cosa ho valorizzato finora
    -- test 1, solo interi
    type Context   = Map.Map String Integer
    
     -- Initialize the context with an empty map.
    generateTAC :: Program -> Err Context
    generateTAC prog@(PDefs def) = foldM generateInstruction(Integer) def
    
    
    generateInstruction :: Context -> Decl -> Context
    generateInstruction context instruction = 
        case instruction of
            --DeclFun fun -> generateDeclFunc context fun
            --DeclStmt stmt -> generateDeclStmt context stmt
            _ -> context-- TODO
    
    
    generateDeclStmt :: Context -> Stmt -> Context
    generateDeclStmt context stmt =
        case stmt of
            StmtVarInitD lexpr@(LExprId (PIdent (pos,name))) guard expr -> do 
                case guard of
                    _ -> context
            
    
    
    -- TODO? define function declaretion more efficiently
    generateDeclFunc :: Context -> LExpr -> [Arg] -> Guard -> [Stmt] -> Context
    generateDeclFunc context lexpr@(LExprId (PIdent (pos, fname))) args guard@(GuardType t) stmts = context