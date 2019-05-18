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


findType :: Maybe Type -> Type
findType Nothing = TypeVoid
findType (Just t) = t

generateStmt :: Env  -> Type -> Stmt -> Env 
generateStmt env@(program, tempCount) type_ stmt@(StmtVarInit var@(PIdent (pos,name)) guard expr ) = 
  case expr of
    -- shortcut per evitare t0 = 1; var = t0
    (ExprInt    val)  -> ([AssignIntVar    (Var (name,pos,type_)) val] ++ program, tempCount)
    (ExprChar   val)  -> ([AssignChrVar    (Var (name,pos,type_)) val] ++ program, tempCount)
    (ExprString val)  -> ([AssignStrVar    (Var (name,pos,type_)) val] ++ program, tempCount)
    (ExprFloat  val)  -> ([AssignFloatVar  (Var (name,pos,type_)) val] ++ program, tempCount)
    (ExprTrue   val)  -> ([AssignTrueVar   (Var (name,pos,type_)) val] ++ program, tempCount)
    (ExprFalse  val)  -> ([AssignFalseVar  (Var (name,pos,type_)) val] ++ program, tempCount)
    -- effettivo assegnamento con espressione complessa a destra
    _               -> generateAssign (generateExpr env type_ expr) type_ var OpAssign
generateStmt env@(program, tempCount) type_ stmt@(StmtExpr expr) = generateExpr env type_ expr

generateExpr :: Env -> Type -> Expr -> Env
generateExpr env@(program, tempCount) type_ expr = 
    case expr of
        ExprAssign   (LExprId id) op re    -> do generateAssign (generateExpr env type_ re) type_ id op
        
        ExprOr       expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpOr
        ExprAnd      expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpAnd
        ExprPlus     expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpPlus
        ExprMinus    expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpMinus
        ExprMul      expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpMul
        ExprIntDiv   expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpIntDiv
        ExprFloatDiv expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpFloatDiv
        ExprReminder expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpRemainder
        ExprModulo   expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpModulo
        ExprPower    expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpPower
        ExprInt      val         -> ([AssignIntTemp   (Temp (tempCount,type_)) val] ++ program, (tempCount+1))
        ExprChar     val         -> ([AssignChrTemp   (Temp (tempCount,type_)) val] ++ program, (tempCount+1))
        ExprString   val         -> ([AssignStrTemp   (Temp (tempCount,type_)) val] ++ program, (tempCount+1))
        ExprFloat    val         -> ([AssignFloatTemp (Temp (tempCount,type_)) val] ++ program, (tempCount+1))
        ExprTrue     val         -> ([AssignTrueTemp  (Temp (tempCount,type_)) val] ++ program, (tempCount+1))
        ExprFalse    val         -> ([AssignFalseTemp (Temp (tempCount,type_)) val] ++ program, (tempCount+1))

--TODO case Type
binaryExpr :: Env -> Type -> BinaryOperator -> Env
binaryExpr env@(program, tempCount) type_ op = ([ BinOp op (Temp (tempCount,type_)) (Temp (tempCount-2,type_)) (Temp (tempCount-1,type_))] ++ program, (tempCount+1))


--StmtAssign LExpr AssignOperator Expr
generateAssign :: Env -> Type -> PIdent -> AssignOperator -> Env
generateAssign env@(program, tempCount) type_ (PIdent (pos,name)) op = 
  case op of
    OpAssign -> ([AssignT2V  (Var  (name,pos,type_)) (Temp (tempCount-1,type_))] ++ program, (tempCount))


-- DeclFun LExpr [Arg] Guard CompStmt
{-
generateDeclFunc :: Context -> LExpr -> [Arg] -> Guard -> [Stmt] -> Context
generateDeclFunc context lexpr@(LExprId (PIdent (pos, fname))) args guard@(GuardType t) stmts = context
-}