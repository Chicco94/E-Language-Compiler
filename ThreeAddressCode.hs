module ThreeAddressCode where

--import Control.Monad.State

import AbsE
import PrintE

import Prelude hiding (lookup)
import qualified Data.Map as Map

-- Environment = (Program, tempValue, labels)
type Env       = ([TAC], Temp_count, LabelsMap)
type Temp_count = Int
type LabelsMap = Map.Map String Var         -- Variables context: String -> Var


 -- Initialize the tacprogram with an empty list.
generateTAC :: [Program] -> Env
generateTAC progs = generateTAC_int (PDefs (initTAC progs))

initTAC :: [Program] -> [Decl]
initTAC [] = []
initTAC (prog@(PDefs defs):progs) = defs ++ initTAC progs 


generateTAC_int :: Program -> Env
generateTAC_int prog@(PDefs decls) = foldl generateInstruction ([],0, Map.empty) decls

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

-- search the variable in the list of variables
-- if there is just return it
-- if there isn't, add it to the list and then return it 
findVar :: LabelsMap -> Var -> (LabelsMap,Var)
findVar labels var@(Var (name,pos,type_)) = 
  case Map.lookup name labels of
    -- the variable is new
    Nothing     -> (Map.insert name var labels, var)
    -- the variable has already been declared
    Just old_var -> (labels, old_var)


generateStmt :: Env  -> Type -> Stmt -> Env 
generateStmt env@(program, temp_count, labels) type_ stmt@(StmtVarInit id@(PIdent (pos,name)) guard expr ) = do
  let (new_labels, var) = findVar labels (Var (name,pos,type_))
  case expr of
    -- shortcut per evitare t0 = 1; var = t0
    (ExprInt    val)  -> ([AssignIntVar    var val] ++ program, temp_count, new_labels)
    (ExprChar   val)  -> ([AssignChrVar    var val] ++ program, temp_count, new_labels)
    (ExprString val)  -> ([AssignStrVar    var val] ++ program, temp_count, new_labels)
    (ExprFloat  val)  -> ([AssignFloatVar  var val] ++ program, temp_count, new_labels)
    (ExprTrue   val)  -> ([AssignTrueVar   var val] ++ program, temp_count, new_labels)
    (ExprFalse  val)  -> ([AssignFalseVar  var val] ++ program, temp_count, new_labels)
    -- effettivo assegnamento con espressione complessa a destra
    _               -> generateAssign (generateExpr (program, temp_count, new_labels) type_ expr) type_ id OpAssign
generateStmt env@(program, temp_count, labels) type_ stmt@(StmtExpr expr) = generateExpr env type_ expr

generateExpr :: Env -> Type -> Expr -> Env
generateExpr env@(program, temp_count, labels) type_ expr = 
    case expr of
        -- assign expression to variable
        ExprAssign   (LExprId id) op re    -> generateAssign (generateExpr env type_ re) type_ id op
        -- variable inside expression
        ExprLeft     (LExprId id@(PIdent (pos,name)))          -> do
          let (new_labels, var) = findVar labels (Var (name,pos,type_))
          ([AssignV2T   (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_labels)
        
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
        
        ExprInt      val         -> ([AssignIntTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprChar     val         -> ([AssignChrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprString   val         -> ([AssignStrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprFloat    val         -> ([AssignFloatTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprTrue     val         -> ([AssignTrueTemp  (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprFalse    val         -> ([AssignFalseTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)

        
-- Build the binary operator using the last two temporaneus variable
binaryExpr :: Env -> Type -> BinaryOperator -> Env
binaryExpr env@(program, temp_count, labels) type_ op = ([ BinOp op (Temp (temp_count,type_)) (Temp (temp_count-2,type_)) (Temp (temp_count-1,type_))] ++ program, (temp_count+1), labels)


--StmtAssign LExpr AssignOperator Expr
generateAssign :: Env -> Type -> PIdent -> AssignOperator -> Env
generateAssign env@(program, temp_count, labels) type_ id@(PIdent (pos,name)) op = do
  let (new_labels, var) = findVar labels (Var (name,pos,type_))
  case op of
    OpAssign    -> ([AssignT2V  var (Temp (temp_count-1,type_))] ++ program, (temp_count), new_labels)
    _           -> generateAssign (binaryExpr ([AssignV2T (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_labels) type_ op') type_ id OpAssign
                    where op' = case op of 
                                  OpOr        -> BOpOr
                                  OpAnd       -> BOpAnd       
                                  OpPlus      -> BOpPlus       
                                  OpMinus     -> BOpMinus     
                                  OpMul       -> BOpMul        
                                  OpIntDiv    -> BOpIntDiv     
                                  OpFloatDiv  -> BOpFloatDiv   
                                  OpRemainder -> BOpRemainder 
                                  OpModulo    -> BOpModulo    
                                  OpPower     -> BOpPower     

-- DeclFun LExpr [Arg] Guard CompStmt
{-
generateDeclFunc :: Context -> LExpr -> [Arg] -> Guard -> [Stmt] -> Context
generateDeclFunc context lexpr@(LExprId (PIdent (pos, fname))) args guard@(GuardType t) stmts = context
-}