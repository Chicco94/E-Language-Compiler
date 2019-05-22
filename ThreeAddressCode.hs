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
generateTAC progs = generateTAC_int ([],0, Map.empty) (PDefs (initTAC progs))

initTAC :: [Program] -> [Decl]
initTAC [] = []
initTAC (prog@(PDefs defs):progs) = defs ++ initTAC progs 


generateTAC_int :: Env -> Program -> Env
generateTAC_int env prog@(PDefs decls) = foldl generateInstruction env decls

-- potrei avere delle istruzioni non tipate per errore, le gestico mettendole a void
generateInstruction :: Env -> Decl -> Env
generateInstruction env decl = generateDecl env Nothing decl

generateDecl :: Env -> Maybe Type -> Decl -> Env 
generateDecl env maybe_type decl =
    case decl of
        TypedDecl (ADecl type_ decl') -> generateDecl env (Just type_) decl'
        -- LExpr [Arg] Guard CompStmt
        DeclFun   id args type_ stmts -> generateDeclFunc env id args type_ stmts
        DeclStmt (stmt) -> generateStmt env new_type stmt
          where new_type = case maybe_type of 
                                          Nothing      -> TypeVoid
                                          (Just type_) -> type_

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
-- expression statement
generateStmt env@(program, temp_count, labels) type_ stmt@(StmtExpr expr) = generateExpr env type_ expr
-- variable initialization or assignement
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
    -- shortcut per evitare t0 = v1; v2 = t0
    -- effettivo assegnamento con espressione complessa a destra
    _                 -> generateAssign (generateExpr (program, temp_count, new_labels) type_ expr) type_ id OpAssign
-- constant inizialization or assignement
generateStmt env type_ stmt@(StmtDefInit id guard expr) = generateStmt env type_ (StmtVarInit id guard expr)
-- return stmt
generateStmt env@(program, temp_count, labels) type_ stmt@(StmtReturn (PReturn (pos,name)) expr) = do
  let (program',temp_count',labels') = generateExpr env type_ expr
  ([Return (Temp (temp_count'-1,type_))] ++ program',temp_count',labels') -- ritorno l'ultima instanziata
-- compound statement
{-
generateStmt env@(program, temp_count, labels) type_ stmt@(SComp (StmtBlock decls)) = do
  let env'@(program',temp_count',labels') = generateTAC_int (PDefs decls)
  (program'++program,temp_count, labels)
-}

generateExpr :: Env -> Type -> Expr -> Env
generateExpr env@(program, temp_count, labels) type_ expr = 
    case expr of
        -- assign expression to variable
        ExprAssign   (LExprId id) op re    -> generateAssign (generateExpr env type_ re) type_ id op
        -- variable inside expression
        ExprLeft     (LExprId id@(PIdent (pos,name)))          -> do
          let (new_labels, var) = findVar labels (Var (name,pos,type_))
          ([AssignV2T   (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_labels)
 
        ExprInt      val         -> ([AssignIntTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprChar     val         -> ([AssignChrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprString   val         -> ([AssignStrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprFloat    val         -> ([AssignFloatTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprTrue     val         -> ([AssignTrueTemp  (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
        ExprFalse    val         -> ([AssignFalseTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), labels)
    
        ExprFunCall  fun params  -> (generateCallFunc env fun params type_) 

        ExprPower    expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpPower
        ExprMul      expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpMul
        ExprFloatDiv expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpFloatDiv
        ExprIntDiv   expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpIntDiv
        ExprReminder expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpRemainder
        ExprModulo   expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpModulo
  
        ExprPlus     expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpPlus
        ExprMinus    expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpMinus
  
        {- da testare (modificare anche PrintE.hs)
        ExprLt       expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpLt
        ExprGt       expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpGt
        ExprLtEq     expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpLtEq
        ExprGtEq     expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpGtEq
        ExprEq       expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpEq
        ExprNeq      expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpNeq
        -}

        {- inserire valutazioni rapide (es: true || _ e false && _) -}
        ExprOr       expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpOr
        ExprAnd      expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpAnd
      


        
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
generateDeclFunc :: Env -> LExpr -> [Arg] -> Guard -> CompStmt -> Env
generateDeclFunc env@(program, temp_count, labels) lexpr@(LExprId (PIdent (pos, name))) args guard stmt@(StmtBlock decls) = do
  let (new_labels, var) = findVar labels (Var (name,pos,type_))
  (generateTAC_int ([FuncDef var]++program,temp_count, labels) (PDefs decls))
    where type_ = case guard of
                    GuardVoid    -> TypeVoid
                    GuardType t_ -> t_

-- use temp variables as parameters
generateCallFunc :: Env -> PIdent -> [Expr] -> Type ->Env
generateCallFunc  env@(program, temp_count, labels) (PIdent (pos, name)) params type_ = do
  let (new_labels, var)              = findVar labels (Var (name,pos,type_)) -- prendo il tipo della funzione
  let (program',temp_count',labels') = (generateParams env params)
  ([FuncCall var (Temp (temp_count,type_))]++program',temp_count',labels')


generateParams :: Env -> [Expr] -> Env
generateParams env [] = env
generateParams env@(program, temp_count, labels) (param:params) = do
  let (program',temp_count',labels') = generateExpr env TypeVoid param
  generateParams ([AssignT2P (Temp (temp_count,TypeVoid))]++program',temp_count',labels') params