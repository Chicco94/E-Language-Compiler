module ThreeAddressCode where

--import Control.Monad.State

import AbsE
import PrintE

import Prelude hiding (lookup)
import qualified Data.Map as Map

-- Environment = (Program, tempValue, variables)
type Env          = ([TAC], Temp_count, VariablesMap,Label_count)
type Temp_count   = Int
type VariablesMap = Map.Map String Var         -- Variables context: String -> Var
type Label_count  = Int


 -- Initialize the tacprogram with an empty list.
generateTAC :: [Program] -> Env
generateTAC progs = generateTAC_int ([],0, Map.empty,0) (PDefs (initTAC progs))

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
-- if there is, just return it
-- if there isn't, add it to the list and then return it 
findVar :: Env -> Var -> (Env,Var)
findVar env@(program, temp_count, variables,label_count) var@(Var (name,pos,type_)) = 
  case Map.lookup name variables of
    -- the variable is new
    Nothing     -> ((program, temp_count, (Map.insert name var variables),label_count), var)
    -- the variable has already been declared
    Just old_var -> (env, old_var)

addTACList :: Env -> [TAC] -> Env
addTACList env [] = env
addTACList env@(program, temp_count, variables,label_count) (tac:rest) = addTACList (addTAC env tac) rest
addTAC :: Env -> TAC -> Env
addTAC env@(program, temp_count, variables,label_count) tac = ([tac]++program , temp_count, variables, label_count)



generateStmt :: Env  -> Type -> Stmt -> Env 
-- expression statement
generateStmt env@(program, temp_count, variables,label_count) type_ stmt@(StmtExpr expr) = generateExpr env type_ expr
-- variable initialization or assignement
generateStmt env@(program, temp_count, variables,label_count) type_ stmt@(StmtVarInit id@(PIdent (pos,name)) guard expr ) = do
  let (env', var) = findVar env (Var (name,pos,type_))
  case expr of
    -- shortcut per evitare t0 = 1; var = t0
    (ExprInt    val)  -> (addTACList env' [AssignIntVar    var val])
    (ExprChar   val)  -> (addTACList env' [AssignChrVar    var val])
    (ExprString val)  -> (addTACList env' [AssignStrVar    var val])
    (ExprFloat  val)  -> (addTACList env' [AssignFloatVar  var val])
    (ExprTrue   val)  -> (addTACList env' [AssignTrueVar   var val])
    (ExprFalse  val)  -> (addTACList env' [AssignFalseVar  var val])
    -- effettivo assegnamento con espressione complessa a destra
    _                 -> generateAssign (generateExpr env' type_ expr) type_ id OpAssign
-- constant inizialization or assignement
generateStmt env type_ stmt@(StmtDefInit id guard expr) = generateStmt env type_ (StmtVarInit id guard expr)
-- return stmt
generateStmt env@(program, temp_count, variables,label_count) type_ stmt@(StmtReturn (PReturn (pos,name)) expr) = do
  let (program',temp_count',variables',label_count') = generateExpr env type_ expr
  ([Return (Temp (temp_count'-1,type_))] ++ program',temp_count',variables',label_count') -- ritorno l'ultima variabile temporanea instanziata
-- compound statement
generateStmt env type_ stmt@(SComp (StmtBlock decls)) = generateTAC_int env (PDefs decls)
-- if then else
generateStmt env@(program, temp_count, variables,label_count) type_ stmt@(StmtIfThenElse bexpr stmtsT stmtsF) = do
  let (program', temp_count', variables',label_count') = (generateExpr env type_ bexpr)
  let (program'', temp_count'', variables'',label_count'') = generateStmt ([IfFalse (Temp (temp_count'-1,TypeBool)) (Label ("if_false",label_count') )]++program', temp_count', variables', label_count'+1) type_ (SComp stmtsT)
  let (program''', temp_count''', variables''',label_count''') = generateStmt ([Lbl (Label ("if_false",label_count') ),Goto (Label ("end_if",label_count') )]++program'',temp_count'',variables'', label_count'') type_ (SComp stmtsF)
  ([Lbl (Label ("end_if",label_count') )]++program''',temp_count''',variables''',label_count''')
-- if then 
generateStmt env@(program, temp_count, variables,label_count) type_ stmt@(StmtIfThen bexpr stmts) = do
  let (program', temp_count', variables', label_count') = (generateExpr env TypeBool bexpr)
  let (program'', temp_count'', variables'', label_count'') = generateStmt ([IfFalse (Temp (temp_count'-1,TypeBool)) (Label ("end_if", label_count') )]++program',temp_count',variables', label_count') type_ (SComp stmts)
  ([Lbl (Label ("end_if", label_count') )]++program'',temp_count'',variables'', label_count'')
-- switch case
generateStmt env@(program, temp_count, variables,label_count) type_ stmt@(StmtSwitchCase expr norm_cases dflt_case) = env
-- evaluate expr
-- jump to conditions
-- normcases   -> stmts + jump_to_end
-- defaultcase -> stmts + jump_to_end
-- condition jump
-- defualut case  
  -- while stmt
generateStmt env@(program, temp_count, variables,label_count) type_ stmt@(StmtWhile bexpr (StmtBlock decls)) = do
  let (program',temp_count',variables',label_count') =  (generateExpr (generateTAC_int (addTACList env [Lbl (Label ("body",label_count) ),Goto (Label ("gaurd",label_count) )]) (PDefs decls)) TypeBool bexpr)
  (([If (Temp (temp_count'-1,TypeBool)) (Label ("body",label_count) ),Lbl (Label ("guard",label_count) )]++program',temp_count',variables', label_count')) 
  --([Return (Temp (temp_count'-1,type_))] ++ program',temp_count',variables') 



generateExpr :: Env -> Type -> Expr -> Env
generateExpr env@(program, temp_count, variables,label_count) type_ expr = 
    case expr of
        -- assign expression to variable
        ExprAssign   (LExprId id) op re    -> generateAssign (generateExpr env type_ re) type_ id op
        -- variable inside expression
        ExprLeft     (LExprId id@(PIdent (pos,name)))          -> do
          let (env'@(_, _, new_variables,_), var) = findVar env (Var (name,pos,type_))
          ([AssignV2T   (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_variables, label_count)
 
        ExprInt      val         -> ([AssignIntTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, label_count)
        ExprChar     val         -> ([AssignChrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, label_count)
        ExprString   val         -> ([AssignStrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, label_count)
        ExprFloat    val         -> ([AssignFloatTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, label_count)
        ExprTrue     val         -> ([AssignTrueTemp  (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, label_count)
        ExprFalse    val         -> ([AssignFalseTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, label_count)
    
        ExprFunCall  fun params  -> (generateCallFunc env fun params type_) 

        ExprPower    expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpPower
        ExprMul      expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpMul
        ExprFloatDiv expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpFloatDiv
        ExprIntDiv   expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpIntDiv
        ExprReminder expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpRemainder
        ExprModulo   expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpModulo
  
        ExprPlus     expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpPlus
        ExprMinus    expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpMinus
  
        ExprLt       expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpLt
        ExprGt       expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpGt
        ExprLtEq     expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpLtEq
        ExprGtEq     expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpGtEq
        ExprEq       expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpEq
        ExprNeq      expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpNeq

        {- inserire valutazioni rapide (es: true || _ e false && _) -}
        ExprOr       expr1 expr2 -> do--binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpOr
          let (program', temp_count', variables', label_count') = (generateExpr env type_ expr1)
          let (program'', temp_count'', variables'', label_count'') = (generateExpr ([If (Temp (temp_count'-1,TypeBool)) (Label ("true_or", label_count') )]++program',temp_count', variables', label_count'+1) type_ expr2)
          ([Lbl (Label ("end_or",label_count') ),AssignTrueTemp  (Temp (temp_count'',TypeBool)) (PTrue ((0,0),"true")),Lbl (Label ("true_or",label_count') ),Goto (Label ("end_or",label_count') ),AssignFalseTemp  (Temp (temp_count'',TypeBool)) (PFalse ((0,0),"false")), If (Temp (temp_count''-1,TypeBool)) (Label ("true_or", label_count') )]++program'',temp_count''+1, variables'', label_count'')
          
        
        ExprAnd      expr1 expr2 -> do --binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpAnd
          let (program', temp_count', variables', label_count') = (generateExpr env type_ expr1)
          let (program'', temp_count'', variables'', label_count'') = (generateExpr ([IfFalse (Temp (temp_count'-1,TypeBool)) (Label ("falseAnd", label_count') )]++program',temp_count', variables', label_count'+1) type_ expr2)
          ([Lbl (Label ("end_and",label_count') ),AssignFalseTemp  (Temp (temp_count'',TypeBool)) (PFalse ((0,0),"false")),Lbl (Label ("falseAnd",label_count') ),Goto (Label ("end_and",label_count') ),AssignTrueTemp  (Temp (temp_count'',TypeBool)) (PTrue ((0,0),"true")), IfFalse (Temp (temp_count''-1,TypeBool)) (Label ("false_and", label_count') )]++program'',temp_count''+1, variables'', label_count'')
        
-- Build the binary operator using the last two temporaneus variable
binaryExpr :: Env -> Type -> BinaryOperator -> Env
binaryExpr env@(program, temp_count, variables,label_count) type_ op = ([ BinOp op (Temp (temp_count,type_)) (Temp (temp_count-2,type_)) (Temp (temp_count-1,type_))] ++ program, (temp_count+1), variables, label_count)



--StmtAssign LExpr AssignOperator Expr
generateAssign :: Env -> Type -> PIdent -> AssignOperator -> Env
generateAssign env@(program, temp_count, variables,label_count) type_ id@(PIdent (pos,name)) op = do
  let (env'@(program, temp_count, new_variables,label_count), var) = findVar env (Var (name,pos,type_))
  case op of
    OpAssign    -> (addTACList env [AssignT2V  var (Temp (temp_count-1,type_))])
    _           -> generateAssign (binaryExpr ([AssignV2T (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_variables, label_count) type_ op') type_ id OpAssign
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
generateDeclFunc env@(program, temp_count, variables,label_count) lexpr@(LExprId (PIdent (pos, name))) args guard stmt@(StmtBlock decls) = do
  let (env', var) = findVar env (Var (name,pos,type_))
  (generateTAC_int (addTACList env' [FuncDef var]) (PDefs decls))
    where type_ = case guard of
                    GuardVoid    -> TypeVoid
                    GuardType t_ -> t_



-- use temp variables as parameters
generateCallFunc :: Env -> PIdent -> [Expr] -> Type ->Env
generateCallFunc  env@(program, temp_count, variables,label_count) (PIdent (pos, name)) params type_ = do
  let (env', var)              = findVar env (Var (name,pos,type_)) -- prendo il tipo della funzione
  (addTACList (generateParams env' params) [FuncCall var (Temp (temp_count,type_))])

generateParams :: Env -> [Expr] -> Env
generateParams env [] = env
generateParams env@(program, temp_count, variables,label_count) (param:params) = generateParams (addTACList (generateExpr env TypeVoid param) [AssignT2P (Temp (temp_count,TypeVoid))]) params