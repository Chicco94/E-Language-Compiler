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
        TypedDecl (ADecl type_ decl1) -> generateDecl env (Just type_) decl1
        -- LExpr [Arg] Guard CompStmt
        DeclFun   id args type_ stmts -> generateDeclFunc env id args type_ stmts
        DeclStmt (stmt) -> generateStmt env new_type stmt
          where new_type = case maybe_type of 
                                          Nothing      -> TypeVoid
                                          (Just type_) -> type_


-- add a nuw variable to the env
addVar :: Env -> Var -> Env
addVar env@(program, temp_count, variables,label_count) var@(Var (name,pos,type_)) = 
  (program, temp_count, (Map.insert name var variables),label_count)

-- search the variable in the list of variables
-- if there is, just return it
-- if there isn1t, add it to the list and then return it 
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
addTAC env@(program, temp_count, variables,label_count) tac =
  case tac of
    Lbl lbl -> ([Lbl lbl]++program , temp_count, variables, label_count+1)
    _ -> ([tac]++program , temp_count, variables, label_count)



generateStmt :: Env  -> Type -> Stmt -> Env 
generateStmt env@(program, temp_count, variables,label_count) type_ stmt = 
  case stmt of
    StmtExpr expr                                 -> generateExpr env type_ expr  -- expression statement
    StmtVarInit id@(PIdent (pos,name)) guard expr -> do                           -- variable initialization or assignement
      let env1 = addVar env (Var (name,pos,type_))
      case expr of
        -- shortcut per evitare t0 = 1; var = t0
        (ExprInt    val)  -> (addTACList env1 [AssignIntVar    (Var (name,pos,type_)) val])
        (ExprChar   val)  -> (addTACList env1 [AssignChrVar    (Var (name,pos,type_)) val])
        (ExprString val)  -> (addTACList env1 [AssignStrVar    (Var (name,pos,type_)) val])
        (ExprFloat  val)  -> (addTACList env1 [AssignFloatVar  (Var (name,pos,type_)) val])
        (ExprTrue   val)  -> (addTACList env1 [AssignTrueVar   (Var (name,pos,type_)) val])
        (ExprFalse  val)  -> (addTACList env1 [AssignFalseVar  (Var (name,pos,type_)) val])
        -- effettivo assegnamento con espressione complessa a destra
        _                 -> generateAssign (generateExpr env1 type_ expr) type_ id OpAssign
    StmtDefInit id guard expr                      -> generateStmt env type_ (StmtVarInit id guard expr) -- constant inizialization or assignement
    StmtReturn (PReturn (pos,name)) expr -> do -- return stmt
      let (program1,temp_count1,variables1,label_count1) = (generateExpr env type_ expr) 
      ([Return (Temp (temp_count1-1,type_))] ++ program1,temp_count1,variables1,label_count1)    -- ritorno l'ultima variabile temporanea instanziata
    SComp (StmtBlock decls) -> generateTAC_int env (PDefs decls) -- compound statement
    StmtIfThenElse bexpr stmtsT stmtsF ->  do -- if then else
      let (program1, temp_count1, variables1,label_count1) = (generateExpr env type_ bexpr)
      addTACList (generateStmt (addTACList (generateStmt ([IfFalse (Temp (temp_count1-1,TypeBool)) (Label ("if_false",label_count1) )]++program1, temp_count1, variables1, label_count1+1) type_ (SComp stmtsT)) [Goto (Label ("end_if",label_count1) ),Lbl (Label ("if_false",label_count1) )]) type_ (SComp stmtsF)) [Lbl (Label ("end_if",label_count1) )]
    StmtIfThen bexpr stmts -> do -- if then
      let (program1, temp_count1, variables1, label_count1) = (generateExpr env TypeBool bexpr)
      addTACList (generateStmt ([IfFalse (Temp (temp_count1-1,TypeBool)) (Label ("end_if", label_count1) )]++program1,temp_count1,variables1, label_count1) type_ (SComp stmts) ) [Lbl (Label ("end_if", label_count1) )]
    -- switch case del default prendo solo il primo
    StmtSwitchCase expr norm_cases ((CaseDefault dflt_stms):_) -> do
      --TODO controllare puntamenti ed etichette (program1, temp_count1, variables1,label_count1)
      let env1 = addTACList (generateExpr env type_ expr) [Goto (Label ("case_conditions",label_count) )]
      -- normcases   -> stmts + jump_to_end
      let env2 = generateCases env1 type_ norm_cases
      let env3 = addTACList (generateStmt (addTACList env2 [Lbl (Label ("match_dflt", label_count) )]) type_ (SComp dflt_stms)) [Goto (Label ("end_case",label_count) )]
      let env4 = addTACList env3 [Lbl (Label ("case_conditions", label_count) )]
      let env5 = generateCasesCond env4 type_ norm_cases
      -- condition jump
      let env6 = addTACList env5 [Goto (Label ("match_dflt",label_count) )]
      -- end case
      addTACList env6 [Lbl (Label ("end_case", label_count) )]
    StmtBreak break -> addTACList env [Goto (Label ("end_??",label_count) )]
    StmtContinue continue -> addTACList env [Goto (Label ("guard_??",label_count) )]     
    StmtWhile bexpr (StmtBlock decls) ->  do -- while stmt
      let (program1,temp_count1,variables1,label_count1) =  (generateExpr (generateTAC_int (addTACList env [Goto (Label ("gaurd",label_count) ),Lbl (Label ("body",label_count) )]) (PDefs decls)) TypeBool bexpr)
      ([Lbl (Label ("end_while",label_count) ),If (Temp (temp_count1-1,TypeBool)) (Label ("body",label_count) ),Lbl (Label ("guard",label_count) )]++program1,temp_count1,variables1, label_count1) 
    StmtFor id range stmts -> env -- TODO for


generateCases :: Env -> Type -> [NormCase] -> Env
generateCases env _ [] = env 
generateCases env@(program, temp_count, variables,label_count) type_ ((CaseNormal _ stmts):rest) = 
  generateCases ( addTACList (generateStmt (addTACList env [Lbl (Label ("match_", label_count) )]) type_ (SComp stmts)) [Goto (Label ("end_case",label_count) )] ) type_ rest 

generateCasesCond :: Env -> Type -> [NormCase] -> Env
generateCasesCond env _ [] = env 
generateCasesCond env@(program, temp_count, variables,label_count) type_ ((CaseNormal expr _):rest) = do
  let (program1, temp_count1, variables1,label_count1) = (generateExpr env type_ expr)
  generateCasesCond ([If (Temp (temp_count1-1,TypeBool)) (Label ("end_if", label_count1) )]++program1, temp_count1,variables, label_count1+1) type_ rest 
  
generateExpr :: Env -> Type -> Expr -> Env
generateExpr env@(program, temp_count, variables,label_count) type_ expr = 
    case expr of
        -- assign expression to variable
        ExprAssign   (LExprId id) op re    -> generateAssign (generateExpr env type_ re) type_ id op
        -- variable inside expression
        ExprLeft     (LExprId id@(PIdent (pos,name)))          -> do
          let (env1@(_, _, new_variables,_), var) = findVar env (Var (name,pos,type_))
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
          let (program1, temp_count1, variables1, label_count1) = (generateExpr env type_ expr1)
          let (program2, temp_count2, variables2, label_count2) = (generateExpr ([If (Temp (temp_count1-1,TypeBool)) (Label ("true_or", label_count1) )]++program1,temp_count1, variables1, label_count1+1) type_ expr2)
          ([Lbl (Label ("end_or",label_count1) ),AssignTrueTemp  (Temp (temp_count2,TypeBool)) (PTrue ((0,0),"true")),Lbl (Label ("true_or",label_count1) ),Goto (Label ("end_or",label_count1) ),AssignFalseTemp  (Temp (temp_count2,TypeBool)) (PFalse ((0,0),"false")), If (Temp (temp_count2-1,TypeBool)) (Label ("true_or", label_count1) )]++program2,temp_count2+1, variables2, label_count2)
          
        ExprAnd      expr1 expr2 -> do --binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpAnd
          let (program1, temp_count1, variables1, label_count1) = (generateExpr env type_ expr1)
          let (program2, temp_count2, variables2, label_count2) = (generateExpr ([IfFalse (Temp (temp_count1-1,TypeBool)) (Label ("falseAnd", label_count1) )]++program1,temp_count1, variables1, label_count1+1) type_ expr2)
          ([Lbl (Label ("end_and",label_count1) ),AssignFalseTemp  (Temp (temp_count2,TypeBool)) (PFalse ((0,0),"false")),Lbl (Label ("falseAnd",label_count1) ),Goto (Label ("end_and",label_count1) ),AssignTrueTemp  (Temp (temp_count2,TypeBool)) (PTrue ((0,0),"true")), IfFalse (Temp (temp_count2-1,TypeBool)) (Label ("false_and", label_count1) )]++program2,temp_count2+1, variables2, label_count2)
        
-- Build the binary operator using the last two temporaneus variable
binaryExpr :: Env -> Type -> BinaryOperator -> Env
binaryExpr env@(program, temp_count, variables,label_count) type_ op = ([ BinOp op (Temp (temp_count,type_)) (Temp (temp_count-2,type_)) (Temp (temp_count-1,type_))] ++ program, (temp_count+1), variables, label_count)



--StmtAssign LExpr AssignOperator Expr
generateAssign :: Env -> Type -> PIdent -> AssignOperator -> Env
generateAssign env@(program, temp_count, variables,label_count) type_ id@(PIdent (pos,name)) op = do
  let (env1@(program, temp_count, new_variables,label_count), var) = findVar env (Var (name,pos,type_))
  case op of
    OpAssign    -> (addTACList env [AssignT2V  var (Temp (temp_count-1,type_))])
    _           -> generateAssign (binaryExpr ([AssignV2T (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_variables, label_count) type_ op1) type_ id OpAssign
                    where op1 = case op of 
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
  let (env1, var) = findVar env (Var (name,pos,type_))
  (generateTAC_int (addTACList env1 [FuncDef var]) (PDefs decls))
    where type_ = case guard of
                    GuardVoid    -> TypeVoid
                    GuardType t_ -> t_



-- use temp variables as parameters
generateCallFunc :: Env -> PIdent -> [Expr] -> Type ->Env
generateCallFunc  env@(program, temp_count, variables,label_count) (PIdent (pos, name)) params type_ = do
  let (_, var)              = findVar env (Var (name,pos,type_)) -- prendo il tipo della funzione
  (addTACList (generateParams env params) [FuncCall var (Temp (temp_count,type_))])

generateParams :: Env -> [Expr] -> Env
generateParams env [] = env
generateParams env@(program, temp_count, variables,label_count) (param:params) = generateParams (addTACList (generateExpr env TypeVoid param) [AssignT2P (Temp (temp_count,TypeVoid))]) params
