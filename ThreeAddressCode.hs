module ThreeAddressCode where

--import Control.Monad.State

import AbsE
import PrintE

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Environment = (Program, tempValue, variables, Scope)
type Env          = ([TAC], Temp_count, VariablesMap,LabelsSet, LabelsCount, Scope)
type Temp_count   = Int
type VariablesMap = Map.Map String Var         -- Variables context: String -> Var
type LabelsSet    = Set.Set Label
type LabelsCount  = Int
type Scope        = Int


 -- Initialize the tacprogram with an empty list.
generateTAC :: [Program] -> Env
generateTAC progs = generateTAC_int ([],0, Map.empty,Set.empty,0,0) (PDefs (initTAC progs))

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
        DeclFun   id args type_ stmts -> generateDeclFunc env id args type_ stmts
        DeclStmt (stmt)               -> generateStmt env new_type stmt
          where new_type = case maybe_type of 
                              Nothing                     -> TypeVoid
                              (Just (TypeBasicType    type_)) -> type_
                              --(Just (TypeCompoundType type_)) -> type_


-- add a new label to the env
addLbl :: Env -> String -> Maybe Int -> (Env,Label)
addLbl env@(program, temp_count, variables,labels,labels_count,scope) name id_ =
  case id_ of
    Nothing  -> ((program, temp_count, variables,(Set.insert (Label (name,labels_count)) labels),labels_count+1,scope),(Label (name,labels_count)))
    Just id  -> ((program, temp_count, variables,(Set.insert (Label (name,id          )) labels),labels_count  ,scope),(Label (name,id          )))
findLbl :: Env -> Label -> (Env,Label)
findLbl env@(program, temp_count, variables,labels,labels_count,scope) label@(Label (name,id)) = 
  case Set.member label labels of
    -- the label is new
    False -> (addLbl env name (Just id))
    -- the label has already been declared
    True  -> (env, label)  



-- add a new variable to the env
addVar :: Env -> Var -> (Env,Var)
addVar env@(program, temp_count, variables,labels,labels_count,scope) var@(Var (name,pos,type_)) = 
  ((program, temp_count, (Map.insert name var variables),labels, labels_count,scope), var)
findVar :: Env -> Var -> (Env,Var)
findVar env@(program, temp_count, variables,labels,labels_count,scope) var@(Var (name,pos,type_)) = 
  case Map.lookup name variables of
    -- the variable is new
    Nothing     -> (addVar env var)
    -- the variable has already been declared
    Just old_var -> (env, old_var)



addTACList :: Env -> [TAC] -> Env
addTACList env [] = env
addTACList env@(program, temp_count, variables,labels,labels_count,scope) (tac:rest) = addTACList (addTAC env tac) rest
addTAC :: Env -> TAC -> Env
addTAC env@(program, temp_count, variables,labels,labels_count,scope) tac =
  case tac of
    Return (Temp (_,type_)) -> ([Return (Temp (temp_count-1,type_))] ++ program,temp_count,variables,labels,labels_count,scope)
    Lbl lbl                 -> ([Lbl lbl]++program , temp_count, variables, (Set.insert lbl labels),labels_count+1,scope)
    IfFalse temp (Label (name,id)) -> do
      let ((_, _, _,labels1,labels_count1,_),lbl_new) = addLbl env name (Just id)
      ([IfFalse (Temp (temp_count-1,TypeBool)) lbl_new]++program, temp_count, variables, labels1, labels_count1,scope)
    If temp (Label (name,id)) -> do
      let ((_, _, _,labels1,labels_count1,_),lbl_new) = addLbl env name (Just id)
      ([If      (Temp (temp_count-1,TypeBool)) lbl_new]++program, temp_count, variables, labels1, labels_count1,scope)
    _                       -> ([tac]++program , temp_count, variables, labels,labels_count,scope)



generateStmt :: Env  -> BasicType -> Stmt -> Env 
generateStmt env@(program, temp_count, variables,labels,labels_count,scope) type_ stmt = 
  case stmt of
    StmtExpr expr                                 -> generateExpr env type_ expr  -- expression statement
    
    StmtVarInit id@(PIdent (pos,name)) guard (ExprSimple expr) -> do                           -- variable initialization or assignement
      let (env1,var) = addVar env (Var (name,pos,type_))
      case expr of
        -- shortcut per evitare t0 = 1; var = t0
        (ExprInt    val)                          -> (addTACList env1 [AssignIntVar    var val])
        (ExprChar   val)                          -> (addTACList env1 [AssignChrVar    var val])
        (ExprString val)                          -> (addTACList env1 [AssignStrVar    var val])
        (ExprFloat  val)                          -> (addTACList env1 [AssignFloatVar  var val])
        (ExprTrue   val)                          -> (addTACList env1 [AssignTrueVar   var val])
        (ExprFalse  val)                          -> (addTACList env1 [AssignFalseVar  var val])
        -- effettivo assegnamento con espressione complessa a destra
        _                                         -> generateAssign (generateExpr env1 type_ expr) type_ id OpAssign
    
    -- constant inizialization or assignement
    StmtDefInit id guard expr                     -> generateStmt env type_ (StmtVarInit id guard expr) 
    
    -- return stmt
    StmtReturn (PReturn (pos,name)) expr          -> (addTACList (generateExpr env type_ expr)  [Return (Temp (0,type_))]) -- ritorno l'ultima variabile temporanea instanziata
    
    -- compound statement
    SComp (StmtBlock decls)                       -> generateTAC_int env (PDefs decls) 

    -- if then else
    StmtIfThenElse bexpr stmtsT stmtsF            -> addTACList (generateStmt (addTACList (generateStmt (addTACList (generateExpr env type_ bexpr) [IfFalse (Temp (0,TypeBool)) (Label ("if_false",labels_count) )]) type_ (SComp stmtsT)) [Goto (Label ("end_if",labels_count) ),Lbl (Label ("if_false",labels_count) )]) type_ (SComp stmtsF)) [Lbl (Label ("end_if",labels_count) )]
    
    -- if then
    StmtIfThen bexpr stmts -> addTACList (generateStmt (addTACList (generateExpr env TypeBool bexpr) [IfFalse (Temp (0,TypeBool)) (Label ("end_if", labels_count) )]) type_ (SComp stmts) ) [Lbl (Label ("end_if", labels_count) )]
    
    -- switch case del default prendo solo il primo
    StmtSwitchCase expr norm_cases ((CaseDefault dflt_stms):_) -> do
      --TODO controllare puntamenti ed etichette (program1, temp_count1, variables1,labels1)
      let env1 = addTACList (generateExpr env type_ expr) [Goto (Label ("case_conditions",labels_count) )]
      -- normcases   -> stmts + jump_to_end
      let env2 = generateCases env1 type_ norm_cases
      let env3 = addTACList (generateStmt (addTACList env2 [Lbl (Label ("match_dflt", labels_count) )]) type_ (SComp dflt_stms)) [Goto (Label ("end_case",labels_count) )]
      let env4 = addTACList env3 [Lbl (Label ("case_conditions", labels_count) )]
      let env5 = generateCasesCond env4 type_ norm_cases
      -- condition jump
      let env6 = addTACList env5 [Goto (Label ("match_dflt",labels_count) )]
      -- end case
      addTACList env6 [Lbl (Label ("end_case", labels_count) )]
    
    -- break stmt
    StmtBreak break -> addTACList env [Goto (Label ("end_cycle",scope-1) )]
    
    -- continue stmt
    StmtContinue continue -> addTACList env [Goto (Label ("guard",scope-1) )] 

     -- while stmt    
    StmtWhile bexpr (StmtBlock decls) -> (addTACList (generateExpr (addTACList (generateTAC_int ([Lbl (Label ("body",scope)),Goto (Label ("gaurd",scope))]++program, temp_count,variables,labels,labels_count,scope+1) (PDefs decls)) [Lbl (Label ("guard",scope))]) TypeBool bexpr) [If (Temp (0,TypeBool)) (Label ("body",scope)),Lbl (Label ("end_cycle",scope))])
    
    -- TODO for
    StmtFor id@(PIdent (pos,name)) (ExprRange start_for end_for) (StmtBlock decls) -> do
      let (env1, var) = findVar env (Var (name,pos,TypeInt)) -- trovo la variabile
      let  env2@(program1, temp_count1, variables1, labels1, labels_count1, scope1) = generateAssign (generateExpr env1 TypeInt (for_bound_identifier env start_for)) TypeInt id OpAssign -- la valorizzo con start_for
      let  env3      = (generateTAC_int 
                            ([Lbl (Label ("body",scope)),Goto (Label ("gaurd",scope))]++program1, temp_count1,variables1,labels1,labels_count1,scope1+1) 
                            (PDefs decls)
                          )
                           
      let env4        = generateExpr env3 TypeInt (ExprAssign (LExprId id) OpAssign (ExprPlus (ExprLeft (LExprId id)) (ExprInt (PInteger ((0,0),"1")))))
      let env5        = (addTACList  -- ciclo 
                            env4
                          [Lbl (Label ("guard",scope))]
                        )
      let env6        = generateExpr env5 TypeBool (ExprLt (ExprLeft (LExprId id)) (for_bound_identifier env end_for))
      (addTACList env6 [If (Temp (0,TypeBool)) (Label ("body",scope)),Lbl (Label ("end_cycle",scope))])

for_bound_identifier :: Env -> ForId -> Expr
for_bound_identifier env for_id =
  case for_id of
    ForInteger val -> ExprInt val
    ForIdent   id  -> ExprLeft (LExprId id)

generateCases :: Env -> BasicType -> [NormCase] -> Env
generateCases env _ [] = env 
generateCases env@(program, temp_count, variables,labels,labels_count,scope) type_ ((CaseNormal _ stmts):rest) = 
  generateCases ( addTACList (generateStmt (addTACList env [Lbl (Label ("match_", labels_count) )]) type_ (SComp stmts)) [Goto (Label ("end_case",labels_count) )] ) type_ rest 

generateCasesCond :: Env -> BasicType -> [NormCase] -> Env
generateCasesCond env _ [] = env 
generateCasesCond env@(program, temp_count, variables,labels,labels_count,scope) type_ ((CaseNormal expr _):rest) = 
  generateCasesCond (addTACList (generateExpr env type_ expr) [If (Temp (0,TypeBool)) (Label ("match_", labels_count) )]) type_ rest 
  
generateExpr :: Env -> BasicType -> Expr -> Env
generateExpr env@(program, temp_count, variables,labels,labels_count,scope) type_ expr = 
    case expr of
        -- assign expression to variable
        ExprAssign   (LExprId id) op re    -> generateAssign (generateExpr env type_ re) type_ id op
        -- variable inside expression
        ExprLeft     (LExprId id@(PIdent (pos,name)))          -> do
          let (env1@(_, _, new_variables,_,_,_), var) = findVar env (Var (name,pos,type_))
          ([AssignV2T   (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_variables, labels, labels_count,scope)
 
        ExprInt      val         -> ([AssignIntTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count,scope)
        ExprChar     val         -> ([AssignChrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count,scope)
        ExprString   val         -> ([AssignStrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count,scope)
        ExprFloat    val         -> ([AssignFloatTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count,scope)
        ExprTrue     val         -> ([AssignTrueTemp  (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count,scope)
        ExprFalse    val         -> ([AssignFalseTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count,scope)
    
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
          let (program1, temp_count1, variables1, labels1, labels_count1,_) = (generateExpr env type_ expr1)
          let (program2, temp_count2, variables2, labels2, labels_count2,_) = (generateExpr ([If (Temp (temp_count1-1,TypeBool)) (Label ("true_or", labels_count1) )]++program1,temp_count1, variables1, labels1, labels_count1+1,scope) type_ expr2)
          ([Lbl (Label ("end_or",labels_count1) ),AssignTrueTemp  (Temp (temp_count2,TypeBool)) (PTrue ((0,0),"true")),Lbl (Label ("true_or",labels_count1) ),Goto (Label ("end_or",labels_count1) ),AssignFalseTemp  (Temp (temp_count2,TypeBool)) (PFalse ((0,0),"false")), If (Temp (temp_count2-1,TypeBool)) (Label ("true_or", labels_count1) )]++program2,temp_count2+1, variables2, labels2, labels_count2+1,scope)
          
        ExprAnd      expr1 expr2 -> do --binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpAnd
          let (program1, temp_count1, variables1, labels1, labels_count1,_) = (generateExpr env type_ expr1)
          let (program2, temp_count2, variables2, labels2, labels_count2,_) = (generateExpr ([IfFalse (Temp (temp_count1-1,TypeBool)) (Label ("falseAnd", labels_count1) )]++program1,temp_count1, variables1, labels1, labels_count1+1,scope) type_ expr2)
          ([Lbl (Label ("end_and",labels_count1) ),AssignFalseTemp  (Temp (temp_count2,TypeBool)) (PFalse ((0,0),"false")),Lbl (Label ("falseAnd",labels_count1) ),Goto (Label ("end_and",labels_count1) ),AssignTrueTemp  (Temp (temp_count2,TypeBool)) (PTrue ((0,0),"true")), IfFalse (Temp (temp_count2-1,TypeBool)) (Label ("false_and", labels_count1) )]++program2,temp_count2+1, variables2, labels2,labels_count2+1,scope)
        
-- Build the binary operator using the last two temporaneus variable
binaryExpr :: Env -> BasicType -> BinaryOperator -> Env
binaryExpr env@(program, temp_count, variables,labels,labels_count,scope) type_ op = ([ BinOp op (Temp (temp_count,type_)) (Temp (temp_count-2,type_)) (Temp (temp_count-1,type_))] ++ program, (temp_count+1), variables, labels, labels_count,scope)



--StmtAssign LExpr AssignOperator Expr
generateAssign :: Env -> BasicType -> PIdent -> AssignOperator -> Env
generateAssign env@(program, temp_count, variables,labels,labels_count,scope) type_ id@(PIdent (pos,name)) op = do
  let (env1@(_, _, new_variables,_,_,_), var) = findVar env (Var (name,pos,type_))
  case op of
    OpAssign    -> (addTACList env [AssignT2V  var (Temp (temp_count-1,type_))])
    _           -> generateAssign (binaryExpr ([AssignV2T (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_variables, labels, labels_count,scope) type_ op1) type_ id OpAssign
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
generateDeclFunc env@(program, temp_count, variables,labels,labels_count,scope) lexpr@(LExprId (PIdent (pos, name))) args guard stmt@(StmtBlock decls) = do
  let (env1, var) = findVar env (Var (name,pos,type_))
  (generateTAC_int (addTACList env1 [FuncDef var]) (PDefs decls))
    where type_ = case guard of
                    GuardVoid    -> TypeVoid
                    GuardType (TypeBasicType t_) -> t_



-- use temp variables as parameters
generateCallFunc :: Env -> PIdent -> [Expr] -> BasicType ->Env
generateCallFunc  env@(program, temp_count, variables,labels,labels_count,scope) (PIdent (pos, name)) params type_ = do
  let (_, var)              = findVar env (Var (name,pos,type_)) -- prendo il tipo della funzione
  (addTACList (generateParams env params) [FuncCall var (Temp (temp_count,type_))])

generateParams :: Env -> [Expr] -> Env
generateParams env [] = env
generateParams env@(program, temp_count, variables,labels,labels_count,scope) (param:params) = generateParams (addTACList (generateExpr env TypeVoid param) [AssignT2P (Temp (temp_count,TypeVoid))]) params
