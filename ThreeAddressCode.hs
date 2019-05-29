module ThreeAddressCode where

--import Control.Monad.State

import AbsE
import PrintE

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Environment = (Program, tempValue, variables)
type Env          = ([TAC], Temp_count, VariablesMap,LabelsSet, LabelsCount)
type Temp_count   = Int
type VariablesMap = Map.Map String Var         -- Variables context: String -> Var
type LabelsSet    = Set.Set Label
type LabelsCount  = Int


 -- Initialize the tacprogram with an empty list.
generateTAC :: [Program] -> Env
generateTAC progs = generateTAC_int ([],0, Map.empty,Set.empty,0) (PDefs (initTAC progs))

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


-- add a new label to the env
addLbl :: Env -> String -> Maybe Int -> (Env,Label)
addLbl env@(program, temp_count, variables,labels,labels_count) name id_ =
  case id_ of
    Nothing  -> ((program, temp_count, variables,(Set.insert (Label (name,labels_count)) labels),labels_count+1),(Label (name,labels_count)))
    Just id  -> ((program, temp_count, variables,(Set.insert (Label (name,id          )) labels),labels_count  ),(Label (name,id          )))
findLbl :: Env -> Label -> (Env,Label)
findLbl env@(program, temp_count, variables,labels,labels_count) label@(Label (name,id)) = 
  case Set.member label labels of
    -- the label is new
    False -> (addLbl env name (Just id))
    -- the label has already been declared
    True  -> (env, label)  



-- add a new variable to the env
addVar :: Env -> Var -> (Env,Var)
addVar env@(program, temp_count, variables,labels,labels_count) var@(Var (name,pos,type_)) = 
  ((program, temp_count, (Map.insert name var variables),labels, labels_count), var)
findVar :: Env -> Var -> (Env,Var)
findVar env@(program, temp_count, variables,labels,labels_count) var@(Var (name,pos,type_)) = 
  case Map.lookup name variables of
    -- the variable is new
    Nothing     -> (addVar env var)
    -- the variable has already been declared
    Just old_var -> (env, old_var)



addTACList :: Env -> [TAC] -> Env
addTACList env [] = env
addTACList env@(program, temp_count, variables,labels,labels_count) (tac:rest) = addTACList (addTAC env tac) rest
addTAC :: Env -> TAC -> Env
addTAC env@(program, temp_count, variables,labels,labels_count) tac =
  case tac of
    Return (Temp (_,type_)) -> ([Return (Temp (temp_count-1,type_))] ++ program,temp_count,variables,labels,labels_count)
    Lbl lbl                 -> ([Lbl lbl]++program , temp_count, variables, (Set.insert lbl labels),labels_count+1)
    IfFalse temp (Label (name,_)) -> do
      let ((_, _, _,labels1,labels_count1),lbl_new) = addLbl env name (Just labels_count)
      ([IfFalse (Temp (temp_count-1,TypeBool)) lbl_new]++program, temp_count, variables, labels1, labels_count1)
    If temp (Label (name,_)) -> do
      let ((_, _, _,labels1,labels_count1),lbl_new) = addLbl env name (Just labels_count)
      ([If      (Temp (temp_count-1,TypeBool)) lbl_new]++program, temp_count, variables, labels1, labels_count1)
    _                       -> ([tac]++program , temp_count, variables, labels,labels_count)



generateStmt :: Env  -> Type -> Stmt -> Env 
generateStmt env@(program, temp_count, variables,labels,labels_count) type_ stmt = 
  case stmt of
    StmtExpr expr                                 -> generateExpr env type_ expr  -- expression statement
    
    StmtVarInit id@(PIdent (pos,name)) guard expr -> do                           -- variable initialization or assignement
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
    StmtBreak break -> addTACList env [Goto (Label ("end_??",labels_count) )]
    StmtContinue continue -> addTACList env [Goto (Label ("guard_??",labels_count) )]     
    StmtWhile bexpr (StmtBlock decls) ->  do -- while stmt
      (addTACList (generateExpr (generateTAC_int (addTACList env [Goto (Label ("gaurd",labels_count) ),Lbl (Label ("body",labels_count) )]) (PDefs decls)) TypeBool bexpr) [Lbl (Label ("end_while",labels_count) ),If (Temp (0,TypeBool)) (Label ("body",labels_count) ),Lbl (Label ("guard",labels_count) )])
    StmtFor id range stmts -> env -- TODO for


generateCases :: Env -> Type -> [NormCase] -> Env
generateCases env _ [] = env 
generateCases env@(program, temp_count, variables,labels,labels_count) type_ ((CaseNormal _ stmts):rest) = 
  generateCases ( addTACList (generateStmt (addTACList env [Lbl (Label ("match_", labels_count) )]) type_ (SComp stmts)) [Goto (Label ("end_case",labels_count) )] ) type_ rest 

generateCasesCond :: Env -> Type -> [NormCase] -> Env
generateCasesCond env _ [] = env 
generateCasesCond env@(program, temp_count, variables,labels,labels_count) type_ ((CaseNormal expr _):rest) = 
  generateCasesCond (addTACList (generateExpr env type_ expr) [If (Temp (0,TypeBool)) (Label ("match_", labels_count) )]) type_ rest 
  
generateExpr :: Env -> Type -> Expr -> Env
generateExpr env@(program, temp_count, variables,labels,labels_count) type_ expr = 
    case expr of
        -- assign expression to variable
        ExprAssign   (LExprId id) op re    -> generateAssign (generateExpr env type_ re) type_ id op
        -- variable inside expression
        ExprLeft     (LExprId id@(PIdent (pos,name)))          -> do
          let (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))
          ([AssignV2T   (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_variables, labels, labels_count)
 
        ExprInt      val         -> ([AssignIntTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count)
        ExprChar     val         -> ([AssignChrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count)
        ExprString   val         -> ([AssignStrTemp   (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count)
        ExprFloat    val         -> ([AssignFloatTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count)
        ExprTrue     val         -> ([AssignTrueTemp  (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count)
        ExprFalse    val         -> ([AssignFalseTemp (Temp (temp_count,type_)) val] ++ program, (temp_count+1), variables, labels, labels_count)
    
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
          let (program1, temp_count1, variables1, labels1, labels_count1) = (generateExpr env type_ expr1)
          let (program2, temp_count2, variables2, labels2, labels_count2) = (generateExpr ([If (Temp (temp_count1-1,TypeBool)) (Label ("true_or", labels_count1) )]++program1,temp_count1, variables1, labels1, labels_count1+1) type_ expr2)
          ([Lbl (Label ("end_or",labels_count1) ),AssignTrueTemp  (Temp (temp_count2,TypeBool)) (PTrue ((0,0),"true")),Lbl (Label ("true_or",labels_count1) ),Goto (Label ("end_or",labels_count1) ),AssignFalseTemp  (Temp (temp_count2,TypeBool)) (PFalse ((0,0),"false")), If (Temp (temp_count2-1,TypeBool)) (Label ("true_or", labels_count1) )]++program2,temp_count2+1, variables2, labels2, labels_count2+1)
          
        ExprAnd      expr1 expr2 -> do --binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpAnd
          let (program1, temp_count1, variables1, labels1, labels_count1) = (generateExpr env type_ expr1)
          let (program2, temp_count2, variables2, labels2, labels_count2) = (generateExpr ([IfFalse (Temp (temp_count1-1,TypeBool)) (Label ("falseAnd", labels_count1) )]++program1,temp_count1, variables1, labels1, labels_count1+1) type_ expr2)
          ([Lbl (Label ("end_and",labels_count1) ),AssignFalseTemp  (Temp (temp_count2,TypeBool)) (PFalse ((0,0),"false")),Lbl (Label ("falseAnd",labels_count1) ),Goto (Label ("end_and",labels_count1) ),AssignTrueTemp  (Temp (temp_count2,TypeBool)) (PTrue ((0,0),"true")), IfFalse (Temp (temp_count2-1,TypeBool)) (Label ("false_and", labels_count1) )]++program2,temp_count2+1, variables2, labels2,labels_count2+1)
        
-- Build the binary operator using the last two temporaneus variable
binaryExpr :: Env -> Type -> BinaryOperator -> Env
binaryExpr env@(program, temp_count, variables,labels,labels_count) type_ op = ([ BinOp op (Temp (temp_count,type_)) (Temp (temp_count-2,type_)) (Temp (temp_count-1,type_))] ++ program, (temp_count+1), variables, labels, labels_count)



--StmtAssign LExpr AssignOperator Expr
generateAssign :: Env -> Type -> PIdent -> AssignOperator -> Env
generateAssign env@(program, temp_count, variables,labels,labels_count) type_ id@(PIdent (pos,name)) op = do
  let (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))
  case op of
    OpAssign    -> (addTACList env [AssignT2V  var (Temp (temp_count-1,type_))])
    _           -> generateAssign (binaryExpr ([AssignV2T (Temp (temp_count,type_)) var] ++ program, (temp_count+1), new_variables, labels, labels_count) type_ op1) type_ id OpAssign
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
generateDeclFunc env@(program, temp_count, variables,labels,labels_count) lexpr@(LExprId (PIdent (pos, name))) args guard stmt@(StmtBlock decls) = do
  let (env1, var) = findVar env (Var (name,pos,type_))
  (generateTAC_int (addTACList env1 [FuncDef var]) (PDefs decls))
    where type_ = case guard of
                    GuardVoid    -> TypeVoid
                    GuardType t_ -> t_



-- use temp variables as parameters
generateCallFunc :: Env -> PIdent -> [Expr] -> Type ->Env
generateCallFunc  env@(program, temp_count, variables,labels,labels_count) (PIdent (pos, name)) params type_ = do
  let (_, var)              = findVar env (Var (name,pos,type_)) -- prendo il tipo della funzione
  (addTACList (generateParams env params) [FuncCall var (Temp (temp_count,type_))])

generateParams :: Env -> [Expr] -> Env
generateParams env [] = env
generateParams env@(program, temp_count, variables,labels,labels_count) (param:params) = generateParams (addTACList (generateExpr env TypeVoid param) [AssignT2P (Temp (temp_count,TypeVoid))]) params
