module ThreeAddressCode where

  --import Control.Monad.State
  
  import AbsE
  import PrintE
  
  import Prelude hiding (lookup)
  import qualified Data.Map as Map
  
  -- Environment = (Program, tempValue, variables, Scope)
  type Env          = ([TAC], LastTemp, VariablesMap, LabelsCount, Scope)
  type LastTemp     = Temp
  type VariablesMap = Map.Map String Var         -- Variables context: String -> Var
  type LabelsCount  = Int
  type Scope        = Int
  
  
   -- Initialize the tacprogram with an empty list.
  generateTAC :: [Program] -> Env
  generateTAC progs = generateTAC_int ([],(Temp (-1,(TypeBasicType TypeVoid))), Map.empty,0,0) (PDefs (initTAC progs))
  
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
      TypedDecl (ADecl type_ decl1) -> generateDecl     env (Just type_) decl1
      DeclFun   id args type_ stmts -> generateDeclFunc env id args type_ stmts
      DeclStmt (stmt)               -> generateStmt     env new_type stmt
        where new_type = case maybe_type of 
                            Nothing      -> (TypeBasicType TypeVoid)
                            (Just type_) -> type_
   
  
  
  generateStmt :: Env  -> Type -> Stmt -> Env 
  generateStmt env@(program, last_temp, variables,labels,scope) type_ stmt = 
    case stmt of
      StmtExpr expr                                 -> generateExpr env type_ expr  -- expression statement
      
      StmtVarInit id@(PIdent (pos,name)) guard expr -> do                           -- variable initialization or assignement
        let (env1,var) = addVar env (Var (name,pos,type_))
        case expr of
          -- shortcut per evitare t0 = 1; var = t0
          (ExprSimple (ExprInt    val))             -> (addTACList  env1 [AssignIntVar    var val])
          (ExprSimple (ExprChar   val))             -> (addTACList  env1 [AssignChrVar    var val])
          (ExprSimple (ExprString val))             -> (addTACList  env1 [AssignStrVar    var val])
          (ExprSimple (ExprFloat  val))             -> (addTACList  env1 [AssignFloatVar  var val])
          (ExprSimple (ExprTrue   val))             -> (addTACList  env1 [AssignTrueVar   var val])
          (ExprSimple (ExprFalse  val))             -> (addTACList  env1 [AssignFalseVar  var val])
          -- assegnamento di un array
          (ExprArray  expr)                         -> (generateAssign env1 (getGuardType guard) id OpAssign (complex2SimpleExpr expr) undefined)
          -- effettivo assegnamento con espressione complessa a destra
          (ExprSimple expr_int)                     -> (generateAssign env1 (getGuardType guard) id OpAssign [expr_int] (Temp (-1,(TypeBasicType TypeInt))))
      
      -- constant inizialization or assignement
      StmtDefInit id guard expr                     -> generateStmt env type_ (StmtVarInit id guard expr) 
      
      -- return stmt
      StmtReturn (PReturn (pos,name)) expr          -> (addTACList (generateExpr env type_ expr)  [Return undefined]) -- ritorno l'ultima variabile temporanea instanziata
      
      -- compound statement
      SComp (StmtBlock decls)                       -> generateTAC_int env (PDefs decls) 
  
      -- if then else
      StmtIfThenElse bexpr stmtsT stmtsF            -> addTACList (generateStmt (addTACList (generateStmt (addTACList (generateExpr env (TypeBasicType TypeBool) bexpr) [IfFalse Empty (Label ("if_false",labels) )]) type_ (SComp stmtsT)) [Goto (Label ("end_if",labels) ),Lbl (Label ("if_false",labels) )]) type_ (SComp stmtsF)) [Lbl (Label ("end_if",labels) )]
      
      -- if then
      StmtIfThen bexpr stmts -> addTACList (generateStmt (addTACList (generateExpr env (TypeBasicType TypeBool) bexpr) [IfFalse Empty (Label ("end_if", labels))]) type_ (SComp stmts) ) [Lbl (Label ("end_if", labels) )]
      
      -- switch case del default prendo solo il primo
      StmtSwitchCase expr norm_cases ((CaseDefault dflt_stms):_) -> do
        let (program4, last_temp4, variables4, labels4, scope4) = addTACList (addTACList (generateStmt (addTACList (generateCases ((addTACList env [Goto (Label ("case_conditions",labels) )])) type_ norm_cases (Label ("end_case",labels) )) [Lbl (Label ("match_dflt", labels) )]) type_ (SComp dflt_stms)) [Goto (Label ("end_case",scope) )]) [Lbl (Label ("case_conditions", labels) )]
        addTACList (addTACList (generateCasesCond (program4, last_temp4, variables4, labels,scope) expr type_ norm_cases) [Goto (Label ("match_dflt",labels) )]) [Lbl (Label ("end_case", labels) )]
      
      -- break stmt
      StmtBreak break -> addTACList env [Goto (Label ("end_stmt",scope-1) )]
      
      -- continue stmt
      StmtContinue continue -> addTACList env [Goto (Label ("guard",scope-1) )] 
  
       -- while stmt    
      StmtWhile bexpr (StmtBlock decls) -> (addTACList (generateExpr (addTACList (generateTAC_int ([Lbl (Label ("body",scope)),Goto (Label ("gaurd",scope))]++program, last_temp,variables,labels,scope+1) (PDefs decls)) [Lbl (Label ("guard",scope))]) (TypeBasicType TypeBool) bexpr) [If Empty (Label ("body",scope)),Lbl (Label ("end_stmt",scope))])
      
      -- for stmt
      StmtFor id@(PIdent (pos,name)) (ExprRange start_for end_for) (StmtBlock decls) -> do
        let (env1, var) = findVar env (Var (name,pos,(TypeBasicType TypeInt)))
        let (program1, last_temp1, variables1, labels1, scope1) = generateAssign env1 (TypeBasicType TypeInt) id OpAssign [(for_bound_identifier env start_for)] (Temp (-1,(TypeBasicType TypeInt)))
        (addTACList (generateExpr (addTACList (generateExpr (generateTAC_int ([Lbl (Label ("body",scope)),Goto (Label ("gaurd",scope))]++program1, last_temp1,variables1,labels1,scope1+1) (PDefs decls)) (TypeBasicType TypeInt) (ExprAssign (LExprId id) OpAssign (ExprPlus (ExprLeft (LExprId id)) (ExprInt (PInteger ((0,0),"1")))))) [Lbl (Label ("guard",scope))]) (TypeBasicType TypeBool) (ExprLt (ExprLeft (LExprId id)) (for_bound_identifier env end_for))) [If Empty (Label ("body",scope)),Lbl (Label ("end_stmt",scope))])
  

  
  generateCases :: Env -> Type -> [NormCase] -> Label -> Env
  generateCases env _ [] _ = env 
  generateCases env@(program, last_temp, variables,labels,scope) type_ ((CaseNormal _ stmts):rest) end_label = 
    generateCases ( addTACList (generateStmt (addTACList env [Lbl (Label ("match_", labels) )]) type_ (SComp stmts)) [Goto end_label] ) type_ rest end_label
  
  generateCasesCond :: Env -> Expr -> Type -> [NormCase] -> Env
  generateCasesCond env _ _ [] = env 
  generateCasesCond env@(program, last_temp, variables, labels, scope) expr_v type_ ((CaseNormal expr _):rest) = do
    let (program, last_temp, variables, labels, scope) = (booleanExpr (generateExpr (generateExpr env type_ expr_v) type_ expr) type_ BOpEq) 
    generateCasesCond ([If (head program) (Label ("match_", labels)) ]++(drop 1 program), last_temp, variables, labels+1, scope) expr_v type_ rest 
    
  

  generateExpr :: Env -> Type -> Expr -> Env
  generateExpr env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) type_ expr = do
    case expr of
        -- assign val to variable
        ExprAssign   (LExprId id) op re                                -> generateAssign env type_ id op [re] (Temp (-1,(TypeBasicType TypeInt)))
        -- assign val to array shortcut per tipi definiti
        ExprAssign   (LExprArr (LArrExpr id@(PIdent (pos,name)) (ArrSing expr_step ))) op re    -> do
          let env2@(program,step_temp,variables,labels,scope) = generateExpr env1 (TypeBasicType TypeInt) (ExprMul expr_step (ExprInt (int2PInt (sizeOf type_v)))) -- (step)*(sizeOf type_v)
          generateAssign (generateExpr env2 type_ expr) type_ id op [re] step_temp 
            where (env1, var@(Var (_,_,type_v))) = findVar env (Var (name,pos,type_))
        -- use of variable
        ExprLeft     (LExprId id@(PIdent (pos,name)))                   -> (addTACList env1 [AssignV2T   (Temp (t_c+1,type_v)) var (Temp (-1,(TypeBasicType TypeInt)))])
          where (env1, var@(Var (_,_,type_v))) = findVar env (Var (name,pos,type_))
        -- use of array 
        ExprLeft (LExprArr (LArrExpr id@(PIdent (pos,name)) (ArrSing expr_step)))  -> do 
          let env2@(_,step_temp,_,_,_) = generateExpr env1 (TypeBasicType TypeInt) (ExprMul expr_step (ExprInt (int2PInt (sizeOf type_v))))
          (addTACList env2 [AssignV2T   (Temp (t_c+1,type_v)) var step_temp]) 
          where (env1, var@(Var (_,_,type_v))) = findVar env (Var (name,pos,type_))
        -- use of multiarray
        ExprLeft (LExprArr (LArrExpr id@(PIdent (pos,name)) a)) -> do
          let env2@(program1,temp1@(Temp (t_c1,t_t1)),variables1,labels1,scope1) = (getStep env1 a  (reverse (type2Dims type_v)))
          let env3@(_,temp2@(Temp (t_c2,t_t2)),_,_,_) = ([BinOp   BOpMul (Temp (t_c1+2,t_t1)) (Temp (t_c1+1,(TypeBasicType TypeInt))) temp1, AssignIntTemp (Temp (t_c1+1,(TypeBasicType TypeInt))) (int2PInt (sizeOf type_v))] ++ program1, (Temp (t_c1+2,t_t1)), variables1,labels1,scope1)
          (addTACList env3 [AssignV2T (Temp (t_c2+1,t_t2)) var temp2]) 
          where (env1, var@(Var (_,_,type_v))) = findVar env (Var (name,pos,type_))
            
        ExprInt      val         -> ([AssignIntTemp   (Temp (t_c+1,(TypeBasicType TypeInt)   )) val] ++ program, (Temp (t_c+1,(TypeBasicType TypeInt)   )), variables, labels, scope)
        ExprChar     val         -> ([AssignChrTemp   (Temp (t_c+1,(TypeBasicType TypeChar)  )) val] ++ program, (Temp (t_c+1,(TypeBasicType TypeChar)  )), variables, labels, scope)
        ExprString   val         -> ([AssignStrTemp   (Temp (t_c+1,(TypeBasicType TypeString))) val] ++ program, (Temp (t_c+1,(TypeBasicType TypeString))), variables, labels, scope)
        ExprFloat    val         -> ([AssignFloatTemp (Temp (t_c+1,(TypeBasicType TypeFloat) )) val] ++ program, (Temp (t_c+1,(TypeBasicType TypeFloat) )), variables, labels, scope)
        ExprTrue     val         -> ([AssignTrueTemp  (Temp (t_c+1,(TypeBasicType TypeBool)  )) val] ++ program, (Temp (t_c+1,(TypeBasicType TypeBool)  )), variables, labels, scope)
        ExprFalse    val         -> ([AssignFalseTemp (Temp (t_c+1,(TypeBasicType TypeBool)  )) val] ++ program, (Temp (t_c+1,(TypeBasicType TypeBool)  )), variables, labels, scope)
    
        ExprFunCall  fun params  -> (generateCallFunc env fun params type_) 

        -- not
        ExprBoolNot   expr       -> notExpr (generateExpr env type_ expr)

        -- unary
        ExprNegation  (ExprInt val) -> ([AssignIntTemp   (Temp (t_c+1,(TypeBasicType TypeInt)   )) (int2PInt (-(pInt2Int val)))] ++ program, (Temp (t_c+1,(TypeBasicType TypeInt)   )), variables, labels, scope)
        ExprNegation  expr       -> unaryExpr (generateExpr env type_ expr) type_ UOpMinus
        ExprAddition  expr       -> unaryExpr (generateExpr env type_ expr) type_ UOpPlus

        -- deref
        ExprReference (LExprId id@(PIdent (pos,name))) -> ([DerefOp UOpDeref (Temp (t_c+1,(getPointerFromType type_v)))  var ]++program, (Temp (t_c+1,(getPointerFromType type_v))), variables, labels, scope)
          where (env1@(_, _, new_variables,_,_), var@(Var (_,_,type_v))) = findVar env (Var (name,pos,undefined))
        --ExprReference (LExprArr (LArrExpr id@(PIdent (pos,name)) (ArrSing step))) -> unaryExpr (generateExpr env type_ expr) type_ UOpDeref
        --ExprReference (LExprId id@(PIdent (pos,name))) -> unaryExpr (generateExpr env type_ expr) type_ UOpDeref  
        
        -- arithmetic
        ExprPower    expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpPower
        ExprMul      expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpMul
        ExprFloatDiv expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpFloatDiv
        ExprIntDiv   expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpIntDiv
        ExprReminder expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpRemainder
        ExprModulo   expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpModulo
        ExprPlus     expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpPlus
        ExprMinus    expr1 expr2 -> binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpMinus
  
        -- boolean
        ExprLt       expr1 expr2 -> booleanExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpLt  
        ExprGt       expr1 expr2 -> booleanExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpGt  
        ExprLtEq     expr1 expr2 -> booleanExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpLtEq
        ExprGtEq     expr1 expr2 -> booleanExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpGtEq
        ExprEq       expr1 expr2 -> booleanExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpEq  
        ExprNeq      expr1 expr2 -> booleanExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpNeq 
        
        ExprOr       expr1 expr2 -> do--binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpOr
          let (program1, last_temp1, variables1, labels1,_) = (generateExpr env type_ expr1)
          let (program2, last_temp2@(Temp (t_c,t_t)), variables2, labels2,_) = (generateExpr ([If (BoolOp  BOpEq last_temp1 (TempT (PTrue ((0,0),"true")))) (Label ("true_or", labels1) )]++program1,last_temp1, variables1, labels1+1,scope) type_ expr2)
          ([Lbl (Label ("end_or", labels1)),AssignTrueTemp  (Temp (t_c+1,(TypeBasicType TypeBool))) (PTrue ((0,0),"true")), Lbl (Label ("true_or", labels1)), Goto (Label ("end_or", labels1)), AssignFalseTemp (Temp (t_c+1,(TypeBasicType TypeBool))) (PFalse ((0,0),"false")), If (BoolOp BOpEq last_temp2 (TempT (PTrue ((0,0),"true")))) (Label ("true_or", labels1))]++program2, (Temp (t_c+1,(TypeBasicType TypeBool))), variables2, labels2+1,scope)
        ExprAnd      expr1 expr2 -> do --binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpAnd
          let (program1, last_temp1, variables1, labels1,_) = (generateExpr env type_ expr1)
          let (program2, last_temp2@(Temp (t_c,t_t)), variables2, labels2,_) = (generateExpr ([If (BoolOp BOpEq last_temp1 (TempF (PFalse ((0,0),"false")))) (Label ("false_and", labels1) )]++program1,last_temp1, variables1, labels1+1,scope) type_ expr2)
          ([Lbl (Label ("end_and",labels1)),AssignFalseTemp (Temp (t_c+1,(TypeBasicType TypeBool))) (PFalse ((0,0),"false")), Lbl (Label ("false_and",labels1) ),Goto (Label ("end_and",labels1) ), AssignTrueTemp  (Temp (t_c+1,(TypeBasicType TypeBool))) (PTrue ((0,0),"true")), If (BoolOp BOpEq last_temp2 (TempF (PFalse ((0,0),"false")))) (Label ("false_and", labels1) )]++program2,(Temp (t_c+1,(TypeBasicType TypeBool))), variables2, labels2+1,scope)
      
        _ -> env 

  -- Build the binary operator using the last two temporaneus variable
  binaryExpr :: Env -> Type -> BinaryOperator -> Env
  binaryExpr env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) type_ op  = ([BinOp   op (Temp (t_c+1,t_t)) (Temp (t_c-1,t_t)) last_temp] ++ program, (Temp (t_c+1,t_t)), variables,labels,scope)
  
  unaryExpr  :: Env -> Type -> UnaryOperator -> Env
  unaryExpr  env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) type_ op  = ([UnaryOp op (Temp (t_c+1,t_t)) last_temp]++program, (Temp (t_c+1,t_t)), variables, labels, scope)

  booleanExpr :: Env -> Type -> BinaryOperator -> Env
  booleanExpr env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) type_ op = ([BoolOp  op (Temp (t_c-1,t_t)) last_temp]++program, (Temp (t_c-1,t_t)), variables, labels, scope)

  -- switch the last boolean Expr
  notExpr :: Env -> Env
  notExpr env@(program, last_temp, variables,labels,scope) =
    case (head program) of
      BoolOp  BOpLt   t1 t2 -> ([BoolOp BOpGtEq t1 t2]++(drop 1 program), last_temp, variables, labels, scope) 
      BoolOp  BOpGt   t1 t2 -> ([BoolOp BOpLtEq t1 t2]++(drop 1 program), last_temp, variables, labels, scope) 
      BoolOp  BOpLtEq t1 t2 -> ([BoolOp BOpGt   t1 t2]++(drop 1 program), last_temp, variables, labels, scope) 
      BoolOp  BOpGtEq t1 t2 -> ([BoolOp BOpLt   t1 t2]++(drop 1 program), last_temp, variables, labels, scope) 
      BoolOp  BOpEq   t1 t2 -> ([BoolOp BOpNeq  t1 t2]++(drop 1 program), last_temp, variables, labels, scope) 
      BoolOp  BOpNeq  t1 t2 -> ([BoolOp BOpEq   t1 t2]++(drop 1 program), last_temp, variables, labels, scope) 
      _                     -> (unaryExpr env (TypeBasicType TypeBool) UOpNegate)

  --StmtAssign LExpr AssignOperator Expr
  generateAssign :: Env -> Type -> PIdent -> AssignOperator -> [Expr] -> Temp -> Env
  generateAssign env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) type_ id@(PIdent (pos,name)) op (expr:[]) step = do
    let (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))
    case op of
      OpAssign    -> (addTACList (generateExpr env (type2BasicType type_) expr) [AssignT2V  var last_temp (step)])
      _           -> generateAssign (binaryExpr (addTACList (generateExpr env (type2BasicType type_) expr) [AssignV2T (Temp (t_c+1,(type2BasicType type_))) var (step)]) type_ op1) type_ id OpAssign [] step
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
  -- usata solo all'assegnamento dell'array
  generateAssign env type_ id op (expr:rest) _ = generateAssign_int env type_ id op (expr:rest) 0

  generateAssign_int :: Env -> Type -> PIdent -> AssignOperator -> [Expr] -> Int -> Env
  generateAssign_int env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) type_ id@(PIdent (pos,name)) op (expr:[]) step = (addTACList (generateExpr env (type2BasicType type_) expr) [AssignT2V  var last_temp (Temp (step,(TypeBasicType TypeVoid)))])
    where (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))
  generateAssign_int env@(program, last_temp, variables,labels,scope) type_ id@(PIdent (pos,name)) op (expr:rest) step = (generateAssign_int (addTACList (generateExpr env (type2BasicType type_) expr) [AssignT2V  var last_temp (Temp (step,(TypeBasicType TypeVoid)))]) type_ id op rest (step+(sizeOf type_)))
    where (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))

  -- DeclFun LExpr [Arg] Guard CompStmt
  generateDeclFunc :: Env -> LExpr -> [Arg] -> Guard -> CompStmt -> Env
  generateDeclFunc env@(program, last_temp, variables,labels,scope) lexpr@(LExprId (PIdent (pos, name))) args guard stmt@(StmtBlock decls) = do
    let (env1, var) = findVar env (Var (name,pos,(getGuardType guard)))
    addTACList (generateTAC_int (addTACList env1 [FuncDef var]) (PDefs decls)) [EndFunction]
  
  -- use temp variables as parameters
  generateCallFunc :: Env -> PIdent -> [Expr] -> Type -> Env
  generateCallFunc  env@(program, last_temp, variables,labels,scope) (PIdent (pos, name)) params type_ = do
    let (_, var)    = findVar env (Var (name,pos,type_)) -- prendo il tipo della funzione
    (addTACList (generateParams env params) [FuncCall var (Temp (0,type_))])
  
  generateParams :: Env -> [Expr] -> Env
  generateParams env [] = env
  generateParams env (param:params) = generateParams (addTACList (generateExpr env undefined param) [AssignT2P undefined]) params
  


  -- --------------------------------------------------------------------------

  --                              USEFUL FUNCTIONS
  
  -- --------------------------------------------------------------------------
  -- add a new variable to the env
  addVar :: Env -> Var -> (Env,Var)
  addVar env@(program, last_temp, variables,labels,scope) var@(Var (name,pos,type_)) = ((program, last_temp, (Map.insert name var variables), labels,scope), var)
  findVar :: Env -> Var -> (Env,Var)
  findVar env@(program, last_temp, variables,labels,scope) var@(Var (name,_,_)) = 
    case Map.lookup name variables of
      -- the variable is new
      Nothing     -> (addVar env var)
      -- the variable has already been declared
      Just old_var -> (env, old_var)

  addTACList :: Env -> [TAC] -> Env
  addTACList env [] = env
  addTACList env@(program, last_temp, variables,labels,scope) (tac:rest) = addTACList (addTAC env tac) rest
  addTAC :: Env -> TAC -> Env
  addTAC env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) tac =
    case tac of
      Return _                    -> ([Return last_temp                  ]++program         , last_temp       , variables, labels  ,scope  )
      Lbl (Label ("end_stmt",_) ) -> ([Lbl (Label ("end_stmt",scope-1) ) ]++program         , last_temp       , variables, labels+1,scope-1)
      Lbl lbl                     -> ([Lbl lbl                           ]++program         , last_temp       , variables, labels+1,scope  )
      IfFalse _ lbl -> case (head program) of
        (BoolOp _ _ _)  -> ([IfFalse (head program) lbl        ]++(drop 1 program), last_temp       , variables, labels  ,scope  )
        _               -> ([IfFalse (BoolOp BOpEq last_temp (TempT (PTrue ((0,0),"true-")))) lbl ]++program, last_temp, variables, labels  ,scope  )
      If      _ lbl -> case (head program) of
        (BoolOp _ _ _)  -> ([If (head program) lbl             ]++(drop 1 program), last_temp       , variables, labels  ,scope  )
        _               -> ([If      (BoolOp BOpEq last_temp (TempT (PTrue ((0,0),"true-")))) lbl ]++program, last_temp , variables, labels  ,scope  )
      AssignV2T temp var step     -> ([AssignV2T temp   var      step    ]++program         , temp            , variables, labels  ,scope  )
      AssignT2V var _ step        -> ([AssignT2V var  last_temp  step    ]++program         , last_temp       , variables, labels  ,scope  )
      AssignT2P _                 -> ([AssignT2P last_temp               ]++program         , last_temp       , variables, labels  ,scope  )
      FuncCall var (Temp (_,t))   -> ([FuncCall  var  (Temp (t_c+1,t))   ]++program         , (Temp (t_c+1,t)), variables, labels  ,scope  )
      _                           -> ([tac                               ]++program         , last_temp       , variables, labels  ,scope  )
      
  getGuardType :: Guard -> Type
  getGuardType GuardVoid       = (TypeBasicType TypeVoid)
  getGuardType (GuardType  t_) = t_

  type2Dims :: Type -> [PInteger]
  type2Dims (TypeBasicType _) = []
  type2Dims (TypeCompoundType (CompoundTypeArrayType (ArrDefBase dims _))) = dims
  type2Dims (TypeCompoundType (CompoundTypeArrayType (ArrDefPtr  dims _))) = dims

  type2BasicType :: Type -> Type
  type2BasicType (TypeBasicType t) = (TypeBasicType t)
  type2BasicType (TypeCompoundType (CompoundTypeArrayType (ArrDefBase _ t))) = (TypeBasicType t)
  type2BasicType (TypeCompoundType (CompoundTypePtr       (Pointer  t)))     = (TypeBasicType t)

  complex2SimpleExpr :: [ComplexExpr] -> [Expr]
  complex2SimpleExpr [] = []
  complex2SimpleExpr ((ExprSimple expr): rest) = expr : (complex2SimpleExpr rest)
  complex2SimpleExpr ((ExprArray  expr): rest) = (complex2SimpleExpr expr)++(complex2SimpleExpr rest)

  getPointerFromType :: Type -> Type
  -- TODO aggiungere tutti gli altri tipi
  getPointerFromType (TypeBasicType t) = (TypeCompoundType (CompoundTypePtr (Pointer t)))
  getPointerFromType (TypeCompoundType (CompoundTypePtr p)) = (TypeCompoundType (CompoundTypePtr (Pointer2Pointer p)))
  getPointerFromType (TypeCompoundType (CompoundTypeArrayType (ArrDefBase d t))) = (TypeCompoundType (CompoundTypeArrayType (ArrDefPtr d (Pointer t))))
  getPointerFromType (TypeCompoundType (CompoundTypeArrayType (ArrDefPtr d p)))  = (TypeCompoundType (CompoundTypeArrayType (ArrDefPtr d ((Pointer2Pointer p)))))

  sizeOf :: Type -> Int
  sizeOf type_ = case (type2BasicType type_) of 
                  (TypeBasicType TypeBool  ) -> 1 -- vado a botte di parole
                  (TypeBasicType TypeFloat ) -> 4
                  (TypeBasicType TypeInt   ) -> 4
                  (TypeBasicType TypeChar  ) -> 1
                  (TypeBasicType TypeString) -> 4 -- Se stringa metto lo spazio per un puntatore a stringa

  pInt2Int :: PInteger -> Int
  pInt2Int (PInteger (pos,value)) = read value :: Int
  int2PInt :: Int -> PInteger
  int2PInt value = (PInteger ((0,0),show value))


  getStep :: Env -> AExpr -> [PInteger] -> Env
  getStep env@(program, last_temp, variables, labels, scope) a (dim:rest) = case a of
    (ArrSing step)   -> (generateExpr env (TypeBasicType TypeInt) step)
    (ArrMul a1 step) -> do
    -- valuto l'espressione
    let env1@(program1,temp1@(Temp (t_c1,t_t1)),variables1,labels1,scope1) = (generateExpr env (TypeBasicType TypeInt) step)
    -- carico la dimensione di una riga e la moltiplico per l'espressione calcolata
    let env2@(_,temp2@(Temp (t_c2,t_t2)),_,_,_) = ([BinOp   BOpMul (Temp (t_c1+2,t_t1)) temp1 (Temp (t_c1+1,t_t1)), AssignIntTemp (Temp (t_c1+1,t_t1)) dim] ++ program1, (Temp (t_c1+2,t_t1)), variables1,labels1,scope1)
    -- valuto il resto delle espressioni
    let env3@(program3,temp3@(Temp (t_c3,t_t3)),variables3,labels3,scope3) = (getStep  env2 a1 rest)
    -- sommo 
    ([BinOp   BOpPlus (Temp (t_c3+1,t_t3)) temp3 temp2] ++ program3, (Temp (t_c3+1,t_t3)), variables3,labels3,scope3)
  
  for_bound_identifier :: Env -> ForId -> Expr
  for_bound_identifier env for_id =
    case for_id of
      ForInteger val -> ExprInt val
      ForIdent   id  -> ExprLeft (LExprId id)

      