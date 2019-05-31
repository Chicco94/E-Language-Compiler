module ThreeAddressCode where

  --import Control.Monad.State
  
  import AbsE
  import PrintE
  
  import Prelude hiding (lookup)
  import qualified Data.Map as Map
  
  -- Environment = (Program, tempValue, variables, Scope)
  type Env          = ([TAC], Temp_count, VariablesMap, LabelsCount, Scope)
  type Temp_count   = Int
  type VariablesMap = Map.Map String Var         -- Variables context: String -> Var
  type LabelsCount  = Int
  type Scope        = Int
  
  
   -- Initialize the tacprogram with an empty list.
  generateTAC :: [Program] -> Env
  generateTAC progs = generateTAC_int ([],0, Map.empty,0,0) (PDefs (initTAC progs))
  
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
   
  
                            
  -- add a new variable to the env
  addVar :: Env -> Var -> (Env,Var)
  addVar env@(program, temp_count, variables,labels,scope) var@(Var (name,pos,type_)) = ((program, temp_count, (Map.insert name var variables), labels,scope), var)
  findVar :: Env -> Var -> (Env,Var)
  findVar env@(program, temp_count, variables,labels,scope) var@(Var (name,_,_)) = 
    case Map.lookup name variables of
      -- the variable is new
      Nothing     -> (addVar env var)
      -- the variable has already been declared
      Just old_var -> (env, old_var)
  
  
  
  addTACList :: Env -> [TAC] -> Env
  addTACList env [] = env
  addTACList env@(program, temp_count, variables,labels,scope) (tac:rest) = addTACList (addTAC env tac) rest
  addTAC :: Env -> TAC -> Env
  addTAC env@(program, temp_count, variables,labels,scope) tac =
    case tac of
      Return (Temp (_,type_))     -> ([Return (Temp (temp_count-1,type_))]++program         , temp_count    , variables, labels  ,scope  )
      Lbl (Label ("end_stmt",_) ) -> ([Lbl (Label ("end_stmt",scope-1) ) ]++program         , temp_count    , variables, labels+1,scope-1)
      Lbl lbl                     -> ([Lbl lbl                           ]++program         , temp_count    , variables, labels+1,scope  )
      IfFalse tac lbl             -> ([IfFalse (head program) lbl        ]++(drop 1 program), temp_count    , variables, labels  ,scope  )
      If      tac lbl             -> ([If      (head program) lbl        ]++(drop 1 program), temp_count    , variables, labels  ,scope  )
      AssignV2T temp var step     -> ([AssignV2T   temp var step         ]++program         , (temp_count+1), variables, labels  ,scope  )
      _                           -> ([tac                               ]++program         , temp_count    , variables, labels  ,scope  )
  
      
  
  generateStmt :: Env  -> Type -> Stmt -> Env 
  generateStmt env@(program, temp_count, variables,labels,scope) type_ stmt = 
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
          (ExprArray  expr)                         -> (generateAssign env1 (getGuardType guard) id OpAssign (complex2SimpleExpr expr) 0)
          -- effettivo assegnamento con espressione complessa a destra
          (ExprSimple expr_int)                     -> (generateAssign env1 (getGuardType guard) id OpAssign [expr_int] (-1))
      
      -- constant inizialization or assignement
      StmtDefInit id guard expr                     -> generateStmt env type_ (StmtVarInit id guard expr) 
      
      -- return stmt
      StmtReturn (PReturn (pos,name)) expr          -> (addTACList (generateExpr env type_ expr)  [Return (Temp (0,type_))]) -- ritorno l'ultima variabile temporanea instanziata
      
      -- compound statement
      SComp (StmtBlock decls)                       -> generateTAC_int env (PDefs decls) 
  
      -- if then else
      StmtIfThenElse bexpr stmtsT stmtsF            -> addTACList (generateStmt (addTACList (generateStmt (addTACList (generateExpr env type_ bexpr) [IfFalse undefined (Label ("if_false",labels) )]) type_ (SComp stmtsT)) [Goto (Label ("end_if",labels) ),Lbl (Label ("if_false",labels) )]) type_ (SComp stmtsF)) [Lbl (Label ("end_if",labels) )]
      
      -- if then
      StmtIfThen bexpr stmts -> addTACList (generateStmt (addTACList (generateExpr env (TypeBasicType TypeBool) bexpr) [IfFalse undefined (Label ("end_if", labels))]) type_ (SComp stmts) ) [Lbl (Label ("end_if", labels) )]
      
      -- switch case del default prendo solo il primo
      StmtSwitchCase expr norm_cases ((CaseDefault dflt_stms):_) -> do
        let (program4, temp_count4, variables4, labels4, scope4) = addTACList (addTACList (generateStmt (addTACList (generateCases ((addTACList env [Goto (Label ("case_conditions",labels) )])) type_ norm_cases (Label ("end_case",labels) )) [Lbl (Label ("match_dflt", labels) )]) type_ (SComp dflt_stms)) [Goto (Label ("end_case",scope) )]) [Lbl (Label ("case_conditions", labels) )]
        addTACList (addTACList (generateCasesCond (program4, temp_count4, variables4, labels,scope) expr type_ norm_cases) [Goto (Label ("match_dflt",labels) )]) [Lbl (Label ("end_case", labels) )]
      
      -- break stmt
      StmtBreak break -> addTACList env [Goto (Label ("end_stmt",scope-1) )]
      
      -- continue stmt
      StmtContinue continue -> addTACList env [Goto (Label ("guard",scope-1) )] 
  
       -- while stmt    
      StmtWhile bexpr (StmtBlock decls) -> (addTACList (generateExpr (addTACList (generateTAC_int ([Lbl (Label ("body",scope)),Goto (Label ("gaurd",scope))]++program, temp_count,variables,labels,scope+1) (PDefs decls)) [Lbl (Label ("guard",scope))]) (TypeBasicType TypeBool) bexpr) [If undefined (Label ("body",scope)),Lbl (Label ("end_stmt",scope))])
      
      -- for stmt
      StmtFor id@(PIdent (pos,name)) (ExprRange start_for end_for) (StmtBlock decls) -> do
        let (env1, var) = findVar env (Var (name,pos,(TypeBasicType TypeInt)))
        let (program1, temp_count1, variables1, labels1, scope1) = generateAssign env1 (TypeBasicType TypeInt) id OpAssign [(for_bound_identifier env start_for)] (-1)
        (addTACList (generateExpr (addTACList (generateExpr (generateTAC_int ([Lbl (Label ("body",scope)),Goto (Label ("gaurd",scope))]++program1, temp_count1,variables1,labels1,scope1+1) (PDefs decls)) (TypeBasicType TypeInt) (ExprAssign (LExprId id) OpAssign (ExprPlus (ExprLeft (LExprId id)) (ExprInt (PInteger ((0,0),"1")))))) [Lbl (Label ("guard",scope))]) (TypeBasicType TypeBool) (ExprLt (ExprLeft (LExprId id)) (for_bound_identifier env end_for))) [If undefined (Label ("body",scope)),Lbl (Label ("end_stmt",scope))])
  
  
  for_bound_identifier :: Env -> ForId -> Expr
  for_bound_identifier env for_id =
    case for_id of
      ForInteger val -> ExprInt val
      ForIdent   id  -> ExprLeft (LExprId id)
  
  
  generateCases :: Env -> Type -> [NormCase] -> Label -> Env
  generateCases env _ [] _ = env 
  generateCases env@(program, temp_count, variables,labels,scope) type_ ((CaseNormal _ stmts):rest) end_label = 
    generateCases ( addTACList (generateStmt (addTACList env [Lbl (Label ("match_", labels) )]) type_ (SComp stmts)) [Goto end_label] ) type_ rest end_label
  
  generateCasesCond :: Env -> Expr -> Type -> [NormCase] -> Env
  generateCasesCond env _ _ [] = env 
  generateCasesCond env@(program, temp_count, variables, labels, scope) expr_v type_ ((CaseNormal expr _):rest) = do
    let (program, temp_count, variables, labels, scope) = (booleanExpr (generateExpr (generateExpr env type_ expr_v) type_ expr) type_ BOpEq) 
    generateCasesCond ([If (head program) (Label ("match_", labels)) ]++(drop 1 program), temp_count, variables, labels+1, scope) expr_v type_ rest 
    
  
  generateExpr :: Env -> Type -> Expr -> Env
  generateExpr env@(program, temp_count, variables,labels,scope) type_ expr = do
    case expr of
        -- assign
        ExprAssign   (LExprId id) op re                                -> generateAssign env type_ id op [re] (-1)
        ExprAssign   (LExprArr (LArrExpr id (ArrSing step ))) op re    -> generateAssign env type_ id op [re] ((pInt2Int step)*(sizeOf type_))
        --ExprAssign   (LExprArr (LArrExpr id (ArrMul a step))) op re    -> generateAssign env type_ id op re ((pInt2Int step)*(sizeOf type_))
        -- variable inside expression ExprLeft ( LExprId PIdent | LExprRef Ref | LExprArr LArrExpr id (ArrSing PInteger | ArrMul arr PInteger))
        ExprLeft     (LExprId id@(PIdent (pos,name)))                        -> (addTACList env1 [AssignV2T   (Temp (temp_count,type_v)) var (-1)])
          where (env1@(_, _, new_variables,_,_), var@(Var (_,_,type_v))) = findVar env (Var (name,pos,undefined))
        ExprLeft (LExprArr (LArrExpr id@(PIdent (pos,name)) (ArrSing step))) -> (addTACList env1 [AssignV2T   (Temp (temp_count,type_v)) var ((pInt2Int step)*(sizeOf type_))]) 
          where (env1@(_, _, new_variables,_,_), var@(Var (_,_,type_v))) = findVar env (Var (name,pos,undefined))
            
        ExprInt      val         -> ([AssignIntTemp   (Temp (temp_count,(TypeBasicType TypeInt)   )) val] ++ program, (temp_count+1), variables, labels, scope)
        ExprChar     val         -> ([AssignChrTemp   (Temp (temp_count,(TypeBasicType TypeChar)  )) val] ++ program, (temp_count+1), variables, labels, scope)
        ExprString   val         -> ([AssignStrTemp   (Temp (temp_count,(TypeBasicType TypeString))) val] ++ program, (temp_count+1), variables, labels, scope)
        ExprFloat    val         -> ([AssignFloatTemp (Temp (temp_count,(TypeBasicType TypeFloat) )) val] ++ program, (temp_count+1), variables, labels, scope)
        ExprTrue     val         -> ([AssignTrueTemp  (Temp (temp_count,(TypeBasicType TypeBool)  )) val] ++ program, (temp_count+1), variables, labels, scope)
        ExprFalse    val         -> ([AssignFalseTemp (Temp (temp_count,(TypeBasicType TypeBool)  )) val] ++ program, (temp_count+1), variables, labels, scope)
    
        ExprFunCall  fun params  -> (generateCallFunc env fun params type_) 

        -- unary
        ExprBoolNot   expr       -> unaryExpr (generateExpr env type_ expr) type_ UOpNegate
        ExprNegation  expr       -> unaryExpr (generateExpr env type_ expr) type_ UOpMinus
        ExprAddition  expr       -> unaryExpr (generateExpr env type_ expr) type_ UOpPlus

        -- deref
        --ExprReference (LExprId id@(PIdent (pos,name)))                            -> unaryExpr (generateExpr env type_ expr) type_ UOpDeref
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
          let (program1, temp_count1, variables1, labels1,_) = (generateExpr env type_ expr1)
          let (program2, temp_count2, variables2, labels2,_) = (generateExpr ([If      (head program1) (Label ("true_or"  , labels1) )]++(drop 1 program1),temp_count1, variables1, labels1+1,scope) type_ expr2)
          ([(BoolOp BOpEq (Temp (temp_count2,(TypeBasicType TypeBool))) (TempT (PTrue ((0,0),"true")))), Lbl (Label ("end_or" ,labels1)),AssignTrueTemp  (Temp (temp_count2,(TypeBasicType TypeBool))) (PTrue ((0,0),"true")),  Lbl (Label ("true_or",  labels1) ),Goto (Label ("end_or", labels1) ),AssignFalseTemp (Temp (temp_count2,(TypeBasicType TypeBool))) (PFalse ((0,0),"false")), If      (head program2) (Label ("true_or"  , labels1) )]++(drop 1 program2),temp_count2+1, variables2, labels2+1,scope)
        ExprAnd      expr1 expr2 -> do --binaryExpr (generateExpr (generateExpr env type_ expr1) type_ expr2) type_ BOpAnd
          let (program1, temp_count1, variables1, labels1,_) = (generateExpr env type_ expr1)
          let (program2, temp_count2, variables2, labels2,_) = (generateExpr ([IfFalse (head program1) (Label ("false_and", labels1) )]++(drop 1 program1),temp_count1, variables1, labels1+1,scope) type_ expr2)
          ([(BoolOp BOpEq (Temp (temp_count2,(TypeBasicType TypeBool))) (TempT (PTrue ((0,0),"true")))), Lbl (Label ("end_and",labels1)),AssignFalseTemp (Temp (temp_count2,(TypeBasicType TypeBool))) (PFalse ((0,0),"false")),Lbl (Label ("false_and",labels1) ),Goto (Label ("end_and",labels1) ),AssignTrueTemp  (Temp (temp_count2,(TypeBasicType TypeBool))) (PTrue ((0,0),"true")),   IfFalse (head program2) (Label ("false_and", labels1) )]++(drop 1 program2),temp_count2+1, variables2, labels2+1,scope)
          
        _ -> env 

  -- Build the binary operator using the last two temporaneus variable
  binaryExpr :: Env -> Type -> BinaryOperator -> Env
  binaryExpr env@(program, temp_count, variables,labels,scope) type_ op  = ([BinOp   op (Temp (temp_count,type_))   (Temp (temp_count-2,type_)) (Temp (temp_count-1,type_))] ++ program, (temp_count+1), variables,labels,scope)
  
  unaryExpr  :: Env -> Type -> UnaryOperator -> Env
  unaryExpr  env@(program, temp_count, variables,labels,scope) type_ op  = ([UnaryOp op (Temp (temp_count-1,type_)) (Temp (temp_count,type_))  ]++program, (temp_count+1), variables, labels, scope)

  booleanExpr :: Env -> Type -> BinaryOperator -> Env
  booleanExpr env@(program, temp_count, variables,labels,scope) type_ op = ([BoolOp  op (Temp (temp_count-2,type_)) (Temp (temp_count-1,type_))]++program, (temp_count+1), variables, labels, scope)
  
  --StmtAssign LExpr AssignOperator Expr
  generateAssign :: Env -> Type -> PIdent -> AssignOperator -> [Expr] -> Int -> Env
  generateAssign env _ _ _ [] _ = env
  generateAssign env@(program, temp_count, variables,labels,scope) type_ id@(PIdent (pos,name)) op (expr:[]) step = do
    let (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))
    case op of
      OpAssign    -> (addTACList (generateExpr env (type2BasicType type_) expr) [AssignT2V  var (Temp (temp_count,(type2BasicType type_))) (step)])
      _           -> generateAssign (binaryExpr (addTACList (generateExpr env (type2BasicType type_) expr) [AssignV2T (Temp (temp_count,(type2BasicType type_))) var (step)]) type_ op1) type_ id OpAssign undefined step
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
  generateAssign env@(program, temp_count, variables,labels,scope) type_ id@(PIdent (pos,name)) op (expr:rest) step = 
    (generateAssign (addTACList (generateExpr env (type2BasicType type_) expr) [AssignT2V  var (Temp (temp_count,(type2BasicType type_))) step]) type_ id op rest (step+(sizeOf type_)))
      where (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))


  -- DeclFun LExpr [Arg] Guard CompStmt
  generateDeclFunc :: Env -> LExpr -> [Arg] -> Guard -> CompStmt -> Env
  generateDeclFunc env@(program, temp_count, variables,labels,scope) lexpr@(LExprId (PIdent (pos, name))) args guard stmt@(StmtBlock decls) = do
    let (env1, var) = findVar env (Var (name,pos,(getGuardType guard)))
    (generateTAC_int (addTACList env1 [FuncDef var]) (PDefs decls))
  
  -- use temp variables as parameters
  generateCallFunc :: Env -> PIdent -> [Expr] -> Type -> Env
  generateCallFunc  env@(program, temp_count, variables,labels,scope) (PIdent (pos, name)) params type_ = do
    let (_, var)    = findVar env (Var (name,pos,type_)) -- prendo il tipo della funzione
    (addTACList (generateParams env params) [FuncCall var (Temp (temp_count,type_))])
  
  generateParams :: Env -> [Expr] -> Env
  generateParams env [] = env
  generateParams env@(program, temp_count, variables,labels,scope) (param:params) = generateParams (addTACList (generateExpr env (TypeBasicType TypeVoid) param) [AssignT2P (Temp (temp_count,(TypeBasicType TypeVoid)))]) params
  

  -- --------------------------------------------------------------------------

  --                              USEFUL FUNCTIONS
  
  -- --------------------------------------------------------------------------
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

  sizeOf :: Type -> Int
  sizeOf type_ = case (type2BasicType type_) of 
                  (TypeBasicType TypeBool  ) -> 1 -- vado a botte di parole
                  (TypeBasicType TypeFloat ) -> 4
                  (TypeBasicType TypeInt   ) -> 4
                  (TypeBasicType TypeChar  ) -> 1

  pInt2Int :: PInteger -> Int
  pInt2Int (PInteger (pos,value)) = read value :: Int