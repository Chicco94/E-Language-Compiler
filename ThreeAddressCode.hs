module ThreeAddressCode where

  -- per il resto del codice con "genera" si intende
  -- crea i costrutti per l'istruzione TAC associata

  import AbsE
  import AbsETAC
  import PrintETAC
  
  import Prelude hiding (lookup)
  import qualified Data.Map as Map
  
  -- envirornment che viene aggiornato ad ogni istruzione 
  type Env          = (TACProg, LastTemp, VariablesMap, LabelsCount, Scope)
  type TACProg      = [TAC]
  type LastTemp     = Temp                       -- ultimo temporaneo calcolato
  type VariablesMap = Map.Map String Var         -- Variables context: String -> Var
  type LabelsCount  = Int
  type Scope        = (Int,Int) -- scope globale, scope interno/ differenzio le istruzioni all'interno del mio stesso scope
  
  
   -- funzione che prende l'output del typechecker e ritorna il programma TAC
  generateTAC :: [Program] -> Env
  generateTAC progs = (generateTAC' ([],(Temp (-1,(TypeBasicType TypeVoid))), Map.empty,0,(0,0)) (PDefs (initTAC progs)))
  
  -- appiana il programma per poterlo leggerlo più facilmente
  initTAC :: [Program] -> [Decl]
  initTAC [] = []
  initTAC (prog@(PDefs defs):progs) = defs ++ initTAC progs 
  
  postAttach :: a -> [a] -> [a]
  postAttach a [] = [a]
  postAttach a (x:xs) = x : postAttach a xs

  -- usata per generare tutto il programma istruzione per istruzione
  generateTAC' :: Env -> Program -> Env
  generateTAC' env prog@(PDefs decls) = do
    let (final_env, rest) = foldl generateInstruction (env,[[]]) decls
    let final_env_with_main@(p,t,v,l,s) = (ifmain final_env)
    ( (concat (postAttach p rest)) ,t,v,l,s)

  -- le istruzioni non tipate gestico mettendole a void
  generateInstruction :: (Env,[TACProg]) -> Decl -> (Env,[TACProg])
  generateInstruction env decl = generateDecl env Nothing decl
  

  -- genera la dichiarazione corrispondente
  -- le funzioni le mette a parte per posizionarle in fondo al TAC
  generateDecl :: (Env,[TACProg]) -> Maybe Type -> Decl -> (Env,[TACProg]) 
  generateDecl (env@(p,t,v,l,s),rest) maybe_type decl =
    case decl of
      TypedDecl (ADecl type_ decl1) -> (generateDecl    (env,rest) (Just type_) decl1)
      DeclFun   id args type_ stmts -> do
        let env_temp@(p',t',v',l',s') =  (generateDeclFunc ([],t,v,l,s) id args type_ stmts)
        ((p,t',v',l',s'),(([EndFunction]++p'):rest))
      DeclStmt (stmt)               -> (generateStmt     env new_type stmt,rest)
        where new_type = case maybe_type of 
                            Nothing      -> (TypeBasicType TypeVoid)
                            (Just type_) -> type_
   
  
  -- genera lo statement corrispondente, già tipato dal typechecker (se necessario)
  generateStmt :: Env  -> Type -> Stmt -> Env 
  generateStmt env@(program, last_temp, variables,labels,s@(scope,s_i)) type_ stmt = 
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
      StmtReturn (PReturn (pos,name)) expr          -> (addTACList (generateExpr env type_ expr)  [Return last_temp]) -- ritorno l'ultima variabile temporanea instanziata
      
      -- compound statement
      SComp (StmtBlock decls)                       -> generateSubTAC env (PDefs decls) 
  
      -- if then else
      --     ifFales boolExpr goto lbl
      --     stmtsTrue
      --     goto end
      -- lbl stmtsFalse
      --     goto end
      -- end
      StmtIfThenElse bexpr stmtsT stmtsF            -> addTACList (generateStmt (addTACList (generateStmt (addTACList (generateExpr env (TypeBasicType TypeBool) bexpr) [IfFalse Empty (Label ("if_false",labels) )]) type_ (SComp stmtsT)) [Goto (Label ("end_if",labels) ),Lbl (Label ("if_false",labels) )]) type_ (SComp stmtsF)) [Lbl (Label ("end_if",labels) )]
      
      -- if then
      --     if boolExpr goto end
      --     stmtsTrue
      -- end 
      StmtIfThen bexpr stmts -> addTACList (generateStmt (addTACList (generateExpr env (TypeBasicType TypeBool) bexpr) [IfFalse Empty (Label ("end_if", labels))]) type_ (SComp stmts) ) [Lbl (Label ("end_if", labels) )]
      
      -- switch case del default prendo solo il primo
      --      goto grd
      -- lbl1 stmtsCase1
      --      goto end
      -- ...
      -- lbln stmtsCasen
      --      goto end
      -- lbld stmtsDefault
      --      goto end
      -- grd if boolExpr1 goto lbl1
      -- ...
      --     if boolExprn goto lbln
      --     goto lbld
      -- end
      StmtSwitchCase expr norm_cases ((CaseDefault dflt_stms):_) -> do
        let (program4, last_temp4, variables4, labels4, scope4) = addTACList (addTACList (generateStmt (addTACList (generateCases ((addTACList env [Goto (Label ("case_conditions",labels) )])) type_ norm_cases (Label ("end_case",labels) )) [Lbl (Label ("match_dflt", labels) )]) type_ (SComp dflt_stms)) [Goto (Label ("end_case",labels) )]) [Lbl (Label ("case_conditions", labels) )]
        addTACList (addTACList (generateCasesCond (program4, last_temp4, variables4, labels,scope4) expr type_ norm_cases) [Goto (Label ("match_dflt",labels) )]) [Lbl (Label ("end_case", labels) )]
      
      -- break stmt
      -- goto end(-1)
      StmtBreak break -> addTACList env [Goto (Label ("end_stmt",scope+s_i-1))]
      
      -- continue stmt
      -- goto guard(-1)
      StmtContinue continue -> addTACList env [Goto (Label ("guard",scope+s_i-1))] 
  
      -- while stmt
      --     goto grd
      -- lbl stmts
      -- grd if boolexpr goto lbl
      -- end
      StmtWhile bexpr (StmtBlock decls) -> (addTACList (generateExpr (addTACList (generateSubTAC ([Lbl (Label ("body",scope+s_i+1)),Goto (Label ("guard",scope+s_i+1))]++program, last_temp,variables,labels,(scope+1,s_i+1)) (PDefs decls)) [Lbl (Label ("guard",scope+s_i+1))]) (TypeBasicType TypeBool) bexpr) [If Empty (Label ("body",scope+s_i+1)),Lbl (Label ("end_stmt",scope+s_i+1))])
      
      -- for stmt
      --     i=start
      --     goto grd
      -- lbl stmts
      --     i+1
      -- grd if boolexpr goto lbl
      -- end
      StmtFor id@(PIdent (pos,name)) (ExprRange start_for end_for) (StmtBlock decls) -> do
        let (env1, var) = findVar env (Var (name,pos,(TypeBasicType TypeInt)))
        let (program1, last_temp1, variables1, labels1, _) = generateAssign env1 (TypeBasicType TypeInt) id OpAssign [(for_bound_identifier env start_for)] (Temp (-1,(TypeBasicType TypeInt)))
        (addTACList (generateExpr (addTACList (generateExpr (generateSubTAC ([Lbl (Label ("body",scope+s_i+1)),Goto (Label ("guard",scope+s_i+1))]++program1, last_temp1,variables1,labels1,(scope +1,s_i+1)) (PDefs decls)) (TypeBasicType TypeInt) (ExprAssign (LExprId id) OpAssign (ExprPlus (ExprLeft (LExprId id)) (ExprInt (PInteger ((0,0),"1")))))) [Lbl (Label ("guard",scope+s_i+1))]) (TypeBasicType TypeBool) (ExprLt (ExprLeft (LExprId id)) (for_bound_identifier env end_for))) [If Empty (Label ("body",scope+s_i+1)),Lbl (Label ("end_stmt",scope+s_i+1))])
  

  -- genera le itruzioni di ogni caso dello switch
  generateCases :: Env -> Type -> [NormCase] -> Label -> Env
  generateCases env _ [] _ = env 
  generateCases env@(program, last_temp, variables,labels,scope) type_ ((CaseNormal _ stmts):rest) end_label = 
    generateCases ( addTACList (generateStmt (addTACList env [Lbl (Label ("match_", labels) )]) type_ (SComp stmts)) [Goto end_label] ) type_ rest end_label
  
  -- genera le condizioni per ogni caso dello switch
  generateCasesCond :: Env -> Expr -> Type -> [NormCase] -> Env
  generateCasesCond env _ _ [] = env 
  generateCasesCond env@(p, last_temp, variables, labels, scope) expr_v type_ ((CaseNormal expr _):rest) = do
    let env1@(_,r1,_,_,_) = (generateExpr env type_ expr_v)
    let (program, last_temp, variables, labels, scope) = (booleanExpr (generateExpr env1 type_ expr) type_ BOpEq r1) 
    generateCasesCond ([If (head program) (Label ("match_", labels)) ]++(drop 1 program), last_temp, variables, labels+1, scope) expr_v type_ rest 
    

  -- genera l'espressione corrispondente
  generateExpr :: Env -> Type -> Expr -> Env
  generateExpr env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) type_ expr = do
    case expr of
        -- assign val to variable
        ExprAssign   (LExprId id) op re                                -> generateAssign env type_ id op [re] (Temp (-1,(TypeBasicType TypeInt)))
        -- assign val to array
        ExprAssign   (LExprArr (LArrExpr id@(PIdent (pos,name)) (ArrSing expr_step ))) op re    -> do
          let env2@(_,temp1,_,_,_) = generateExpr env1 (TypeBasicType TypeInt) (ExprMul expr_step (ExprInt (int2PInt (sizeOf type_v)))) -- (step)*(sizeOf type_v)
          generateAssign env2 type_ id op [re] temp1 
            where (env1, var@(Var (_,_,type_v))) = findVar env (Var (name,pos,type_))
        -- assign val to multiarray
        ExprAssign   (LExprArr (LArrExpr id@(PIdent (pos,name)) a)) op re    -> do
          let env2@(_,temp1@(Temp (t_c1,t_t1)),_,_,_) = (getStep env1 a  (reverse (type2Dims type_v)))
          let env3@(_,temp2@(Temp (t_c2,t_t2)),_,_,_) = (addTACList (addTACList (env2) [AssignIntTemp (Temp (t_c1+1,(TypeBasicType TypeInt))) (int2PInt (sizeOf type_v))]) [BinOp   BOpMul (Temp (t_c1+2,t_t1)) (Temp (t_c1+1,(TypeBasicType TypeInt))) temp1]) -- (step)*(sizeOf type_v)
          generateAssign env3 type_ id op [re] temp2 
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
          let env2@(_,temp1@(Temp (t_c1,t_t1)),_,_,_) = (getStep env1 a  (reverse (type2Dims type_v)))
          let env3@(_,temp2@(Temp (t_c2,t_t2)),_,_,_) = (addTACList (addTACList (env2) [AssignIntTemp (Temp (t_c1+1,(TypeBasicType TypeInt))) (int2PInt (sizeOf type_v))]) [BinOp   BOpMul (Temp (t_c1+2,t_t1)) (Temp (t_c1+1,(TypeBasicType TypeInt))) temp1])
          (addTACList env3 [AssignV2T (Temp (t_c2+1,t_t2)) var temp2]) 
            where (env1, var@(Var (_,_,type_v))) = findVar env (Var (name,pos,type_))
        
        -- assegnamento di valore a temporaneo
        ExprInt      val         -> (addTACList env [AssignIntTemp   (Temp (t_c+1,(TypeBasicType TypeInt)   )) val])
        ExprChar     val         -> (addTACList env [AssignChrTemp   (Temp (t_c+1,(TypeBasicType TypeChar)  )) val])
        ExprString   val         -> (addTACList env [AssignStrTemp   (Temp (t_c+1,(TypeBasicType TypeString))) val])
        ExprFloat    val         -> (addTACList env [AssignFloatTemp (Temp (t_c+1,(TypeBasicType TypeFloat) )) val])
        ExprTrue     val         -> (addTACList env [AssignTrueTemp  (Temp (t_c+1,(TypeBasicType TypeBool)  )) val])
        ExprFalse    val         -> (addTACList env [AssignFalseTemp (Temp (t_c+1,(TypeBasicType TypeBool)  )) val])
    
        -- chiamata di funzione
        ExprFunCall  fun params  -> (generateCallFunc env fun params type_) 

        -- not
        ExprBoolNot   expr       -> notExpr (generateExpr env type_ expr)

        -- unary operator (se -int 
        ExprNegation (ExprInt   val) -> (addTACList env [AssignIntTemp    (Temp (t_c+1,(TypeBasicType TypeInt)   )) (int2PInt (-(pInt2Int val)))])
        ExprNegation (ExprFloat val) -> (addTACList env [AssignFloatTemp  (Temp (t_c+1,(TypeBasicType TypeFloat) )) (float2PFloat (-(pFloat2Float val)))])
        ExprNegation  expr       -> unaryExpr (generateExpr env type_ expr) type_ UOpMinus
        ExprAddition  expr       -> unaryExpr (generateExpr env type_ expr) type_ UOpPlus

        -- deref
        ExprReference (LExprId id@(PIdent (pos,name))) -> (addTACList env1 [DerefOp UOpDeref (Temp (t_c+1,(getPointerFromType type_v)))  var ])
          where (env1@(_, _, new_variables,_,_), var@(Var (_,_,type_v))) = findVar env (Var (name,pos,undefined))
        
        -- binary operations
        -- arithmetic
        (ExprPower expr1 expr2) -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          binaryExpr  (generateExpr env1 type_ expr2) type_ BOpPower     r1 
        ExprMul      expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          binaryExpr  (generateExpr env1 type_ expr2) type_ BOpMul       r1
        ExprFloatDiv expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          binaryExpr  (generateExpr env1 type_ expr2) type_ BOpFloatDiv  r1
        ExprModulo   expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          binaryExpr  (generateExpr env1 type_ expr2) type_ BOpModulo    r1
        ExprIntDiv   expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          binaryExpr  (generateExpr env1 type_ expr2) type_ BOpIntDiv    r1
        ExprPlus     expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          binaryExpr  (generateExpr env1 type_ expr2) type_ BOpPlus      r1 
        ExprReminder expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          binaryExpr  (generateExpr env1 type_ expr2) type_ BOpRemainder r1
        ExprMinus    expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          binaryExpr  (generateExpr env1 type_ expr2) type_ BOpMinus     r1  

        -- boolean
        ExprLt       expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          booleanExpr (generateExpr env1 type_ expr2) type_ BOpLt        r1 
        ExprGt       expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          booleanExpr (generateExpr env1 type_ expr2) type_ BOpGt        r1 
        ExprLtEq     expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          booleanExpr (generateExpr env1 type_ expr2) type_ BOpLtEq      r1 
        ExprGtEq     expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          booleanExpr (generateExpr env1 type_ expr2) type_ BOpGtEq      r1 
        ExprEq       expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          booleanExpr (generateExpr env1 type_ expr2) type_ BOpEq        r1
        ExprNeq      expr1 expr2 -> do
          let env1@(_,r1,_,_,_) = (generateExpr env type_ expr1)
          booleanExpr (generateExpr env1 type_ expr2) type_ BOpNeq       r1 

        -- con shortcut
        ExprOr       expr1 expr2 -> do
          let env1@(program1, last_temp1, variables1, labels1,_) = (generateExpr env type_ expr1)
          let (program2, last_temp2@(Temp (t_c,t_t)), variables2, labels2,_) = (generateExpr (addTACList env1 [If Empty (Label ("true_or", labels1))]) type_ expr2)
          ([Lbl (Label ("end_or", labels1)),AssignTrueTemp  (Temp (t_c+1,(TypeBasicType TypeBool))) (PTrue ((0,0),"true")), Lbl (Label ("true_or", labels1)), Goto (Label ("end_or", labels1)), AssignFalseTemp (Temp (t_c+1,(TypeBasicType TypeBool))) (PFalse ((0,0),"false")), If (BoolOp BOpEq last_temp2 (TempT (PTrue ((0,0),"true")))) (Label ("true_or", labels1))]++program2, (Temp (t_c+1,(TypeBasicType TypeBool))), variables2, labels2+1,scope)
        ExprAnd      expr1 expr2 -> do 
          let env1@(program1, last_temp1, variables1, labels1,_) = (generateExpr env type_ expr1)
          let (program2, last_temp2@(Temp (t_c,t_t)), variables2, labels2,_) = (generateExpr (addTACList env1 [If Empty (Label ("false_and", labels1))]) type_ expr2)
          ([Lbl (Label ("end_and",labels1)),AssignFalseTemp (Temp (t_c+1,(TypeBasicType TypeBool))) (PFalse ((0,0),"false")), Lbl (Label ("false_and",labels1) ),Goto (Label ("end_and",labels1) ), AssignTrueTemp  (Temp (t_c+1,(TypeBasicType TypeBool))) (PTrue ((0,0),"true")), If (BoolOp BOpEq last_temp2 (TempF (PFalse ((0,0),"false")))) (Label ("false_and", labels1) )]++program2,(Temp (t_c+1,(TypeBasicType TypeBool))), variables2, labels2+1,scope)       

  -- genera ogni espressione binaria usando l'utlima variabile temporanea
  -- e una passata come parametro
  binaryExpr :: Env -> Type -> BinaryOperator -> Temp-> Env
  binaryExpr env@(_, last_temp@(Temp (t_c,t_t)), _,_,_) type_ op t1 = (addTACList env [BinOp   op (Temp (t_c+1,t_t)) t1 last_temp])

  -- genera ogni espressione unaria usando l'ultima variabile temporanea
  -- passata come parametro
  unaryExpr  :: Env -> Type -> UnaryOperator -> Env
  unaryExpr  env@(program, last_temp@(Temp (t_c,t_t)),variables,labels,scope) type_ op  = ([UnaryOp op (Temp (t_c+1,t_t)) last_temp]++program, (Temp (t_c+1,t_t)), variables, labels, scope)

  -- genera ogni espressione booleana binaria usando l'utlima variabile temporanea
  -- e una passata come parametro
  booleanExpr :: Env -> Type -> BinaryOperator -> Temp -> Env
  booleanExpr env@(program, last_temp@(Temp (t_c,t_t)),variables,labels,scope) type_ op t1 = ([BoolOp  op t1 last_temp]++program, last_temp, variables, labels, scope)

  -- prende l'ultima espressione calcolata (assumo sia booleana) e la inverto
  -- se non è binaria semplicemente ci metto un not davanti
  notExpr :: Env -> Env
  notExpr env@(p, last_temp, variables,labels,scope) =
    case (head p) of
      BoolOp  BOpLt   t1 t2 -> (addTACList env [BoolOp BOpGtEq t1 t2]) 
      BoolOp  BOpGt   t1 t2 -> (addTACList env [BoolOp BOpLtEq t1 t2])
      BoolOp  BOpLtEq t1 t2 -> (addTACList env [BoolOp BOpGt   t1 t2])
      BoolOp  BOpGtEq t1 t2 -> (addTACList env [BoolOp BOpLt   t1 t2])
      BoolOp  BOpEq   t1 t2 -> (addTACList env [BoolOp BOpNeq  t1 t2])
      BoolOp  BOpNeq  t1 t2 -> (addTACList env [BoolOp BOpEq   t1 t2])
      _                     -> (unaryExpr env (TypeBasicType TypeBool) UOpNegate)

  -- genera ogni possibile assegnamanto
  -- generateAssign (generateExpr env2 type_ expr) type_ id op [re] step_temp 
  generateAssign :: Env -> Type -> PIdent -> AssignOperator -> [Expr] -> Temp -> Env
  generateAssign env _ _ _ [] _ = env
  generateAssign env@(program, last_temp, variables,labels,scope) type_ id@(PIdent (pos,name)) op [expr] step = do
    let (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))
    case op of
      OpAssign    -> (addTACList (generateExpr env (type2BasicType type_) expr) [AssignT2V  var undefined (step)])
      _           -> do -- se sono arrivato qui devo aggiornare l'espressione a sinistra con il valore a destra
                    let env2@(_,r2@(Temp (t_c2,t_t2)),_,_,_) = (generateExpr env (type2BasicType type_) expr)
                    let env3@(p,r3@(Temp (t_c3,t_t3)),v,l,s) = addTACList env2 [AssignV2T (Temp (t_c2+1,(type2BasicType type_))) var (step)]
                    generateAssign (addTACList env3 [BinOp   op1 (Temp (t_c3+1,t_t3)) r3 r2]) type_ id OpAssign [] step
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
  generateAssign env type_ id op list@(expr:rest:rests) _ = env --generateArray env type_ id op list 0

  -- genera l'inizializzazione di un array
  generateArray :: Env -> Type -> PIdent -> AssignOperator -> [Expr] -> Int -> Env
  generateArray env@(program, last_temp@(Temp (t_c,t_t)), variables,labels,scope) type_ id@(PIdent (pos,name)) op (expr:[]) step = (addTACList (generateExpr env (type2BasicType type_) expr) [AssignT2V  var last_temp (Temp (step,(TypeBasicType TypeVoid)))])
    where (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))
  generateArray env@(program, last_temp, variables,labels,scope) type_ id@(PIdent (pos,name)) op (expr:rest) step = (generateArray (addTACList (generateExpr env (type2BasicType type_) expr) [AssignT2V  var last_temp (Temp (step,(TypeBasicType TypeVoid)))]) type_ id op rest (step+(sizeOf type_)))
    where (env1@(_, _, new_variables,_,_), var) = findVar env (Var (name,pos,type_))

  -- genera la dichiarazione di una funzione
  generateDeclFunc :: Env -> LExpr -> [Arg] -> Guard -> CompStmt -> Env
  generateDeclFunc env@(program, last_temp, variables,labels,scope) lexpr@(LExprId (PIdent (pos, name))) args guard stmt@(StmtBlock decls) = do
    let (env1@(p,t,v,l,(s_g,s_i)), var) = findVar (addArgs env args) (Var (name,pos,(getGuardType guard)))
    (generateSubTAC (addTACList (p,t,v,l,(s_g+1,s_i+1)) [FuncDef var]) (PDefs decls))

  addArgs :: Env -> [Arg] -> Env
  addArgs env [] = env
  addArgs env@(_, _,vars,_,_) (arg@(ArgDecl _ (PIdent id@(pos,name)) guard):args)= (addArgs (fst (addVar env (Var (name,pos,getGuardType guard)))) args)


  -- chiamata di funzione, inserisce i valori negli slot param
  generateCallFunc :: Env -> PIdent -> [Expr] -> Type -> Env
  generateCallFunc  env (PIdent (pos, name)) params type_ = do
    let (_, var)    = findVar env (Var (name,pos,type_)) -- prendo il tipo della funzione
    (addTACList (generateParams env params) [FuncCall var (Temp (0,type_))])
  
  -- data una lista di espressioni ritorna l'assegnamento ai parametri associati
  generateParams :: Env -> [Expr] -> Env
  generateParams env [] = env
  generateParams env (param:params) = generateParams (addTACList (generateExpr env undefined param) [AssignT2P undefined]) params
  


  -- --------------------------------------------------------------------------

  --                              USEFUL FUNCTIONS
  
  -- --------------------------------------------------------------------------
  -- aggiunge una variabile all'environment
  addVar :: Env -> Var -> (Env,Var)
  addVar env@(program, last_temp, variables,labels,scope) var@(Var (name,pos,type_)) = ((program, last_temp, (Map.insert name var variables), labels,scope), var)
  -- cerca una variabile, se non la trova, la aggiunge
  -- (assumo i controlli sull'uso della variabile siano già stati fatti a livello typechecker)
  findVar :: Env -> Var -> (Env,Var)
  findVar env@(_, _, variables,_,_) var@(Var (name,_,_)) = 
    case Map.lookup name variables of
      Nothing     -> (addVar env var)
      Just old_var -> (env, old_var)

  -- aggiunge una lista di istruzioni TAC all'environment
  -- se necessario modifica gli altri parametri
  addTACList :: Env -> [TAC] -> Env
  addTACList env [] = env
  addTACList env (tac:rest) = addTACList (addTAC env tac) rest
  addTAC :: Env -> TAC -> Env
  addTAC env@(p, last_temp@(Temp (t_c,t_t)), variables,labels,scope@(s_g,s_i)) tac =
    case tac of
      Return _                    -> ([Return last_temp                  ]++p, last_temp       , variables, labels  ,(s_g,s_i)  )
      Lbl (Label ("end_stmt",_) ) -> ([Lbl (Label ("end_stmt",s_g+s_i-1))]++p, last_temp       , variables, labels+1,(s_g-1,s_i))
      Lbl lbl                     -> ([Lbl lbl                           ]++p, last_temp       , variables, labels+1,(s_g,s_i)  )
      AssignV2T temp var step     -> ([AssignV2T temp   var      step    ]++p, temp            , variables, labels  ,(s_g,s_i)  )
      AssignT2V var _ step        -> ([AssignT2V var  last_temp  step    ]++p, last_temp       , variables, labels  ,(s_g,s_i)  )
      AssignT2P _                 -> ([AssignT2P last_temp               ]++p, last_temp       , variables, labels  ,(s_g,s_i)  )
      AssignIntTemp    temp val   -> ([AssignIntTemp   temp val          ]++p, temp            , variables, labels  ,(s_g,s_i)  )
      AssignChrTemp    temp val   -> ([AssignChrTemp   temp val          ]++p, temp            , variables, labels  ,(s_g,s_i)  )
      AssignStrTemp    temp val   -> ([AssignStrTemp   temp val          ]++p, temp            , variables, labels  ,(s_g,s_i)  )
      AssignFloatTemp  temp val   -> ([AssignFloatTemp temp val          ]++p, temp            , variables, labels  ,(s_g,s_i)  )
      AssignTrueTemp   temp val   -> ([AssignTrueTemp  temp val          ]++p, temp            , variables, labels  ,(s_g,s_i)  )
      AssignFalseTemp  temp val   -> ([AssignFalseTemp temp val          ]++p, temp            , variables, labels  ,(s_g,s_i)  )
      FuncCall var (Temp (_,t))   -> ([FuncCall  var  (Temp (t_c+1,t))   ]++p, (Temp (t_c+1,t)), variables, labels  ,(s_g,s_i)  )
      EndFunction                 -> ([EndFunction                       ]++p, last_temp       , variables, labels  ,(s_g-1,s_i))
      BinOp   op tres  t1   t2    -> ([BinOp   op     tres    t1   t2    ]++p,tres             , variables, labels  ,(s_g,s_i)  )  
      DerefOp op temp  var        -> ([DerefOp UOpDeref temp  var        ]++p, temp            , variables, labels  ,(s_g,s_i)  )  
      -- l'if si mangia l'ultima istruzione booleana calcolata per ricostruirla all'interno dell'istruzione TAC
      IfFalse _ lbl -> case (head p) of
        (BoolOp _ _ _)  -> ([IfFalse (head p) lbl                                                 ]++(drop 1 p), last_temp, variables, labels  ,(s_g,s_i)   )
        _               -> ([IfFalse (BoolOp BOpEq last_temp (TempT (PTrue ((0,0),"true-")))) lbl ]++p         , last_temp, variables, labels  ,(s_g,s_i)   )
      If      _ lbl -> case (head p) of
        (BoolOp _ _ _)  -> ([If (head p) lbl                                                      ]++(drop 1 p), last_temp, variables, labels  ,(s_g,s_i)   )
        _               -> ([If      (BoolOp BOpEq last_temp (TempT (PTrue ((0,0),"true-")))) lbl ]++p         , last_temp, variables, labels  ,(s_g,s_i)   )
      _                           -> ([tac                               ]++p, last_temp       , variables, labels  ,(s_g,s_i)   )
  
  -- costruisce il codice interno dei vari compoundStmt come while e for
  generateSubTAC :: Env -> Program -> Env
  generateSubTAC env prog@(PDefs decls) = do 
    let ((p,t,v,l,s), rest) = foldl generateInstruction (env,[]) decls
    ( (concat (p:rest)) ,t,v,l,s)

  -- restituisce il tipo di base data una guardia
  getGuardType :: Guard -> Type
  getGuardType GuardVoid       = (TypeBasicType TypeVoid)
  getGuardType (GuardType  t_) = t_

  -- restituisce le dimensioni di un vettore/matrice
  type2Dims :: Type -> [PInteger]
  type2Dims (TypeBasicType _) = []
  type2Dims (TypeCompoundType (CompoundTypeArrayType (ArrDefBase dims _))) = dims
  type2Dims (TypeCompoundType (CompoundTypeArrayType (ArrDefPtr  dims _))) = dims

  -- ritorna il tipo di base di un vettore/matrice/puntatore
  type2BasicType :: Type -> Type
  type2BasicType (TypeBasicType t) = (TypeBasicType t)
  type2BasicType (TypeCompoundType (CompoundTypeArrayType (ArrDefBase _ t))) = (TypeBasicType t)
  type2BasicType (TypeCompoundType (CompoundTypePtr       (Pointer  t)))     = (TypeBasicType t)

  -- trasforma una lista di espressioni complesse (es: espressioni usate per la posizione all'interno di un array)
  -- e restituisce una lista più facile da maneggiare di sole espressioni
  complex2SimpleExpr :: [ComplexExpr] -> [Expr]
  complex2SimpleExpr [] = []
  complex2SimpleExpr ((ExprSimple expr): rest) = expr : (complex2SimpleExpr rest)
  complex2SimpleExpr ((ExprArray  expr): rest) = (complex2SimpleExpr expr)++(complex2SimpleExpr rest)

  -- restituisce il tipo del puntatore data il tipo della variabile puntata
  getPointerFromType :: Type -> Type
  getPointerFromType (TypeBasicType t) = (TypeCompoundType (CompoundTypePtr (Pointer t)))
  getPointerFromType (TypeCompoundType (CompoundTypePtr p)) = (TypeCompoundType (CompoundTypePtr (Pointer2Pointer p)))
  getPointerFromType (TypeCompoundType (CompoundTypeArrayType (ArrDefBase d t))) = (TypeCompoundType (CompoundTypeArrayType (ArrDefPtr d (Pointer t))))
  getPointerFromType (TypeCompoundType (CompoundTypeArrayType (ArrDefPtr d p)))  = (TypeCompoundType (CompoundTypeArrayType (ArrDefPtr d ((Pointer2Pointer p)))))

  -- ritorna la dimensione del tipo per la posizione nell'array
  sizeOf :: Type -> Int
  sizeOf type_ = case (type2BasicType type_) of 
                  (TypeBasicType TypeBool  ) -> 1 -- vado a botte di parole
                  (TypeBasicType TypeFloat ) -> 4
                  (TypeBasicType TypeInt   ) -> 4
                  (TypeBasicType TypeChar  ) -> 1
                  (TypeBasicType TypeString) -> 4 -- Se stringa metto lo spazio per un puntatore a stringa

  -- da PInteger a Int e viceversa (usata soprattutto per gli accessi agli array)
  pInt2Int :: PInteger -> Int
  pInt2Int (PInteger (pos,value)) = read value :: Int
  int2PInt :: Int -> PInteger
  int2PInt value = (PInteger ((0,0),show value))

-- da PFloat a Float e viceversa (usata soprattutto per gli accessi agli array)
  pFloat2Float :: PFloat -> Float
  pFloat2Float (PFloat (pos,value)) = read value :: Float
  float2PFloat :: Float -> PFloat
  float2PFloat value = (PFloat ((0,0),show value))

  -- calcola lo step di spostamento dall'indirizzo base dell'array
  getStep :: Env -> AExpr -> [PInteger] -> Env
  getStep env@(program, last_temp, variables, labels, scope) a (dim:rest) = case a of
    (ArrSing step)   -> (generateExpr env (TypeBasicType TypeInt) step)
    (ArrMul a1 step) -> do
      -- valuto l'espressione
      let env1@(_,temp1@(Temp (t_c1,t_t1)),_,_,_) = (generateExpr env (TypeBasicType TypeInt) step)
      -- carico la dimensione di una riga e la moltiplico per l'espressione calcolata
      let env2@(_,temp2@(Temp (t_c2,t_t2)),_,_,_) = (addTACList env1 [BinOp   BOpMul (Temp (t_c1+2,t_t1)) temp1 (Temp (t_c1+1,t_t1)), AssignIntTemp (Temp (t_c1+1,t_t1)) dim])
        -- valuto il resto delle espressioni
      let env3@(_,temp3@(Temp (t_c3,t_t3)),_,_,_) = (getStep  env2 a1 rest)
      -- sommo 
      (addTACList env3 [BinOp   BOpPlus (Temp (t_c3+1,t_t3)) temp3 temp2])
  
  -- rende maneggevoli per il calcolo i bound del for
  for_bound_identifier :: Env -> ForId -> Expr
  for_bound_identifier env for_id =
    case for_id of
      ForInteger val -> ExprInt val
      ForIdent   id  -> ExprLeft (LExprId id)

  -- aggiunge la chiamata al main al programma se è presente nel codice la funzione
  ifmain :: Env -> Env
  ifmain env@(p, t, variables, l, _) = do
    case Map.lookup "main" variables of
      Nothing   -> env
      Just main -> (addTACList (p, t, variables, l, (0,0)) [FuncCall main (Temp (0,(TypeBasicType TypeVoid)))])