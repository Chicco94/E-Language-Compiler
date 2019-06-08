module TypeChecker where

  import Control.Monad
  
  import qualified Data.Map as Map
  import qualified Data.List as List
  
  import AbsE
  import PrintE
  import ErrM
  
  type Env       = ([Sig], [(BlockType, Context)])           -- Environment
  type Sig       = Map.Map String ([(Modality, Type)], Type) -- Functions signature: String -> ([(Modality, Type], Type) 
  type Context   = Map.Map String (Mutability, Type)         -- Variables context: String -> (Mutability, Type)
  
  -- Mutability of the variables (i.e., constants or variables).
  data Mutability = MutConst | MutVal | MutRef
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
  
  -- Block types.
  data BlockType = NormBlock | FunBlock PIdent Type | IterBlock | SelBlock | TryBlock | CatchBlock
    deriving(Eq, Ord, Show, Read)
  
  -- #######################################
  -- 					Core function:
  -- #######################################
  
  -- Type checking function.
  typeCheck :: Program -> Err (Env, [Program])
  typeCheck prog@(PDefs def) = foldM checkDecl (initEnv, []) def
  
  -- #######################################
  -- 					Check functions:
  -- #######################################
  
  -- Check declaration. 
  checkDecl :: (Env, [Program]) -> Decl -> Err (Env,[Program])
  checkDecl (env, prog) def = 
    case def of
      DeclFun lexpr args guard compstmt -> checkFunDecl (env, prog) lexpr args guard compstmt
      DeclStmt stmt -> checkDeclStmt (env, prog) stmt
      _ -> Ok (env, prog)
  
  -- Check function declaration.
  checkFunDecl :: (Env, [Program]) -> LExpr -> [Arg] -> Guard -> CompStmt -> Err (Env, [Program])
  checkFunDecl (env@(sig@(x:xs), blocks), prog) lexpr@(LExprId pident@(PIdent (p, fname))) args guard compstmt = do
    case guard of
      GuardType t ->
        case lookupFun sig fname of
          Ok t  -> fail $ show p ++ ": function " ++ printTree fname ++ " already declared"
          _     -> do case checkArgDecl Map.empty args of
                        Ok ctxt -> do
                          case extendEnv ((((addFun x fname args guard):xs), blocks), prog) ((FunBlock pident t), ctxt) compstmt of
                            Ok (e', p') -> Ok (e', prog ++ [(PDefs [TypedDecl (ADecl t (DeclFun lexpr args guard (StmtBlock (getDecls p'))))])])
                            Bad s -> Bad s
                        Bad s   -> Bad s
      GuardVoid -> fail $ show p ++ ": the function " ++ printTree fname ++ " must have a type to be well defined"
  
  -- Check arguments declaration.
  checkArgDecl :: Context -> [Arg] -> Err Context  
  checkArgDecl context [] = Ok context
  checkArgDecl context (arg@(ArgDecl mod (PIdent pident@(p,ident)) guard):args) = do
    case guard of
      GuardVoid   -> fail $ show p ++ ": argument " ++ printTree ident ++ " must have a type"
      GuardType t -> if mod == ModEmpty || mod == ModVal -- if no modality is provided, or if the modality is value, then add it as value
                       then checkArgDecl (addVar context (MutVal, ident) t) args
                       else 
                         if mod == ModConst 
                           then checkArgDecl (addVar context (MutConst, ident) t) args
                           -- if it is a reference, then set the type of the argument to a pointer lifted one level up
                           else checkArgDecl (addVar context (MutRef, ident) (TypeCompoundType (CompoundTypePtr (liftTypeToPointer (getBasicType (getInnerType t)) ((getTypeLevel t)+1))))) args
  
  -- Check statement declaration.
  checkDeclStmt :: (Env, [Program]) -> Stmt -> Err (Env, [Program])
  checkDeclStmt (env, prog) stmt =
    case stmt of
      StmtExpr expr                     -> checkExpr (env, prog) expr
      StmtVarInit pident guard cexpr    -> checkStmtInit (env, prog) pident guard cexpr MutVal
      StmtDefInit pident guard cexpr    -> checkStmtInit (env, prog) pident guard cexpr MutConst
      StmtReturn preturn expr           -> checkReturn (env, prog) preturn expr
      StmtNoReturn preturn              -> checkNoReturn (env, prog) preturn
      StmtBreak pbreak                  -> checkBreak (env, prog) pbreak
      StmtContinue pcontinue            -> checkContinue (env, prog) pcontinue
      SComp compstmt                    -> checkSComp (env, prog) compstmt
      StmtIfThenElse expr cstmt1 cstmt2 -> checkIfThenElse (env, prog) expr cstmt1 cstmt2
      StmtIfThen expr cstmt             -> checkIfThen (env, prog) expr cstmt
      StmtTryCatch cstmt1 cstmt2        -> checkTryCatch (env, prog) cstmt1 cstmt2
      StmtSwitchCase expr norm dflt     -> checkStmtSwitchCase (env, prog) expr norm dflt
      StmtWhile expr cstmt              -> checkWhile (env, prog) expr cstmt
      StmtLoopUntil cstmt expr          -> checkLoopUntil (env, prog) cstmt expr
      StmtFor pident range cstmt        -> checkFor (env, prog) pident range cstmt
  
  -- Check switch case statement.
  checkStmtSwitchCase :: (Env, [Program]) -> Expr -> [NormCase] -> [DfltCase] -> Err (Env, [Program])
  checkStmtSwitchCase (env@(sig@(x:xs), blocks), prog) expr norm dflt = do
    case checkNormalCases (env, prog) expr norm [] of
      Ok normacc        -> do
        case checkDefaultCases (env, prog) expr dflt [] of
          Ok dfltacc -> Ok (env, prog ++ [(PDefs [DeclStmt (StmtSwitchCase expr normacc dfltacc)])])
          Bad s      -> Bad s
      Bad s       -> Bad s
  
  -- Check normal cases
  checkNormalCases :: (Env, [Program]) -> Expr -> [NormCase] -> [NormCase] -> Err [NormCase]
  checkNormalCases (env, prog) _ [] acc = Ok acc
  checkNormalCases (env@(sig@(x:xs), blocks), prog) expr (y@(CaseNormal e cstmt):ys) acc = do
    texpr <- inferExpr env expr
    tcase <- inferExpr env e
    if tcase `isCompatibleWith` texpr
      then do
        (e', p') <- extendEnv (env, prog) (SelBlock, Map.empty) cstmt
        checkNormalCases (env, prog) expr ys (acc ++ [CaseNormal e (StmtBlock (getDecls p'))])
      else fail $ show (getExprPosition expr) ++ ": expression " ++ printTree expr ++ " has " ++ printTree texpr ++ " type, but the matching expression " ++ printTree e ++ " " ++ show (getExprPosition e) ++ " has " ++ printTree tcase ++ " type"
  
  -- Check default cases.
  checkDefaultCases :: (Env, [Program]) -> Expr -> [DfltCase] -> [DfltCase] -> Err [DfltCase]
  checkDefaultCases (env, prog) _ [] acc = Ok acc
  checkDefaultCases (env@(sig@(x:xs), blocks), prog) expr (y@(CaseDefault cstmt):ys) acc = do
    texpr <- inferExpr env expr
    (e', p') <- extendEnv (env, prog) (SelBlock, Map.empty) cstmt
    checkDefaultCases (env, prog) expr ys (acc ++ [CaseDefault (StmtBlock (getDecls p'))])
  
  -- Check if-then-else statement.
  checkIfThenElse :: (Env, [Program]) -> Expr -> CompStmt -> CompStmt -> Err (Env, [Program]) 
  checkIfThenElse (env@(sig@(x:xs), blocks), prog) expr cstmt1 cstmt2 = do
    texpr <- inferExpr env expr
    if texpr == tBool 
      then do
        (env1, p1) <- extendEnv (env, prog) (SelBlock, Map.empty) cstmt1
        (env2, p2) <- extendEnv (env, p1) (SelBlock, Map.empty) cstmt2
        Ok (env, prog ++ [(PDefs [DeclStmt (StmtIfThenElse expr (StmtBlock (getDecls p1)) (StmtBlock (getDecls p2)))])])
      else
        fail $ show (getExprPosition expr) ++ ": the expression " ++ printTree expr ++ " must be boolean"
  
  -- Check if-then statement.
  checkIfThen :: (Env, [Program]) -> Expr -> CompStmt -> Err (Env, [Program]) 
  checkIfThen (env@(sig@(x:xs), blocks), prog) expr cstmt = do
    texpr <- inferExpr env expr
    if texpr == tBool 
      then do
        (env1, p1) <- extendEnv (env, prog) (SelBlock, Map.empty) cstmt
        Ok (env, prog ++ [(PDefs [DeclStmt (StmtIfThen expr (StmtBlock (getDecls p1)))])])
      else
        fail $ show (getExprPosition expr) ++ ": the expression " ++ printTree expr ++ " must be boolean"
  
  checkTryCatch :: (Env, [Program]) -> CompStmt -> CompStmt -> Err (Env, [Program]) 
  checkTryCatch (env@(sig@(x:xs), blocks), prog) cstmt1 cstmt2 = do
    (env1, p1) <- extendEnv (env, prog) (TryBlock, Map.empty) cstmt1
    (env2, p2) <- extendEnv (env, p1) (CatchBlock, Map.empty) cstmt2
    Ok (env, prog ++ [(PDefs [DeclStmt (StmtTryCatch (StmtBlock (getDecls p1)) (StmtBlock (getDecls p2)))])])
  
  -- Check return with expression statement.
  checkReturn :: (Env, [Program]) -> PReturn -> Expr -> Err (Env, [Program])
  checkReturn (env@(sigs, blocks), prog) preturn@(PReturn (pr, _)) expr = do
    texpr <- inferExpr env expr
    case findFunBlockAndType blocks of
      Ok (PIdent (pf, fname), tfun) -> 
        if (texpr `isCompatibleWith` tfun) 
          -- If the type of the returned expression (texpr) is compatible with the type of the function (tfun), 
          -- then the returned type is tfun 
          then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl tfun (DeclStmt (StmtReturn preturn expr)))])])
          else fail $ show pr ++ ": the returned type is " ++ printTree texpr ++ ", but function " ++ printTree fname ++ " (declared at " ++ show pf ++ ") has return type " ++ printTree tfun
      _                            -> fail $ show pr ++ ": the return statement must be inside a function block"
  
  -- Check return without expression statement.
  checkNoReturn :: (Env, [Program]) -> PReturn -> Err (Env, [Program])
  checkNoReturn (env@(sigs, blocks), prog) preturn@(PReturn (pr, _)) = do
    case findFunBlockAndType blocks of
      Ok (PIdent (pf, fname), tfun) ->
        if (tfun == tVoid)
          then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl tfun (DeclStmt (StmtNoReturn preturn)))])])
          else fail $ show pr ++ ": the returned type is " ++ printTree TypeVoid ++ ", but function " ++ printTree fname ++ " (declared at " ++ show pf ++ ") has return type " ++ printTree tfun
      _                            -> fail $ show pr ++ ": the return statement must be inside a function declaration"
  
  -- Check break statement.
  checkBreak :: (Env, [Program]) -> PBreak -> Err (Env, [Program])
  checkBreak (env@(sigs, blocks), prog) pbreak@(PBreak (pb, _)) = do
    case findIterBlock blocks of
      Ok _ -> Ok (env, prog ++ [(PDefs [DeclStmt (StmtBreak pbreak)])])
      _    -> fail $ show pb ++ ": the break statement must be inside an iteration block"
  
  -- Check continue statement.
  checkContinue :: (Env, [Program]) -> PContinue -> Err (Env, [Program])
  checkContinue (env@(sigs, blocks), prog) pcontinue@(PContinue (pc, _)) = do
    case findIterBlock blocks of
      Ok _ -> Ok (env, prog ++ [(PDefs [DeclStmt (StmtContinue pcontinue)])])
      _    -> fail $ show pc ++ ": the continue statement must be inside an iteration block"
  
  -- Check compound statement.
  checkSComp :: (Env, [Program]) -> CompStmt -> Err (Env, [Program])
  checkSComp (env@(sigs, blocks), prog) compstmt =
    case extendEnv (env, prog) (NormBlock, Map.empty) compstmt of
      Ok (e', p') -> Ok (e', prog ++ [(PDefs [DeclStmt (SComp (StmtBlock (getDecls p')))])])
      Bad s       -> Bad s
  
  -- Check while statement.
  checkWhile :: (Env, [Program]) -> Expr -> CompStmt -> Err (Env, [Program])
  checkWhile (env@(xs, blocks@((blockType, context):ys)), prog) expr compstmt = do
    t <- inferExpr env expr
    if (t == tBool)
      then case extendEnv (env, prog) (IterBlock, Map.empty) compstmt of
             Ok (e', p') -> Ok (e', prog ++ [(PDefs [DeclStmt (StmtWhile expr (StmtBlock (getDecls p')))])])
             Bad s       -> Bad s
      else fail $ show (getExprPosition expr) ++ ": the expression " ++ printTree expr ++ " must be boolean"
  
  -- Check loop-until statement.
  checkLoopUntil :: (Env, [Program]) -> CompStmt -> Expr -> Err (Env, [Program])
  checkLoopUntil (env@(xs, blocks@((blockType, context):ys)), prog) compstmt expr = do
    t <- inferExpr env expr
    if (t == tBool)
      then case extendEnv (env, prog) (IterBlock, Map.empty) compstmt of
             Ok (e', p') -> Ok (e', prog ++ [(PDefs [DeclStmt (StmtLoopUntil (StmtBlock (getDecls p')) expr)])])
             Bad s       -> Bad s
      else fail $ show (getExprPosition expr) ++ ": the expression " ++ printTree expr ++ " must be boolean"
  
  -- Check for statement.
  checkFor :: (Env, [Program]) -> PIdent -> Range -> CompStmt -> Err (Env, [Program])
  checkFor (env@(xs, blocks@((blockType, context):ys)), prog) pident@(PIdent (p,ident)) range@(ExprRange e1 e2) compstmt = do
    case lookupVar blocks ident of
      Ok (m,t) -> do 
        if (t == tInt) && (m == MutVal)
          then do 
            t1 <- inferExpr env e1
            t2 <- inferExpr env e2
            if t1 == tInt && t2 == tInt
              then case extendEnv (env, prog) (IterBlock, Map.insert ident (MutConst, t) $ Map.empty) compstmt of
                                    Ok (e', p') -> Ok (e', prog ++ [(PDefs [DeclStmt (StmtFor pident range (StmtBlock (getDecls p')))])])
                                    Bad s       -> Bad s
              else fail $ show p ++ ": variable " ++ printTree ident ++ " in the for loop must iterate over a range where both types must be int; at the moment there is a " ++ printTree t1 ++ " type and a " ++ printTree t2 ++ " type"  
          else 
            if (t /= tInt)
              then fail $ show p ++ ": the variable " ++ printTree ident ++ " must have " ++ printTree tInt ++ " type in a range expression"
              else
                if (m /= MutVal) then fail $ show p ++ ": the variable " ++ printTree ident ++ " must be a variable instead of a constant in a range expression"
                                 else fail "checkFor: fatal error"
      _ -> fail $ show p ++ ": the variable " ++ printTree ident ++ " is not declared"
  
  -- Check initialization statement.
  checkStmtInit :: (Env, [Program]) -> PIdent -> Guard -> ComplexExpr -> Mutability -> Err (Env, [Program])
  checkStmtInit (env@(sig, blocks@((blockType, context):xs)), prog) pident@(PIdent (p, ident)) guard cexpr mut = case guard of
    GuardVoid        -> fail $ show p ++ ": variable " ++ printTree ident ++ " must have a type (" ++ printTree TypeVoid ++ " is not allowed)"
    GuardType tguard -> case tguard of
      TypeCompoundType (CompoundTypeArrayType (ArrDefBase pints basicType)) -> case checkArrayInitBounds (getBoundsFromPInts pints) cexpr of
        True  -> case inferArray env cexpr tguard of 
          Ok tarr -> Ok $ ((sig, ((blockType, addVar context (mut, ident) tguard):blocks)), prog ++ [(PDefs [TypedDecl (ADecl tguard (DeclStmt (StmtVarInit pident guard cexpr)))])])
          Bad s   -> fail $ show p ++ ": array " ++ printTree ident ++ " has type " ++ printTree tguard ++ " but there is an expression with type " ++ s ++ " in its initialization"
        False -> fail $ show p ++ ": array " ++ printTree ident ++ " has bounds " ++ show (getBoundsFromPInts pints) ++ " but its initialization does not match such bounds"
      TypeCompoundType (CompoundTypeArrayType (ArrDefPtr pints ptr)) -> case checkArrayInitBounds (getBoundsFromPInts pints) cexpr of
        True  -> case inferArray env cexpr tguard of 
                                 Ok tarr -> Ok $ ((sig, ((blockType, addVar context (mut, ident) tguard):blocks)), prog ++ [(PDefs [TypedDecl (ADecl tguard (DeclStmt (StmtVarInit pident guard cexpr)))])])
                                 Bad s   -> fail $ show p ++ ": array " ++ printTree ident ++ " has type " ++ printTree tguard ++ " but there is an expression with type " ++ s ++ " in its initialization"
        False -> fail $ show p ++ ": array " ++ printTree ident ++ " has bounds " ++ show (getBoundsFromPInts pints) ++ " but its initialization does not match such bounds"
      TypeCompoundType (CompoundTypeArrayType (ArrDefBaseC pints basicType)) -> case checkArrayInitBounds (getBoundsFromPInts pints) cexpr of
        True  -> case inferArray env cexpr tguard of 
          Ok tarr -> Ok $ ((sig, ((blockType, addVar context (mut, ident) tguard):blocks)), prog ++ [(PDefs [TypedDecl (ADecl tguard (DeclStmt (StmtVarInit pident guard cexpr)))])])
          Bad s   -> fail $ show p ++ ": array " ++ printTree ident ++ " has type " ++ printTree tguard ++ " but there is an expression with type " ++ s ++ " in its initialization"
        False -> fail $ show p ++ ": array " ++ printTree ident ++ " has bounds " ++ show (getBoundsFromPInts pints) ++ " but its initialization does not match such bounds"
      TypeCompoundType (CompoundTypeArrayType (ArrDefPtrC pints ptr)) -> case checkArrayInitBounds (getBoundsFromPInts pints) cexpr of
        True  -> case inferArray env cexpr tguard of 
                                 Ok tarr -> Ok $ ((sig, ((blockType, addVar context (mut, ident) tguard):blocks)), prog ++ [(PDefs [TypedDecl (ADecl tguard (DeclStmt (StmtVarInit pident guard cexpr)))])])
                                 Bad s   -> fail $ show p ++ ": array " ++ printTree ident ++ " has type " ++ printTree tguard ++ " but there is an expression with type " ++ s ++ " in its initialization"
        False -> fail $ show p ++ ": array " ++ printTree ident ++ " has bounds " ++ show (getBoundsFromPInts pints) ++ " but its initialization does not match such bounds"
  
      TypeCompoundType (CompoundTypePtr ptr) -> do
        expr <- getExprFromComplexExpr cexpr
        texpr <- inferExpr env expr
        if (decomposePtr ptr) == TypeVoid
          then fail $ show p ++ ": variable " ++ printTree ident ++ " must have a type (" ++ printTree TypeVoid ++ " is not allowed)"
          else do
            if ((getInitLevel expr) + (getTypeLevel texpr)) == (getTypeLevel tguard)
              then do
                if (getInnerType texpr) == (getInnerType tguard)
                  then 
                    case lookupVar blocks ident of
                      Ok (m,t) -> fail $ show p ++ ": variable " ++ printTree ident ++ " already declared"
                      _        -> do 
                        Ok $ ((sig, ((blockType, addVar context (mut, ident) tguard):blocks)), prog ++ [(PDefs [TypedDecl (ADecl tguard (DeclStmt (StmtVarInit pident guard cexpr)))])])
                  else fail $ show (getExprPosition expr) ++ ": expression " ++ printTree expr ++ " has type " ++ printTree texpr ++ " which is incompatible with variable " ++ show ident ++ " " ++ show p ++ " that has type " ++ printTree tguard
              -- if not
              else fail $ show p ++ ": variable " ++ show ident ++ " has a pointer level " ++ show (getTypeLevel tguard) ++ " which is incompatible with expression " ++ printTree expr ++ " that has pointer level " ++ show ((getInitLevel expr) + (getTypeLevel texpr))
      TypeBasicType basicType -> do
        if basicType == TypeVoid
          then fail $ show p ++ ": array " ++ printTree ident ++ " must have a type (" ++ printTree TypeVoid ++ " type is not allowed)"
          else do 
            expr <- getExprFromComplexExpr cexpr
            texpr <- inferExpr env expr
            -- if the levels are the same
            if ((getInitLevel expr) + (getTypeLevel texpr)) == (getTypeLevel tguard)
              then do
                if (getInnerType texpr) `isCompatibleWith` (getInnerType tguard) -- the inner type of the expression must be compatible with the inner type of the guard
                  then 
                    case lookupVar blocks ident of
                      Ok (m,t) -> fail $ show p ++ ": variable " ++ printTree ident ++ " already declared"
                      _        -> do 
                        Ok $ ((sig, ((blockType, addVar context (mut, ident) tguard):blocks)), prog ++ [(PDefs [TypedDecl (ADecl tguard (DeclStmt (StmtVarInit pident guard cexpr)))])])
                  else fail $ show (getExprPosition expr) ++ ": expression " ++ printTree expr ++ " has type " ++ printTree (getInnerType texpr) ++ " which is incompatible with variable " ++ show ident ++ " " ++ show p ++ " that has type " ++ printTree (getInnerType tguard)
              -- if not
              else fail $ show p ++ ": variable " ++ show ident ++ " has a pointer level " ++ show (getTypeLevel tguard) ++ " which is incompatible with expression " ++ printTree expr ++ " that has pointer level " ++ show ((getInitLevel expr) + (getTypeLevel texpr))
  
  -- Check (right) expression.
  checkExpr :: (Env, [Program]) -> Expr -> Err (Env, [Program])
  checkExpr (env@(sigs,blocks), prog) expr = do
    case expr of
      -- Assignment statement (i.e., lexpr operator expr).
      ExprAssign lexpr op expr -> do
        case lookupVar blocks (findLExprName lexpr) of
          Ok (m,t) -> do
            if m == MutVal || m == MutRef
              then checkAssign (env, prog) lexpr op expr
              else fail $ show (getLExprPosition lexpr) ++ ": variable " ++ printTree (findLExprName lexpr) ++ " is defined as constant, you cannot assign a value to a constant"
          _        -> fail $ show (getLExprPosition lexpr) ++ ": variable " ++ printTree (findLExprName lexpr) ++ " is not defined"
  
      -- Ternary expression (i.e. (expr) ? expr : expr).
      ExprTernaryIf e1 e2 e3 -> do -- ASSUMPTION: t2 and t3 must have the same type
        t1 <- inferExpr env e1
        t2 <- inferExpr env e2
        t3 <- inferExpr env e3
        if t1 == tBool
          then do
            if t2 == t3
              then return (env, prog ++ [(PDefs [TypedDecl (ADecl t2 (DeclStmt (StmtExpr expr)))])])
              else fail $ show (getExprPosition e2) ++ ": expression " ++ printTree e2 ++ " must have the same type of the expression " ++ printTree e3 ++ "; at the moment there is a " ++ printTree t2 ++ " type and a " ++ printTree t3 ++ " type"
          else fail $ show (getExprPosition e1) ++ ": expression " ++ printTree e1 ++ " must have " ++ printTree tBool ++ " type"
  
      -- Left expressions.
      ExprLeft lexpr           -> do t <- inferLExpr env lexpr
                                     return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr (ExprLeft lexpr))))])])
  
      -- Base expressions (i.e., integers, floats, chars, strings, booleans)
      ExprInt int              -> return (env, prog ++ [(PDefs [TypedDecl (ADecl tInt (DeclStmt (StmtExpr (ExprInt int))))])])
      ExprFloat flt            -> return (env, prog ++ [(PDefs [TypedDecl (ADecl tFloat (DeclStmt (StmtExpr (ExprFloat flt))))])])
      ExprChar chr             -> return (env, prog ++ [(PDefs [TypedDecl (ADecl tChar (DeclStmt (StmtExpr (ExprChar chr))))])])
      ExprString str           -> return (env, prog ++ [(PDefs [TypedDecl (ADecl tString (DeclStmt (StmtExpr (ExprString str))))])])
      ExprTrue ptrue           -> return (env, prog ++ [(PDefs [TypedDecl (ADecl tBool (DeclStmt (StmtExpr (ExprTrue ptrue))))])])
      ExprFalse pfalse         -> return (env, prog ++ [(PDefs [TypedDecl (ADecl tBool (DeclStmt (StmtExpr (ExprFalse pfalse))))])])
  
      -- Function call.
      ExprFunCall pident args  -> do checkFunCall (env, prog) expr
  
      -- Boolean not.
      ExprBoolNot e            -> do t <- inferBoolUnOp env expr e
                                     return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
  
      -- Arithmetic unary operators.
      ExprAddition e           -> do t <- inferArithUnOp env expr e
                                     return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])  
      ExprNegation e           -> do t <- inferArithUnOp env expr e
                                     return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
  
      -- Arithmetic binary operators.
      ExprPower e1 e2    -> do t <- inferArithBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprMul e1 e2      -> do t <- inferArithBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprDiv e1 e2      -> do t <- inferArithBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprReminder e1 e2 -> do t <- inferArithBinOpMod env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprModulo e1 e2   -> do t <- inferArithBinOpMod env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprPlus e1 e2     -> do t <- inferArithBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprMinus e1 e2    -> do t <- inferArithBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
  
      -- Relational binary operators.
      ExprLt e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprGt e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprLtEq e1 e2     -> do t <- inferRelBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprGtEq e1 e2     -> do t <- inferRelBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprEq e1 e2       -> do t <- inferRelBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprNeq e1 e2      -> do t <- inferRelBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
  
      -- Boolean binary operator.
      ExprAnd e1 e2      -> do t <- inferBoolBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
      ExprOr e1 e2       -> do t <- inferBoolBinOp env expr e1 e2
                               return (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr expr)))])])
  
  -- Check assignment.
  checkAssign :: (Env, [Program]) -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
  checkAssign (env, prog) lexpr op expr = do
    case op of
      -- Normal assignment (i.e., ':=').
      OpAssign      -> do checkAssignOp (env, prog) lexpr op expr
  
      -- Assignment with boolean operator (i.e., '|=', '&=').
      OpOr          -> do checkAssignBoolOp (env, prog) lexpr op expr
      OpAnd         -> do checkAssignBoolOp (env, prog) lexpr op expr
      
      -- Assignment with arithmetic operator (e.g., '+=', '%%=').
      OpPlus        -> do checkAssignOp (env, prog) lexpr op expr
      OpMinus       -> do checkAssignOp (env, prog) lexpr op expr
      OpMul         -> do checkAssignOp (env, prog) lexpr op expr
      OpDiv         -> do checkAssignOp (env, prog) lexpr op expr
      OpRemainder   -> do checkAssignOpMod (env, prog) lexpr op expr
      OpModulo      -> do checkAssignOpMod (env, prog) lexpr op expr
      OpPower       -> do checkAssignOp (env, prog) lexpr op expr  
  
  isCompoundType :: Type -> Bool
  isCompoundType t = case t of TypeCompoundType _ -> True
                               otherwise          -> False
  
  checkTypes :: Type -> Type -> Bool
  checkTypes t1 t2 = 
    if (isCompoundType t1) || (isCompoundType t2)
      then ((getInnerType t1) == (getInnerType t1)) && ((getBoundsFromType t1) == (getBoundsFromType t2))
      else (getInnerType t1) `isCompatibleWith` (getInnerType t1)
  
  -- Check assignment operator.
  checkAssignOp :: (Env, [Program]) -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
  checkAssignOp (env, prog) lexpr op expr = do
    tlexpr <- inferLExpr env lexpr
    texpr <- inferExpr env expr
    if ((getInitLevel expr) + (getTypeLevel texpr)) == ((getLeftLevel lexpr) + (getTypeLevel tlexpr))
      then do
        case lexpr of
          -- if array access
          LExprArr _ -> do
            case expr of
              ExprLeft (LExprArr _) -> do
                if checkTypes (getInnerType tlexpr) (getInnerType texpr)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree (getInnerType tlexpr) ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree (getInnerType texpr)
              _          -> do 
                if checkTypes (getInnerType tlexpr) texpr
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree (getInnerType tlexpr) ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree texpr
          -- else
          _            -> do 
            case expr of
              ExprLeft (LExprArr _) -> do
                if checkTypes tlexpr (getInnerType texpr)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree tlexpr ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree (getInnerType texpr)
              _          -> do 
                if checkTypes tlexpr texpr
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree tlexpr ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree texpr
      else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr ++ " has a pointer level " ++ show ((getLeftLevel lexpr) + (getTypeLevel tlexpr)) ++ " which is incompatible with the (right) expression " ++ printTree expr ++ " that has pointer level " ++ show ((getInitLevel expr) + (getTypeLevel texpr))
  
  -- Check assignment boolean operator.
  checkAssignBoolOp :: (Env, [Program]) -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
  checkAssignBoolOp (env, prog) lexpr op expr = do
    tlexpr <- inferLExpr env lexpr
    texpr <- inferExpr env expr 
    if ((getInitLevel expr) + (getTypeLevel texpr)) == ((getLeftLevel lexpr) + (getTypeLevel tlexpr))
      then do
        case lexpr of
          -- if array access
          LExprArr _ -> do
            case expr of
              ExprLeft (LExprArr _) -> do
                if checkTypes (getInnerType tlexpr) (getInnerType texpr) && (getInnerType tlexpr == tBool) && (getInnerType texpr == tBool)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree (getInnerType tlexpr) ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree (getInnerType texpr)
              _          -> do 
                if checkTypes (getInnerType tlexpr) texpr && (getInnerType tlexpr == tBool) && (getInnerType texpr == tBool)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree (getInnerType tlexpr) ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree texpr
          -- else
          _            -> do 
            case expr of
              ExprLeft (LExprArr _) -> do
                if checkTypes tlexpr (getInnerType texpr) && (getInnerType tlexpr == tBool) && (getInnerType texpr == tBool)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree tlexpr ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree (getInnerType texpr)
              _          -> do 
                if checkTypes tlexpr texpr && (getInnerType tlexpr == tBool) && (getInnerType texpr == tBool)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree tlexpr ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree texpr
      else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr ++ " has a pointer level " ++ show ((getLeftLevel lexpr) + (getTypeLevel tlexpr)) ++ " which is incompatible with the (right) expression " ++ printTree expr ++ " that has pointer level " ++ show ((getInitLevel expr) + (getTypeLevel texpr)) 
  
  -- Check modulo and remainder operators.
  checkAssignOpMod :: (Env, [Program]) -> LExpr -> AssignOperator -> Expr -> Err (Env, [Program])
  checkAssignOpMod (env, prog) lexpr op expr = do
    tlexpr <- inferLExpr env lexpr
    texpr <- inferExpr env expr 
    if ((getInitLevel expr) + (getTypeLevel texpr)) == ((getLeftLevel lexpr) + (getTypeLevel tlexpr))
      then do
        case lexpr of
          -- if array access
          LExprArr _ -> do
            case expr of
              ExprLeft (LExprArr _) -> do
                if checkTypes (getInnerType tlexpr) (getInnerType texpr) && (getInnerType tlexpr == tInt) && (getInnerType texpr == tInt)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree (getInnerType tlexpr) ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree (getInnerType texpr)
              _          -> do 
                if checkTypes (getInnerType tlexpr) texpr && (getInnerType tlexpr == tInt) && (getInnerType texpr == tInt)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree (getInnerType tlexpr) ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree texpr
          -- else
          _            -> do 
            case expr of
              ExprLeft (LExprArr _) -> do
                if checkTypes tlexpr (getInnerType texpr) && (getInnerType tlexpr == tInt) && (getInnerType texpr == tInt)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree tlexpr ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree (getInnerType texpr)
              _          -> do 
                if checkTypes tlexpr texpr && (getInnerType tlexpr == tBool) && (getInnerType texpr == tBool)
                  then Ok (env, prog ++ [(PDefs [TypedDecl (ADecl (getInnerType tlexpr) (DeclStmt (StmtExpr (ExprAssign lexpr op expr))))])])
                  else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr  ++ " has type " ++ printTree tlexpr ++ " and the (right) expression " ++ printTree expr ++ " has type " ++ printTree texpr
      else fail $ show (getLExprPosition lexpr) ++ ": the (left) expression " ++ printTree lexpr ++ " has a pointer level " ++ show ((getLeftLevel lexpr) + (getTypeLevel tlexpr)) ++ " which is incompatible with the (right) expression " ++ printTree expr ++ " that has pointer level " ++ show ((getInitLevel expr) + (getTypeLevel texpr)) 
  
  -- Check function call.
  checkFunCall :: (Env, [Program]) -> Expr -> Err (Env, [Program])
  checkFunCall (env@(sig, _), prog) (ExprFunCall pident@(PIdent (p,ident)) args) = do
    case lookupFun sig ident of
      Ok funDef@(mods,t) -> 
        if length args == length mods --
          then do
            case checkFunCallArgs env pident mods args of
              Ok _  -> Ok (env, prog ++ [(PDefs [TypedDecl (ADecl t (DeclStmt (StmtExpr (ExprFunCall pident args))))])])
              Bad s -> fail s
          else fail $ show p ++ ": function " ++ printTree ident ++ " doesn't match the numerosity of its parameters"
      _                  -> fail $ show p ++ ": function " ++ printTree ident ++ " is not defined" 
  
  -- Check the arguments of a function call.
  checkFunCallArgs :: Env -> PIdent -> [(Modality, Type)] -> [Expr] -> Err String
  checkFunCallArgs env _ [] [] = Ok "fun call arguments are ok"
  checkFunCallArgs env@(sig, blocks) pident@(PIdent (p, ident)) (x@(mod,tdecl):xs) (expr:exprs) = do
    targ <- inferExpr env expr
    if ((getInitLevel expr) + (getTypeLevel targ)) == (getTypeLevel tdecl)
     then do
       if (getInnerType targ) `isCompatibleWith` (getInnerType tdecl)
         then do
           case expr of
             ExprLeft lexpr -> do
               case lookupVar blocks (findLExprName lexpr) of
                 Ok (m,t) -> do
                   if (length (getBoundsFromType targ)) == (length (getBoundsFromType t))
                     then checkFunCallArgs env pident xs exprs
                     else fail $ show p ++ ": function " ++ show ident ++ " is called with argument " ++ printTree lexpr ++ " that has bounds " ++ show (getBoundsFromType targ) ++ " but the definition of such argument requires to have bounds " ++ show (getBoundsFromType t)
                 _    -> fail $ show p ++ ": variable " ++ show ident ++ " is not declared"
             _ -> do checkFunCallArgs env pident xs exprs
       else fail $ show (getExprPosition expr) ++ ": expression argument " ++ printTree expr ++ " has type " ++ printTree targ ++ " but the declaration of the function requires to have type " ++ printTree tdecl
     else fail $ show (getExprPosition expr) ++ ": expression " ++ printTree expr ++ " has a pointer level " ++ show ((getInitLevel expr) + (getTypeLevel targ)) ++ " but the function requires pointer level " ++ show (getTypeLevel tdecl)
  
  -- #######################################
  -- 					Inference functions:
  -- #######################################
  
  -- Infer (right) expression.
  inferExpr :: Env -> Expr -> Err Type
  inferExpr env expr =
    case expr of
      -- Left expressions. 
      ExprLeft lexpr -> inferLExpr env lexpr 
  
      -- Ternary if.
      ExprTernaryIf e1 e2 e3 -> inferTernaryIf env e1 e2 e3
  
      -- Base expressions (i.e., integers, floats, chars, strings, booleans) 
      ExprTrue _    -> Ok tBool
      ExprFalse _   -> Ok tBool
      ExprInt _     -> Ok tInt
      ExprFloat _   -> Ok tFloat
      ExprChar _    -> Ok tChar
      ExprString _  -> Ok tString
      
      -- Function call.
      ExprFunCall pident args -> inferFunCall env expr
  
      -- Boolean not.
      ExprBoolNot e      -> inferBoolUnOp env expr e
  
      -- Address operator (i.e. "&").
      ExprReference le   -> inferLExpr env le
  
      -- Arithmetic unary operators.
      ExprAddition e           -> inferArithUnOp env expr e
      ExprNegation e           -> inferArithUnOp env expr e
  
      -- Arithmetic binary operators.
      ExprPower e1 e2    -> inferArithBinOp env expr e1 e2
      ExprMul e1 e2      -> inferArithBinOp env expr e1 e2
      ExprDiv e1 e2      -> inferArithBinOp env expr e1 e2
      ExprReminder e1 e2 -> inferArithBinOpMod env expr e1 e2
      ExprModulo e1 e2   -> inferArithBinOpMod env expr e1 e2
      ExprPlus e1 e2     -> inferArithBinOp env expr e1 e2
      ExprMinus e1 e2    -> inferArithBinOp env expr e1 e2
  
      -- Relational binary operators.
      ExprLt e1 e2       -> inferRelBinOp env expr e1 e2
      ExprGt e1 e2       -> inferRelBinOp env expr e1 e2
      ExprLtEq e1 e2     -> inferRelBinOp env expr e1 e2
      ExprGtEq e1 e2     -> inferRelBinOp env expr e1 e2
      ExprEq e1 e2       -> inferRelBinOp env expr e1 e2
  
      -- Boolean binary operator.
      ExprAnd e1 e2      -> inferBoolBinOp env expr e1 e2
      ExprOr e1 e2       -> inferBoolBinOp env expr e1 e2
      _ -> fail $ "inferExpr: fatal error"
  
  -- Infer function call.
  inferFunCall :: Env -> Expr -> Err Type
  inferFunCall env@(sig, blocks) (ExprFunCall pident@(PIdent (p,ident)) args) = do
    case lookupFun sig ident of
      Ok (mods,t) -> do
        if length args == length mods
          then do
            case checkFunCallArgs env pident mods args of
              Ok _  -> Ok t
              Bad s -> fail s 
          else fail $ show p ++ ": function " ++ printTree ident ++ " is declared with " ++ show (length mods) ++ " parameters, passing " ++ show (length args) ++ " arguments to the function is not wise"
      _ -> fail $ show p ++ ": function " ++ printTree ident ++ " not defined" 
  
  -- Infer left expression. 
  inferLExpr :: Env -> LExpr -> Err Type
  inferLExpr env@(sig, blocks) lexpr = do
    case lexpr of
      LExprId (PIdent (p, ident)) -> 
        case lookupVar blocks ident of
          Ok (_,t) -> Ok t
          _        -> fail $ show p ++ ": variable " ++ printTree ident ++ " not defined!"
      LExprRef (LRefExpr le)       -> inferLExpr env le
      LExprArr (LArrExpr pident@(PIdent (p,ident)) aexpr) -> do 
        case lookupVar blocks ident of
          Ok (_, tid) -> do
            case tid of
              TypeCompoundType (CompoundTypeArrayType t) -> do
                case t of
                  ArrDefBase pints _ -> do
                    if (getNumberOfArgsFromArrayAccess aexpr) == (length (getBoundsFromPInts pints))
                      then Ok tid
                      else fail $ show p ++ ": array " ++ printTree ident ++ " has bounds " ++ show (getBoundsFromPInts pints) ++ " (i.e., " ++ show (length (getBoundsFromPInts pints)) ++ " indices), but you are trying to access an element of such array through " ++ show (getNumberOfArgsFromArrayAccess aexpr) ++ " indices" 
                  ArrDefBaseC pints _ -> do
                    if (getNumberOfArgsFromArrayAccess aexpr) == (length (getBoundsFromPInts pints))
                      then Ok tid
                      else fail $ show p ++ ": array " ++ printTree ident ++ " has bounds " ++ show (getBoundsFromPInts pints) ++ " (i.e., " ++ show (length (getBoundsFromPInts pints)) ++ " indices), but you are trying to access an element of such array through " ++ show (getNumberOfArgsFromArrayAccess aexpr) ++ " indices" 
                  ArrDefPtr pints _ -> do
                    if (getNumberOfArgsFromArrayAccess aexpr) == (length (getBoundsFromPInts pints))
                      then Ok tid
                      else fail $ show p ++ ": array " ++ printTree ident ++ " has bounds " ++ show (getBoundsFromPInts pints) ++ " (i.e., " ++ show (length (getBoundsFromPInts pints)) ++ " indices), but you are trying to access an element of such array through " ++ show (getNumberOfArgsFromArrayAccess aexpr) ++ " indices" 
                  ArrDefPtrC pints _ -> do
                    if (getNumberOfArgsFromArrayAccess aexpr) == (length (getBoundsFromPInts pints))
                      then Ok tid
                      else fail $ show p ++ ": array " ++ printTree ident ++ " has bounds " ++ show (getBoundsFromPInts pints) ++ " (i.e., " ++ show (length (getBoundsFromPInts pints)) ++ " indices), but you are trying to access an element of such array through " ++ show (getNumberOfArgsFromArrayAccess aexpr) ++ " indices" 
              _                                -> fail $ "inferLExpr: tid not typecompound"
          _ -> fail $ show p ++ ": variable " ++ printTree ident ++ " not defined"
  
  -- Infer ternary if.
  inferTernaryIf :: Env -> Expr -> Expr -> Expr -> Err Type
  inferTernaryIf env e1 e2 e3 = do
    t1 <- inferExpr env e1
    t2 <- inferExpr env e2
    t3 <- inferExpr env e3
    if t1 == tBool
      then do
        if t2 == t3
          then Ok t2
          else fail $ show (getExprPosition e2) ++ ": expression " ++ printTree e2 ++ " must have the same type of the expression " ++ printTree e3 ++ "; at the moment there is a " ++ printTree t2 ++ " type and a " ++ printTree t3 ++ " type"
      else fail $ show (getExprPosition e1) ++ ": expression " ++ printTree e1 ++ " must have " ++ printTree tBool ++ " type"
  
  -- Infer arithmetic unary operator.
  inferArithUnOp :: Env -> Expr -> Expr -> Err Type
  inferArithUnOp env expr e = do
    t <- inferExpr env e
    if ((getInnerType t) `isCompatibleWithAny` [tBool,tChar,tInt, tFloat]) -- ASSUMPTION: void and string are not allowed
      then Ok t
      else fail $ show (getExprPosition e) ++ ": " ++ printTree e ++ " has type " ++ printTree t ++ " (void and string are not allowed)"
  
  -- Infer arithmetic binary operator.
  inferArithBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
  inferArithBinOp env expr e1 e2 = do
    t1 <- inferExpr env e1
    t2 <- inferExpr env e2
    if ((getInnerType t1) `isCompatibleWithAny` [tBool,tChar,tInt, tFloat]) && -- ASSUMPTION: void and string are not allowed
       ((getInnerType t2) `isCompatibleWithAny` [tBool,tChar,tInt, tFloat]) &&
       areCompatible (getInnerType t1) (getInnerType t2)
       then getMostGeneric t1 t2
       else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ printTree t1 ++ " and " ++ printTree e2 ++ " " ++ show (getExprPosition e2) ++ " having type " ++ printTree t2 ++ " (void and string are not allowed)"
  
  -- Infer arithmetic integer division or float division operator.
  inferArithBinOpDiv :: Env -> Expr -> Expr -> Expr -> Type -> Err Type
  inferArithBinOpDiv env expr e1 e2 top = do
    t1 <- inferExpr env e1
    t2 <- inferExpr env e2
    if (top `isCompatibleWith` (getInnerType t1)) && (top `isCompatibleWith` (getInnerType t2)) &&
       areCompatible (getInnerType t1) (getInnerType t2)
       then Ok top
       else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ printTree t1 ++ " and " ++ printTree e2 ++ " having type " ++ printTree t2 ++ ", but the type of the operation, which is " ++ printTree top ++ ", is not compatible with any of such types"
  
  -- Infer arithmetic modulo or remainder operator (between integers).
  inferArithBinOpMod :: Env -> Expr -> Expr -> Expr -> Err Type
  inferArithBinOpMod env expr e1 e2 = do
    t1 <- inferExpr env e1
    t2 <- inferExpr env e2
    if ((getInnerType t1) == tInt) && -- ASSUMPTION: only ints are allowed
       ((getInnerType t2) == tInt) &&
       areCompatible (getInnerType t1) (getInnerType t2)
       then getMostGeneric t1 t2
       else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ printTree t1 ++ " and " ++ printTree e2 ++ " " ++ show (getExprPosition e2) ++ " having type " ++ printTree t2 ++ " (only int allowed)"
  
  -- Infer relational binary operator.
  inferRelBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
  inferRelBinOp env expr e1 e2 = do
    t1 <- inferExpr env e1
    t2 <- inferExpr env e2
    if ((getInnerType t1) `isCompatibleWithAny` [tBool, tInt, tFloat, tChar, tString]) &&  -- ASSUMPTION: only ints, floats, chars and strings are allowed
       ((getInnerType t2) `isCompatibleWithAny` [tBool, tInt, tFloat, tChar, tString]) &&  -- the same as above
       areCompatible (getInnerType t1) (getInnerType t2)
       then Ok tBool
       else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ printTree t1 ++ " and " ++ printTree e2 ++ " " ++ show (getExprPosition e2) ++ " having type " ++ printTree t2 ++ " (only bool, int, float, char and string allowed)"
  
  -- Infer boolean unary operator.
  inferBoolUnOp :: Env -> Expr -> Expr -> Err Type
  inferBoolUnOp env expr e = do
    t <- inferExpr env e
    if ((getInnerType t) == tBool) -- only booleans are allowed
      then Ok t
      else fail $ show (getExprPosition e) ++ ": " ++ printTree e ++ " has type " ++ printTree t ++ " which must be boolean"
  
  -- Infer boolean binary operator.
  inferBoolBinOp :: Env -> Expr -> Expr -> Expr -> Err Type
  inferBoolBinOp env expr e1 e2 = do
    t1 <- inferExpr env e1
    t2 <- inferExpr env e2
    if ((getInnerType t1) == tBool) && ((getInnerType t2) == tBool) -- ASSUMPTION: only booleans are allowed
      then Ok tBool  
      else fail $ show (getExprPosition e1) ++ ": invalid type operands in-between " ++ printTree e1 ++ " having type " ++ printTree t1 ++ " and " ++ printTree e2 ++ " " ++ show (getExprPosition e2) ++ " having type " ++ printTree t2 ++ " (only bool allowed)"
  
  
  -- #######################################
  -- 					Auxiliary functions:
  -- #######################################
  
  tInt    = TypeBasicType TypeInt    :: Type
  tChar   = TypeBasicType TypeChar   :: Type
  tString = TypeBasicType TypeString :: Type
  tFloat  = TypeBasicType TypeFloat  :: Type
  tBool   = TypeBasicType TypeBool   :: Type
  tVoid   = TypeBasicType TypeVoid   :: Type
  
  -- Default given primitives.
  defaultPrimitives :: Sig
  defaultPrimitives = 
    Map.insert "writeInt"    ([(ModEmpty, tInt)], tVoid) $     -- void writeInt(int i)
    Map.insert "writeFloat"  ([(ModEmpty, tFloat)], tVoid) $   -- void writeFloat(float d)
    Map.insert "writeChar"   ([(ModEmpty, tChar)], tVoid) $    -- void writeChar(char c)
    Map.insert "writeString" ([(ModEmpty, tString)], tVoid) $  -- void writeString(string s)
    Map.insert "readInt"     ([], tInt) $                      -- int readInt()
    Map.insert "readFloat"   ([], tFloat) $                    -- float readFloat()
    Map.insert "readChar"    ([], tChar) $                     -- char readChar()
    Map.insert "readString"  ([], tString) Map.empty           -- string readString()
  
  -- Initialize the environment with the default primitives.
  initEnv :: Env
  initEnv = ([defaultPrimitives], [(NormBlock, Map.empty)])
  
  -- Look for function in signature.
  lookupFun :: [Sig] -> String -> Err ([(Modality, Type)], Type)
  lookupFun [] _ = Bad "function is not declared!"
  lookupFun sig@(x:xs) ident =
    case Map.lookup ident x of
      Just t -> Ok t
      Nothing -> lookupFun xs ident
  
  -- Look for variable/constant in blocks.
  lookupVar :: [(BlockType, Context)] -> String -> Err (Mutability, Type)
  lookupVar [] _ = Bad "variable is not declared!"
  lookupVar ((blockType, context):xs) ident = 
    case Map.lookup ident context of
      Just (m,t) -> Ok (m,t)
      Nothing -> lookupVar xs ident
  
  -- Add function to signature.
  addFun :: Sig -> String -> [Arg] -> Guard -> Sig
  addFun sig fname args guard = 
    case guard of
      GuardType retType -> Map.insert fname ([(mod, t) | (ArgDecl mod _ (GuardType t)) <- args], retType) sig 
      GuardVoid -> Map.insert fname ([(mod, t) | (ArgDecl mod _ (GuardType t)) <- args], tVoid) sig -- procedures
  
  -- Add variable/constant to signature.
  addVar :: Context -> (Mutability, String) -> Type -> Context
  addVar context (m,ident) t = Map.insert ident (m,t) context
  
  -- T1 is compatible with T2?
  isCompatibleWith :: Type -> Type -> Bool
  t1 `isCompatibleWith` t2
    | t1 == t2          = True
    | t1 == tBool       = t2 `elem` [tChar,tInt,tFloat,tString]
    | t1 == tInt        = t2 == tFloat
    | t1 == tChar       = t2 `elem` [tInt,tFloat,tString]
    | otherwise         = False
  
  -- T is compatible with any of [T1,..Tn]?
  isCompatibleWithAny :: Type -> [Type] -> Bool
  t `isCompatibleWithAny` types = any (isCompatibleWith t) types
  
  -- Are T1 and T2 compatible ?
  areCompatible :: Type -> Type -> Bool
  areCompatible t1 t2 = (t1 `isCompatibleWith` t2) || (t2 `isCompatibleWith` t1)
  
  -- Get most generic type in between T1 and T2.
  getMostGeneric :: Type -> Type -> Err Type
  getMostGeneric t1 t2
    | t1 == t2               = Ok t1
    | tFloat `elem` [t1,t2]  = Ok tFloat
    | tInt `elem` [t1,t2]    = Ok tInt
    | tString `elem` [t1,t2] = Ok tString
    | tChar `elem` [t1,t2]   = Ok tChar
  
  -- Get list of declarations from list of program.
  getDecls :: [Program] -> [Decl]
  getDecls [] = []
  getDecls (prog:progs) = case prog of
    PDefs [TypedDecl (ADecl t s)] -> (TypedDecl (ADecl t s)) : getDecls progs
    PDefs [DeclStmt stmt] -> (DeclStmt stmt) : getDecls progs 
    _ -> []
  
  -- Get (right) expression position.
  getExprPosition :: Expr -> (Int, Int)
  getExprPosition e =
    case e of
      ExprInt (PInteger (p, _))   -> p
      ExprFloat (PFloat (p, _))   -> p
      ExprChar (PChar (p, _))     -> p
      ExprString (PString (p, _)) -> p
      ExprTrue (PTrue (p, _))     -> p
      ExprFalse (PFalse (p, _))   -> p
  
      ExprAssign lexpr op rexpr -> getLExprPosition lexpr
      ExprLeft lexpr            -> getLExprPosition lexpr
      ExprTernaryIf e1 e2 e3    -> getExprPosition e1
  
      ExprBoolNot rexpr         -> getExprPosition rexpr
      ExprNegation rexpr        -> getExprPosition rexpr
      ExprAddition rexpr        -> getExprPosition rexpr
      ExprReference lexpr       -> getLExprPosition lexpr
      
      ExprPower e1 e2           -> getExprPosition e1
      ExprMul e1 e2             -> getExprPosition e1
      ExprDiv e1 e2             -> getExprPosition e1
      ExprReminder e1 e2        -> getExprPosition e1
      ExprModulo e1 e2          -> getExprPosition e1
  
      ExprPlus e1 e2            -> getExprPosition e1
      ExprMinus e1 e2           -> getExprPosition e1
   
      ExprLt e1 e2              -> getExprPosition e1
      ExprGt e1 e2              -> getExprPosition e1
      ExprLtEq e1 e2            -> getExprPosition e1
      ExprGtEq e1 e2            -> getExprPosition e1
  
      ExprEq e1 e2              -> getExprPosition e1
      ExprNeq e1 e2             -> getExprPosition e1
  
      ExprAnd e1 e2             -> getExprPosition e1
      ExprOr e1 e2              -> getExprPosition e1
   
  -- Get left expression position.
  getLExprPosition :: LExpr -> (Int, Int)
  getLExprPosition e =
    case e of
      LExprId (PIdent (p, ident))                        -> p
      LExprRef (LRefExpr le)                             -> getLExprPosition le
      LExprArr (LArrExpr pident@(PIdent (p,ident)) re)   -> p    
  
  -- Find function block and its type, if exists.
  findFunBlockAndType :: [(BlockType, Context)] -> Err (PIdent, Type)
  findFunBlockAndType [] = fail $ "there is no function block declared"
  findFunBlockAndType (block@(blockType, context):blocks) = 
    case blockType of
      FunBlock pident t -> Ok (pident, t)
      _                 -> findFunBlockAndType blocks
  
  -- Find iteration block, if exists.
  findIterBlock :: [(BlockType, Context)] -> Err ()
  findIterBlock [] = fail $ "there is no iteration block declared"
  findIterBlock (block@(blockType, context):blocks) = 
    case blockType of
      IterBlock -> Ok ()
      _         -> findIterBlock blocks
  
  -- Get basic type auxiliary function, needed for lifting one level up the type of an argument (when declaring a function).
  getBasicType :: Type -> BasicType
  getBasicType (TypeBasicType t) = t
  
  --liftTypeToPointer :: BasicType -> Int -> Type
  liftTypeToPointer t 1 = (Pointer t)
  liftTypeToPointer t n = Pointer2Pointer (liftTypeToPointer t (n-1))
  
  -- Get type level.
  getTypeLevel :: Type -> Int
  getTypeLevel t = case t of
    TypeCompoundType (CompoundTypePtr (Pointer _)) -> 1
    TypeCompoundType (CompoundTypePtr (Pointer2Pointer p2p)) -> (getTypeLevel (TypeCompoundType (CompoundTypePtr p2p))) + 1
    TypeCompoundType (CompoundTypeArrayType (ArrDefPtr _ (Pointer _))) -> 1
    TypeCompoundType (CompoundTypeArrayType (ArrDefPtr _ (Pointer2Pointer p2p))) -> (getTypeLevel (TypeCompoundType (CompoundTypePtr p2p))) + 1
    _                            -> 0      
  
  -- Get left expression level.
  getLeftLevel :: LExpr -> Int
  getLeftLevel le = case le of
    LExprRef (LRefExpr le) -> (getLeftLevel le) - 1
    _                      -> 0
  
  -- Get initial level.
  getInitLevel :: Expr -> Int
  getInitLevel e = case e of
    ExprReference le                  -> 1
    ExprLeft (LExprRef (LRefExpr le)) -> (getLeftLevel le) - 1
    _                                 -> 0
  
  -- Get inner type.
  getInnerType :: Type -> Type
  getInnerType t = case t of
    TypeCompoundType (CompoundTypePtr p) -> TypeBasicType $ decomposePtr p
    TypeCompoundType (CompoundTypeArrayType arr) -> TypeBasicType $ decomposeArr arr
    basicType -> basicType
  
  -- Decompose pointer.
  decomposePtr :: Ptr -> BasicType
  decomposePtr p = case p of
    (Pointer t)           -> t
    (Pointer2Pointer p2p) -> decomposePtr p2p
  
  -- Decompose array.
  decomposeArr :: ArrayType -> BasicType
  decomposeArr arr = case arr of
    ArrDefBase pints basicType -> basicType
    ArrDefPtr pints p          -> decomposePtr p
    ArrDefBaseC pints basicType -> basicType
    ArrDefPtrC pints p          -> decomposePtr p
  
  -- Get expression from complex expression.
  getExprFromComplexExpr :: ComplexExpr -> Err Expr
  getExprFromComplexExpr cexpr = case cexpr of
    ExprSimple expr -> Ok expr
    ExprArray _     -> fail $ "cannot get expression from array"
  
  -- Control types of an array.
  controlArrayTypes :: Type -> [(Int, Type)] -> Err Type
  controlArrayTypes t [] = Ok t 
  controlArrayTypes t1 (typ@(i,t2):types) =
    if (getTypeLevel t1) == (i + (getTypeLevel t2)) 
      then 
        if (getInnerType t2) == (getInnerType t1)
          then controlArrayTypes t1 types
          else fail $ printTree t2 
      else 
       if (getTypeLevel t1) < (i + (getTypeLevel t2)) 
         then fail $ printTree t2
         else fail $ printTree t2
  
  -- Infer array type.
  inferArray :: Env -> ComplexExpr -> Type -> Err Type
  inferArray env@(sig, blocks) arr tguard = let types = inferArr env arr
                                            in controlArrayTypes tguard types
  
  -- Infer array.
  inferArr :: Env -> ComplexExpr -> [(Int, Type)] 
  inferArr env arr = do
    case arr of
      ExprArray multiArray@(x:xs) -> inferArr env x ++ inferArrs env xs
      ExprSimple expr -> case inferExpr env expr of 
                           Ok t -> case expr of
                                     ExprReference _ -> [(1, t)]
                                     _               -> [((getInitLevel expr), t)]
                           _    -> []
  
  -- Infer arrays.
  inferArrs :: Env -> [ComplexExpr] -> [(Int, Type)]
  inferArrs _ [] = []
  inferArrs env (x:xs) = do
    case x of
      ExprArray multiArr@(y:ys) -> inferArr env x ++ inferArr env y ++ inferArrs env xs ++ inferArrs env ys
      ExprSimple expr -> case inferExpr env expr of 
                           Ok t -> case expr of 
                                     ExprReference _ -> [(1, t)] ++ inferArrs env xs
                                     _               -> [((getInitLevel expr), t)] ++ inferArrs env xs
                           _    -> []  ++ inferArrs env xs
  
  -- Check array initialization bounds.
  checkArrayInitBounds :: [Int] -> ComplexExpr -> Bool
  checkArrayInitBounds [] _ = True
  checkArrayInitBounds (int:ints) arr =
    case arr of
      ExprArray multiArr@(x:xs) -> (checkLengthArray int multiArr) && 
                                   (checkArrayInitBounds ints x) && 
                                   (checkNestedArrayBounds ((int-1):ints) xs)
      ExprSimple expr -> False
  
  -- Check nested array bounds.
  checkNestedArrayBounds :: [Int] -> [ComplexExpr] -> Bool
  checkNestedArrayBounds _ [] = True
  checkNestedArrayBounds (int:ints) (x:xs) =
    case x of
      ExprArray multiArr@(y:ys) -> (checkLengthArray int (x:xs))  && 
                                   (checkLengthArray (head ints) (y:ys)) && 
                                   (checkArrayInitBounds (tail ints) y) && 
                                   (checkNestedArrayBounds (((head ints)-1):(tail ints)) ys) && 
                                   (checkNestedArrayBounds ((int-1):ints) xs)
      ExprSimple expr -> True
  
  -- Get bounds from PIntegers.
  getBoundsFromPInts :: [PInteger] -> [Int]
  getBoundsFromPInts [] = []
  getBoundsFromPInts ((PInteger (p, int)):xs) = (read int :: Int) : getBoundsFromPInts xs
  
  -- Check length of an array.
  checkLengthArray :: Int -> [ComplexExpr] -> Bool
  checkLengthArray int arr = length arr == int
  
  -- Find left expression name.
  findLExprName :: LExpr -> String
  findLExprName lexpr =
    case lexpr of
      LExprId (PIdent (p,ident))               -> ident
      LExprRef (LRefExpr le)                   -> findLExprName le
      LExprArr (LArrExpr (PIdent (p,ident)) _) -> ident
  
  -- Get bounds from type.
  getBoundsFromType :: Type -> [Int]
  getBoundsFromType t =
    case t of
      TypeCompoundType (CompoundTypeArrayType (ArrDefBase pints _))  -> getBoundsFromPInts pints
      TypeCompoundType (CompoundTypeArrayType (ArrDefBaseC pints _)) -> getBoundsFromPInts pints
      TypeCompoundType (CompoundTypeArrayType (ArrDefPtr pints _))   -> getBoundsFromPInts pints
      TypeCompoundType (CompoundTypeArrayType (ArrDefPtrC pints _))  -> getBoundsFromPInts pints
      _                                                              -> []
  
  -- Get number of arguments from array access.
  getNumberOfArgsFromArrayAccess :: AExpr -> Int
  getNumberOfArgsFromArrayAccess aexpr =
    case aexpr of
      ArrSing _ -> 1
      ArrMul ae _ -> getNumberOfArgsFromArrayAccess ae + 1
  
  -- INTS `areConsistentWithBounds` INTS ?
  areConsistentWithBounds :: [Int] -> [Int] -> Bool
  areConsistentWithBounds [] [] = True
  areConsistentWithBounds _ [] = False
  areConsistentWithBounds [] _ = False
  (x:xs) `areConsistentWithBounds` (y:ys) =
    if x < y then xs `areConsistentWithBounds` ys
             else False
  
  -- Extend environment.
  extendEnv :: (Env, [Program]) -> (BlockType, Context) -> CompStmt -> Err (Env, [Program])
  extendEnv (env@(sig, blocks), prog) blk (StmtBlock decls) = do 
    let env' = ((Map.empty:sig), (blk:blocks))
    (_, p) <- foldM checkDecl (env', prog) decls
    Ok (env, p List.\\ prog) -- pop & list difference (to remove the redundancy) 
  