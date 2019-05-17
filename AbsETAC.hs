module AbsETAC where

  
  newtype PIdent = PIdent ((Int,Int),String)
    deriving (Eq, Ord, Show, Read)
  
  data Program = PDefs [Decl] | TACProgram [TAC]
    deriving (Eq, Ord, Read)
  
  data Decl
      = TypedDecl AnnotatedDecl
      | DeclFun LExpr [Arg] Guard CompStmt
      | DeclStmt Stmt
    deriving (Eq, Ord, Show, Read)
  
  data AnnotatedDecl = ADecl Type Decl
    deriving (Eq, Ord, Show, Read)
  
  data Arg = ArgDecl Modality PIdent Guard
    deriving (Eq, Ord, Show, Read)
  
  data Modality = ModEmpty | ModVar | ModDef
    deriving (Eq, Ord, Show, Read)
  
  data Guard = GuardVoid | GuardType Type
    deriving (Eq, Ord, Show, Read)
  
  data Stmt
      = StmtExpr Expr
      | StmtDecl LExpr Guard
      | StmtInit LExpr Guard Expr
      | StmtVoidIterDecl LExpr Guard
      | StmtIterDecl Expr LExpr Guard
      | StmtArrDecl LExpr Guard Array
      | StmtDeclD LExpr Guard
      | StmtInitD LExpr Guard Expr
      | StmtVoidIterDeclD LExpr Guard
      | StmtIterDeclD Expr LExpr Guard
      | StmtArrDeclD LExpr Guard Array
      | StmtReturn Expr
      | StmtNoReturn
      | SComp CompStmt
      | StmtIfThenElse Expr CompStmt CompStmt
      | StmtIfThen Expr CompStmt
      | SSwitchCase Expr [NormCase] [DfltCase]
      | StmtBreak
      | StmtContinue
      | StmtWhile Expr CompStmt
      | StmtFor PIdent Array CompStmt
    deriving (Eq, Ord, Show, Read)
  
  data CompStmt = StmtBlock [Decl]
    deriving (Eq, Ord, Show, Read)
  
  data NormCase = CaseNormal Expr CompStmt
    deriving (Eq, Ord, Show, Read)
  
  data DfltCase = CaseDefault CompStmt
    deriving (Eq, Ord, Show, Read)
  
  data Expr
      = StmtAssign LExpr AssignOperator Expr
      | LeftExpr LExpr
      | ExprInt Integer
      | ExprDouble Double
      | ExprChar Char
      | ExprString String
      | ExprTrue
      | ExprFalse
      | ExprFunCall PIdent [Expr]
      | ExprBoolNot Expr
      | ExprNegation Expr
      | ExprAddition Expr
      | ExprPower Expr Expr
      | ExprMul Expr Expr
      | ExprFloatDiv Expr Expr
      | ExprIntDiv Expr Expr
      | ExprReminder Expr Expr
      | ExprModulo Expr Expr
      | ExprPlus Expr Expr
      | ExprMinus Expr Expr
      | ExprLt Expr Expr
      | ExprGt Expr Expr
      | ExprLtEq Expr Expr
      | ExprGtEq Expr Expr
      | ExprEq Expr Expr
      | ExprNeq Expr Expr
      | ExprAnd Expr Expr
      | ExprOr Expr Expr
    deriving (Eq, Ord, Show, Read)
  
  data LExpr
      = LExprId PIdent | LExprDeref Deref | LExprRef Ref | LExprArr Arr
    deriving (Eq, Ord, Show, Read)
  
  data Deref = LDerefExpr LExpr
    deriving (Eq, Ord, Show, Read)
  
  data Ref = LRefExpr LExpr
    deriving (Eq, Ord, Show, Read)
  
  data Arr = LArrExpr LExpr Expr
    deriving (Eq, Ord, Show, Read)
  
  data AssignOperator
      = OpAssign
      | OpOr
      | OpAnd
      | OpPlus
      | OpMinus
      | OpMul
      | OpIntDiv
      | OpFloatDiv
      | OpRemainder
      | OpModulo
      | OpPower
    deriving (Eq, Ord, Show, Read)
  
  data Type
      = TypeBool
      | TypeDouble
      | TypeInt
      | TypeVoid
      | TypeChar
      | TypeString
      | TypeCompound CompoundType
    deriving (Eq, Ord, Show, Read)
  
  data CompoundType = TypePointer Type | TypeArray Array
    deriving (Eq, Ord, Show, Read)
  
  data Array = TypeMultiArray [Array]
    deriving (Eq, Ord, Show, Read)
  
  data Label = Label (String,Integer)
    deriving (Eq, Ord, Show, Read)
  
  -- temporal variables: 't'id_numeber
  data Temp = Temp (Int,Type)
    deriving (Eq, Ord, Read)
  -- user variables: var_name@line_number
  data Var  = Var  (String,(Int,Int),Type)
    deriving (Eq, Ord, Read)
  
  data TAC 
    = AssignIntTemp   Temp Integer
    | AssignChrTemp   Temp Char
    | AssignBoolTemp  Temp Bool
    | AssignFloatTemp Temp Float
  
    | AssignIntVar    Var  Integer
    | AssignChrVar    Var  Char
    | AssignBoolVar   Var  Bool
    | AssignFloatVar  Var  Float
  
    | AssignT2V Var Temp
    | AssignT2T Temp Temp
    | AssignV2T Temp Var
  
    | BinOp BinaryOperator Temp Temp Temp
    | UnaryOp UnaryOp Temp Temp
    | Goto Label
    | IfNot Temp Label
    | L Label
    | Noop
    deriving (Eq, Ord, Read)
  
  data BinaryOperator
    = BOpAssign
    | BOpOr
    | BOpAnd
    | BOpPlus
    | BOpMinus
    | BOpMul
    | BOpIntDiv
    | BOpFloatDiv
    | BOpRemainder
    | BOpModulo
    | BOpPower
    deriving (Eq, Ord, Show, Read)
  
  data UnaryOp
  -- Assign
    = UOpMinus
  -- Logical
    | UOpNegate
    deriving (Eq, Ord, Show, Read)