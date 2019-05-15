module AbsETAC where

newtype Label = Label (String,Integer)
  deriving (Eq, Ord, Show, Read)

-- temporal variables: 't'id_numeber
data Temp = Temp (String,(Int,Int),Type)
  deriving (Eq, Ord, Show, Read)
-- user variables: var_name@line_number
data Var = Var (String,(Int,Int),Type)
  deriving (Eq, Ord, Show, Read)

type TACProgram = [TAC]

-- presenti in AbsE
-- data Type
--  = Bool
--  | Double
--  | Int
--  | Void
--  | Char
--  | String
-- deriving (Eq, Ord, Show, Read)
-- | Compound CompoundType
-- data CompoundType = TypePointer Type | TypeIterable TypeIter
-- deriving (Eq, Ord, Show, Read)
-- data TypeIter = TypeIterInterval Expr | TypeIterArray [Expr]
-- deriving (Eq, Ord, Show, Read)

data TAC 
    = AssignInt   Temp Integer
    | AssignChr   Temp Char
    | AssignBool  Temp Bool
    | AssignFloat Temp Float
    | AssignVar Var Temp
    | AssignTemp Temp Temp
    | AssignV2T Temp Var
    | BinOp Op Temp Temp Temp
    | UnaryOp UnaryOp Temp Temp
    | Goto Label
    | IfNot Temp Label
    | L Label
    | Noop
  deriving (Data, Typeable, Show, Eq)


data Op
  -- Assign
    = OpAssign
  -- Logical
    | OpOr
    | OpAnd
  -- Arithmetic
    | OpPlus
    | OpMinus
    | OpMul
    | OpIntDiv
    | OpFloatDiv
    | OpRemainder
    | OpModulo
    | OpPower
  deriving (Eq, Ord, Show, Read)

data UnaryOp
-- Assign
  = UOpMinus
-- Logical
  | UOpNegate
deriving (Eq, Ord, Show, Read)

instance Show Op where
  show op = case op of
                BOpSub -> "-"
                BOpAdd -> "+"
                BOpMul -> "*"
                BOpLTE -> "<="
                BOpEq  -> "=="
                BOpAnd -> "&&"
                BOpOR  -> "||"

instance Show UnaryOp where
    show op = case op of
                  UOpMinus -> "-"
                  UOpNegate -> "!"