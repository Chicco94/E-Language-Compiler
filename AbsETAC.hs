module AbsETAC where

import AbsE

newtype Label = Label (String,Integer)
  deriving (Eq, Ord, Show, Read)

-- temporal variables: 't'id_numeber
data Temp = Temp (String,(Int,Int),Type)
  deriving (Eq, Ord, Show, Read)
-- user variables: var_name@line_number
data Var  = Var  (String,(Int,Int),Type)
  deriving (Eq, Ord, Show, Read)

data TACProgram = TACProgram [TAC]
  deriving (Eq, Ord, Show, Read)

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

    | BinOp Op Temp Temp Temp
    | UnaryOp UnaryOp Temp Temp
    | Goto Label
    | IfNot Temp Label
    | L Label
    | Noop
  deriving (Eq, Ord, Show, Read)


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