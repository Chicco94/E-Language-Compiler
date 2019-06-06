

module AbsETAC where

  
import AbsE

data Label = Label (String,Int)
  deriving (Eq, Ord, Read)

-- temporal variables: 't'id_numeber
data Temp = Temp (Int,Type) | TempT PTrue | TempF PFalse
  deriving (Eq, Ord, Read)
-- user variables: var_name@line_number
data Var  = Var  (String,(Int,Int),Type)
  deriving (Eq, Ord, Read)

-- istruzioni TAC  
data TAC 
  -- assegnamento di valore a temporaneo
  = AssignIntTemp   Temp PInteger
  | AssignChrTemp   Temp PChar
  | AssignStrTemp   Temp PString
  | AssignTrueTemp  Temp PTrue
  | AssignFalseTemp Temp PFalse
  | AssignFloatTemp Temp PFloat

-- assegnamento di valore a variabile
  | AssignIntVar    Var  PInteger
  | AssignChrVar    Var  PChar
  | AssignStrVar    Var  PString
  | AssignTrueVar   Var  PTrue
  | AssignFalseVar  Var  PFalse
  | AssignFloatVar  Var  PFloat

-- assegnamenti tra temporanei e variabili
  | AssignT2V Var  Temp  Temp
  | AssignT2T Temp Temp
  | AssignV2T Temp Var   Temp
  | AssignT2P Temp

-- operazioni varie
  | BinOp  BinaryOperator Temp Temp Temp
  | UnaryOp UnaryOperator Temp Temp
  | DerefOp UnaryOperator Temp Var
  | BoolOp BinaryOperator Temp Temp

-- funzioni
  | FuncDef Var
  | FuncCall Var Temp
  | Return Temp
  | EndFunction
-- salti
  | Goto Label
  | Lbl Label

-- try_catch
  | OnException Label

-- condizioni
  | If TAC Label
  | IfFalse TAC Label
-- ammetto qualunque TAC, tanto il controllo che sia un operazione booleana
-- è stato già fatto

-- placeholder
  | Empty
  deriving (Eq, Ord, Read)

-- operatori binari
data BinaryOperator
  = BOpAssign
  | BOpPlus
  | BOpMinus
  | BOpMul
  | BOpIntDiv
  | BOpFloatDiv
  | BOpRemainder
  | BOpModulo
  | BOpPower

  | BOpOr
  | BOpAnd
  
  | BOpLt
  | BOpGt
  | BOpLtEq
  | BOpGtEq
  | BOpEq
  | BOpNeq
  deriving (Eq, Ord, Read)

data UnaryOperator
-- Assign
  = UOpMinus
  | UOpPlus
-- Logical
  | UOpNegate
-- Address
  | UOpDeref
  deriving (Eq, Ord, Show, Read)