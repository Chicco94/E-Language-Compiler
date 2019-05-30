module AbsETAC where


import AbsE
{- da importare dopo bnfc in abse in testa-}
instance Show PTrue     where show (PTrue     (_, val)) = filter (/='\"') (show val)
instance Show PFalse    where show (PFalse    (_, val)) = filter (/='\"') (show val)
instance Show PReturn   where show (PReturn   (_, val)) = filter (/='\"') (show val)
instance Show PContinue where show (PContinue (_, val)) = filter (/='\"') (show val)
instance Show PBreak    where show (PBreak    (_, val)) = filter (/='\"') (show val)
instance Show PIdent    where show (PIdent    (_, val)) = filter (/='\"') (show val)
instance Show PInteger  where show (PInteger  (_, val)) = filter (/='\"') (show val)
instance Show PFloat    where show (PFloat    (_, val)) = filter (/='\"') (show val)
instance Show PChar     where show (PChar     (_, val)) = filter (/='\"') (show val)
instance Show PString   where show (PString   (_, val)) =                 (show val)

instance Show BasicType where 
  show t = case t of 
    TypeBool   -> "bool"
    TypeFloat  -> "float"
    TypeInt    -> "int"
    TypeVoid   -> "void"
    TypeChar   -> "char"
    TypeString -> "string"

newtype PTrue = PTrue ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PFalse = PFalse ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PReturn = PReturn ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PContinue = PContinue ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PBreak = PBreak ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PIdent = PIdent ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PInteger = PInteger ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PFloat = PFloat ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PChar = PChar ((Int,Int),String)
  deriving (Eq, Ord, Read)
newtype PString = PString ((Int,Int),String)
  deriving (Eq, Ord, Read)
  
data Program = PDefs [Decl] | TACProgram [TAC]
  deriving (Eq, Ord, Read)


-- da importare in coda

data Label = Label (String,Int)
  deriving (Eq, Ord, Read)

-- temporal variables: 't'id_numeber
data Temp = Temp (Int,Type)
  deriving (Eq, Ord, Read)
-- user variables: var_name@line_number
data Var  = Var  (String,(Int,Int),Type)
  deriving (Eq, Ord, Read)

data TAC 
  = AssignIntTemp   Temp PInteger
  | AssignChrTemp   Temp PChar
  | AssignStrTemp   Temp PString
  | AssignTrueTemp  Temp PTrue
  | AssignFalseTemp Temp PFalse
  | AssignFloatTemp Temp PFloat

  | AssignIntVar    Var  PInteger
  | AssignChrVar    Var  PChar
  | AssignStrVar    Var  PString
  | AssignTrueVar   Var  PTrue
  | AssignFalseVar  Var  PFalse
  | AssignFloatVar  Var  PFloat

  | AssignT2V Var  Temp
  | AssignT2T Temp Temp
  | AssignV2T Temp Var
  | AssignV2V Var  Var
  | AssignT2P Temp

  | BinOp BinaryOperator Temp Temp Temp
  | UnaryOp UnaryOp Temp Temp

  | FuncDef Var
  | FuncCall Var Temp
  | Return Temp
  | Goto Label
  | Lbl Label
  | If Temp Label
  | IfFalse Temp Label
  deriving (Eq, Ord, Read)

data BinaryOperator
  = BOpAssign
  | BOpOr
  | BOpAnd
  
  | BOpLt
  | BOpGt
  | BOpLtEq
  | BOpGtEq
  | BOpEq
  | BOpNeq

  | BOpPlus
  | BOpMinus
  | BOpMul
  | BOpIntDiv
  | BOpFloatDiv
  | BOpRemainder
  | BOpModulo
  | BOpPower
  deriving (Eq, Ord, Read)

data UnaryOp
-- Assign
  = UOpMinus
-- Logical
  | UOpNegate
  deriving (Eq, Ord, Show, Read)