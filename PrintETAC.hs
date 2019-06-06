module PrintETAC where


import AbsETAC
import AbsE


instance Show TAC where
  show t = case t of
    {-mettere \n dopo return-}
    FuncDef (Var (name,pos@(row,col),type_))         -> "\n\n"++ filter (/='\"') (show name) ++ "@"++ show row ++ "-"++show col ++"\tFunction:\n\tType: " ++ show type_ ++ "\n\tStatements:\n"
    Return  temp                                     -> "\treturn_" ++ show temp ++"\n"
    EndFunction                                      -> "\tEnd Function\n\n"
    FuncCall (Var ("main",pos@(row,col),type_)) temp -> "\tcall main@"++ show row ++ "-"++show col++"\n\texit\n"
    FuncCall (Var (name,pos@(row,col),type_))   temp -> "\t" ++ show temp ++ " = " ++ show type_  ++ " call " ++ filter (/='\"') (show name) ++ "@"++ show row ++ "-"++show col++"\n"


    AssignIntVar    var  val             -> "\t" ++ show var   ++ " = int "    ++ show val ++ "\n"
    AssignChrVar    var  val             -> "\t" ++ show var   ++ " = char "   ++ show val ++ "\n"
    AssignStrVar    var  val             -> "\t" ++ show var   ++ " = string " ++ show val ++ "\n"
    AssignTrueVar   var  val             -> "\t" ++ show var   ++ " = bool "   ++ show val ++ "\n"
    AssignFalseVar  var  val             -> "\t" ++ show var   ++ " = bool "   ++ show val ++ "\n"
    AssignFloatVar  var  val             -> "\t" ++ show var   ++ " = float "  ++ show val ++ "\n"
              
    AssignIntTemp   temp val             -> "\t" ++ show temp  ++ " = int "    ++ show val ++ "\n"
    AssignChrTemp   temp val             -> "\t" ++ show temp  ++ " = char "   ++ show val ++ "\n"
    AssignStrTemp   temp val             -> "\t" ++ show temp  ++ " = string " ++ show val ++ "\n"
    AssignTrueTemp  temp val             -> "\t" ++ show temp  ++ " = bool "   ++ show val ++ "\n"
    AssignFalseTemp temp val             -> "\t" ++ show temp  ++ " = bool "   ++ show val ++ "\n"
    AssignFloatTemp temp val             -> "\t" ++ show temp  ++ " = float "  ++ show val ++ "\n"
            
    AssignT2V   var  temp (Temp (pos,(TypeBasicType TypeVoid))) -> "\t" ++ show var  ++ (if pos /= -1 then "[" ++ show pos ++ "] = " else " = ") ++ show temp ++ "\n"
    AssignT2V   var  temp (Temp (pos,_)) -> "\t" ++ show var  ++ (if pos /= -1 then "[t" ++ show pos ++ "] = " else " = ") ++ show temp ++ "\n"
    AssignT2T   tmp1 tmp2                -> "\t" ++ show tmp1 ++ " = " ++ show tmp2 ++ "\n"
    AssignV2T   temp var  (Temp (pos,t)) -> "\t" ++ show temp ++ " = " ++ show var  ++ (if pos /= -1 then "[t" ++ show pos ++ "]\n" else "\n")
    AssignT2P   temp                     -> "\t" ++ "param_"  ++ show temp ++ "\n"
    
    BinOp BOpPlus      tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " + " ++ show temp2  ++ "\n"
    BinOp BOpMinus     tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " - " ++ show temp2  ++ "\n"
    BinOp BOpMul       tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " * " ++ show temp2  ++ "\n"
    BinOp BOpIntDiv    tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " // " ++ show temp2 ++ "\n"
    BinOp BOpFloatDiv  tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " / " ++ show temp2  ++ "\n"
    BinOp BOpRemainder tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " % " ++ show temp2  ++ "\n"
    BinOp BOpModulo    tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " %% " ++ show temp2 ++ "\n"
    BinOp BOpPower     tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " **" ++ show temp2  ++ "\n"

    UnaryOp UOpMinus         temp1 temp2 -> "\t" ++ show temp1 ++ " = 0 - "  ++ show temp2 ++ "\n"       
    UnaryOp UOpPlus          temp1 temp2 -> "\t" ++ show temp1 ++ " = 0 + "  ++ show temp2 ++ "\n"
    UnaryOp UOpNegate        temp1 temp2 -> "\t" ++ show temp1 ++ " = not "  ++ show temp2 ++ "\n"
    DerefOp UOpDeref         temp1 var   -> "\t" ++ show temp1 ++ " = addr_" ++ show var   ++ "\n"

    BoolOp BOpOr             temp1 temp2 -> show temp1 ++ " or "                  ++ show temp2
    BoolOp BOpAnd            temp1 temp2 -> show temp1 ++ " and "                 ++ show temp2
    BoolOp BOpLt             temp1 temp2 -> show temp1 ++ " less_than "           ++ show temp2
    BoolOp BOpGt             temp1 temp2 -> show temp1 ++ " greater_than "        ++ show temp2
    BoolOp BOpLtEq           temp1 temp2 -> show temp1 ++ " less_equal_than "     ++ show temp2
    BoolOp BOpGtEq           temp1 temp2 -> show temp1 ++ " greater_equal_than "  ++ show temp2
    BoolOp BOpEq             temp1 temp2 -> show temp1 ++ " equal "               ++ show temp2
    BoolOp BOpNeq            temp1 temp2 -> show temp1 ++ " not_equal "           ++ show temp2

    Goto label                           -> "\tgoto " ++ show label  ++ "\n"
    Lbl  label                           -> show label
    If      bexpr label                  ->"\tif "      ++ show bexpr ++ " goto "++show label++"\n"
    IfFalse bexpr label                  ->"\tifFalse " ++ show bexpr ++ " goto "++show label++"\n"
    _ -> "comando non trovato\n"

instance Show Var where --Var = (String,(Int,Int),TACType)
  show (Var (name,pos@(row,col),type_)) = show type_ ++ "  "++   filter (/='\"') (show name) ++ "@"++ show row ++ "-"++show col

instance Show Temp where --Temp = Temp (Int,TACType) | TempT PTrue | TempF PFalse
  show temp  = case temp of
    (Temp (num,type_)) -> show type_ ++ " t" ++ show num
    (TempT _) -> show TypeBool ++ "  true"
    (TempF _) -> show TypeBool ++ "  false"
  
instance Show Label where --Label = (Int,TACType)
  show (Label (name,id)) = filter (/='\"') (show name) ++ "@" ++ show id

-- DA IMPORTARE IN ABSE.HS DOPO CDM -> BNFC E.CF
{-
instance Show PTrue     where show (PTrue     (_, val)) = filter (/='\"') (show val)
instance Show PFalse    where show (PFalse    (_, val)) = filter (/='\"') (show val)
instance Show PReturn   where show (PReturn   (_, val)) = filter (/='\"') (show val)
instance Show PContinue where show (PContinue (_, val)) = filter (/='\"') (show val)
instance Show PBreak    where show (PBreak    (_, val)) = filter (/='\"') (show val)
instance Show PIdent    where show (PIdent    (_, val)) = filter (/='\"') (show val)
instance Show PInteger  where show (PInteger  (_, val)) = filter (/='\"') (show val)
instance Show PFloat    where show (PFloat    (_, val)) = filter (/='\"') (show val)
instance Show PChar     where show (PChar     (_, val)) = filter (/='\"') (show val)
instance Show PString   where show (PString   (_, val)) = "\"" ++ filter (/='\\') (filter (/='\"') (show val) ) ++ "\"" 

instance Show Type where 
  show e = case e of 
    TypeBasicType    t -> show t
    TypeCompoundType c -> show c

instance Show BasicType where 
  show t = case t of 
    TypeBool   -> "bool"
    TypeFloat  -> "float"
    TypeInt    -> "int"
    TypeVoid   -> "void"
    TypeChar   -> "char"
    TypeString -> "string"

instance Show CompoundType where 
  show t = case t of 
    CompoundTypeArrayType a -> show a
    CompoundTypePtr       p -> show p

instance Show ArrayType where
  show t = case t of
    ArrDefBase dim bt -> "[]" ++ show bt
    ArrDefPtr  dim p  -> "[]" ++ show p

instance Show Ptr where
  show t = case t of
    Pointer         bt -> show bt ++ "*"
    Pointer2Pointer p  -> show p  ++ "*"
    -}