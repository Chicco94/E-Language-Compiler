module PrintETAC where


import AbsETAC
import Data.Char


instance Show Program where
  show p = case p of
    --PDefs decls -> prPrec i 0 (concatD [prt 0 decls])
    TACProgram [] -> ""
    TACProgram tacs -> (show tacs)

instance Show TAC where
  show t = case t of
    {-mettere \n dopo return-}
    FuncDef (Var (name,pos@(row,col),type_)) -> "\n"++ filter (/='\"') (show name) ++ "@"++ show row ++ "-"++show col ++"\tFunction:\n\tType: " ++ show type_ ++ "\n\tStatements:\n"
    Return  temp                   -> "\treturn_" ++ show temp ++"\n\tEnd Function\n\n"
    FuncCall (Var (name,pos@(row,col),type_)) temp-> "\t" ++ show temp ++ " = " ++ show type_ ++ " call " ++ filter (/='\"') (show name) ++ "@"++ show row ++ "-"++show col++"\n"


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
            
    AssignT2V       var  temp            -> "\t" ++ show var  ++ " = " ++ show temp ++ "\n"
    AssignT2T       tmp1 tmp2            -> "\t" ++ show tmp1 ++ " = " ++ show tmp2 ++ "\n"
    AssignV2T       temp var             -> "\t" ++ show temp ++ " = " ++ show var  ++ "\n"
    AssignV2V       var1 var2            -> "\t" ++ show var1 ++ " = " ++ show var2 ++ "\n"
    AssignT2P       temp                 -> "\t" ++ "param_"  ++ show temp ++ "\n"
    
    BinOp BOpOr       tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " || " ++ show temp2 ++ "\n"
    BinOp BOpAnd      tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " && " ++ show temp2 ++ "\n"

    BinOp BOpLt       tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " less_than "           ++ show temp2 ++ "\n"
    BinOp BOpGt       tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " greater_than "        ++ show temp2 ++ "\n"
    BinOp BOpLtEq     tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " less_equal_than "     ++ show temp2 ++ "\n"
    BinOp BOpGtEq     tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " greater_equal_than "  ++ show temp2 ++ "\n"
    BinOp BOpEq       tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " equal "               ++ show temp2 ++ "\n"
    BinOp BOpNeq      tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " not_equal "           ++ show temp2 ++ "\n"

    BinOp BOpPlus      tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " + " ++ show temp2  ++ "\n"
    BinOp BOpMinus     tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " - " ++ show temp2  ++ "\n"
    BinOp BOpMul       tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " * " ++ show temp2  ++ "\n"
    BinOp BOpIntDiv    tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " // " ++ show temp2 ++ "\n"
    BinOp BOpFloatDiv  tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " / " ++ show temp2  ++ "\n"
    BinOp BOpRemainder tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " % " ++ show temp2  ++ "\n"
    BinOp BOpModulo    tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " %% " ++ show temp2 ++ "\n"
    BinOp BOpPower     tempr temp1 temp2 -> "\t" ++ show tempr ++ " = " ++ show temp1 ++ " ^ " ++ show temp2  ++ "\n"

    Goto label -> "\tgoto " ++ show label  ++ "\n"
    Lbl label -> show label
    If      bexpr label ->"\tif "      ++ show bexpr ++ " goto "++show label++"\n"
    IfFalse bexpr label ->"\tifFalse " ++ show bexpr ++ " goto "++show label++"\n"
      --prPrec i 0 (concatD [prt 0 var, doc (showString "="),prt 0 integer, doc (showString "\n")])
    _ -> "comando non trovato\n" -- prPrec i 0 (concatD [doc (showString "_")]) --TODO

instance Show Var where --Var = (String,(Int,Int),Type)
  show (Var (name,pos@(row,col),type_)) = show type_ ++ "  "++   filter (/='\"') (show name) ++ "@"++ show row ++ "-"++show col
  --prt i (Var (name,pos@(row,col),type_)) = prPrec i 0 (concatD [prt 0 type_,prt 0 name, doc (showString "@"),prt 0 row, doc (showString ","),prt 0 col])

instance Show Temp where --Temp = (Int,Type)
  show (Temp (num,type_)) = show type_ ++ "  t" ++ show num
  --prt i (Temp (num,type_)) = prPrec i 0 (concatD [prt 0 type_,doc (showString "t"),prt 0 num])
  
instance Show Label where --Label = (Int,Type)
  show (Label (name,id)) = filter (/='\"') (show name) ++ "@" ++ show id
--prt i (Temp (num,type_)) = prPrec i 0 (concatD [prt 0 type_,doc (showString "t"),prt 0 num])