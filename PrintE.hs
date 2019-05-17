{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintE where

-- pretty-printer generated by the BNF converter

import AbsE
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)

instance Print Int where
    prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)



instance Print PIdent where
  prt _ (PIdent (_,i)) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
    PDefs decls -> prPrec i 0 (concatD [prt 0 decls])

{-
instance Print TAC where
  prt i e = case e of
    AssignIntVar   var  integer -> prPrec i 0 (concatD [prt 0 var , doc (showString "="),prt 0 integer, doc (showString "\n")])
    AssignIntTemp  temp integer -> prPrec i 0 (concatD [prt 0 temp, doc (showString "="),prt 0 integer, doc (showString "\n")])
    BinOp BOpPlus  tempr temp1 temp2 -> prPrec i 0 (concatD [prt 0 tempr, doc (showString "="),prt 0 temp1,doc (showString "+"),prt 0 temp2, doc (showString "\n")])
    BinOp BOpMinus tempr temp1 temp2 -> prPrec i 0 (concatD [prt 0 tempr, doc (showString "="),prt 0 temp1,doc (showString "-"),prt 0 temp2, doc (showString "\n")])
    _ -> prPrec i 0 (concatD [doc (showString "_\n")]) --TODO

instance Print Var where --Var = (String,(Int,Int),Type)
  prt i (Var (name,pos@(row,col),type_)) = prPrec i 0 (concatD [prt 0 type_,prt 0 name, doc (showString "@"),prt 0 row, doc (showString ","),prt 0 col])

instance Print Temp where --Var = (String,(Int,Int),Type)
  prt i (Temp (num,type_)) = prPrec i 0 (concatD [prt 0 type_,doc (showString "t"),prt 0 num])
-}
instance Print Decl where
  prt i e = case e of
    TypedDecl annotateddecl -> prPrec i 0 (concatD [prt 0 annotateddecl])
    DeclFun lexpr args guard compstmt -> prPrec i 0 (concatD [doc (showString "def"), prt 0 lexpr, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 guard, prt 0 compstmt])
    DeclStmt stmt -> prPrec i 0 (concatD [prt 0 stmt])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print AnnotatedDecl where
  prt i e = case e of
    ADecl type_ decl -> prPrec i 0 (concatD [doc (showString "["), prt 0 type_, doc (showString ":]"), prt 0 decl])

instance Print Arg where
  prt i e = case e of
    ArgDecl modality pident guard -> prPrec i 0 (concatD [prt 0 modality, prt 0 pident, prt 0 guard])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Modality where
  prt i e = case e of
    ModEmpty -> prPrec i 0 (concatD [])
    ModVar -> prPrec i 0 (concatD [doc (showString "var")])
    ModDef -> prPrec i 0 (concatD [doc (showString "def")])

instance Print Guard where
  prt i e = case e of
    GuardVoid -> prPrec i 0 (concatD [])
    GuardType type_ -> prPrec i 0 (concatD [doc (showString ":"), prt 0 type_])

instance Print Stmt where
  prt i e = case e of
    StmtExpr expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
    StmtDecl lexpr guard -> prPrec i 0 (concatD [doc (showString "var"), prt 0 lexpr, prt 0 guard, doc (showString ";")])
    StmtInit lexpr guard expr -> prPrec i 0 (concatD [doc (showString "var"), prt 0 lexpr, prt 0 guard, doc (showString ":="), prt 0 expr, doc (showString ";")])
    StmtVoidIterDecl lexpr guard -> prPrec i 0 (concatD [doc (showString "var"), doc (showString "["), doc (showString "]"), prt 0 lexpr, prt 0 guard, doc (showString ";")])
    StmtIterDecl expr lexpr guard -> prPrec i 0 (concatD [doc (showString "var"), doc (showString "["), prt 0 expr, doc (showString "]"), prt 0 lexpr, prt 0 guard, doc (showString ";")])
    StmtArrDecl lexpr guard array -> prPrec i 0 (concatD [doc (showString "var"), prt 0 lexpr, prt 0 guard, doc (showString ":="), prt 0 array, doc (showString ";")])
    StmtDeclD lexpr guard -> prPrec i 0 (concatD [doc (showString "def"), prt 0 lexpr, prt 0 guard, doc (showString ";")])
    StmtInitD lexpr guard expr -> prPrec i 0 (concatD [doc (showString "def"), prt 0 lexpr, prt 0 guard, doc (showString ":="), prt 0 expr, doc (showString ";")])
    StmtVoidIterDeclD lexpr guard -> prPrec i 0 (concatD [doc (showString "def"), doc (showString "["), doc (showString "]"), prt 0 lexpr, prt 0 guard, doc (showString ";")])
    StmtIterDeclD expr lexpr guard -> prPrec i 0 (concatD [doc (showString "def"), doc (showString "["), prt 0 expr, doc (showString "]"), prt 0 lexpr, prt 0 guard, doc (showString ";")])
    StmtArrDeclD lexpr guard array -> prPrec i 0 (concatD [doc (showString "def"), prt 0 lexpr, prt 0 guard, doc (showString ":="), prt 0 array, doc (showString ";")])
    StmtReturn expr -> prPrec i 0 (concatD [doc (showString "return"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString ";")])
    StmtNoReturn -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    SComp compstmt -> prPrec i 0 (concatD [prt 0 compstmt])
    StmtIfThenElse expr compstmt1 compstmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 compstmt1, doc (showString "else"), prt 0 compstmt2])
    StmtIfThen expr compstmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 compstmt])
    SSwitchCase expr normcases dfltcases -> prPrec i 0 (concatD [doc (showString "switch"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 normcases, prt 0 dfltcases, doc (showString "}")])
    StmtBreak -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    StmtContinue -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    StmtWhile expr compstmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 compstmt])
    StmtFor pident array compstmt -> prPrec i 0 (concatD [doc (showString "for"), prt 0 pident, doc (showString "in"), prt 0 array, prt 0 compstmt])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print CompStmt where
  prt i e = case e of
    StmtBlock decls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 decls, doc (showString "}")])

instance Print NormCase where
  prt i e = case e of
    CaseNormal expr compstmt -> prPrec i 0 (concatD [doc (showString "match"), prt 0 expr, prt 0 compstmt])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print DfltCase where
  prt i e = case e of
    CaseDefault compstmt -> prPrec i 0 (concatD [doc (showString "match _"), prt 0 compstmt])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Expr where
  prt i e = case e of
    StmtAssign lexpr assignoperator expr -> prPrec i 0 (concatD [prt 0 lexpr, prt 0 assignoperator, prt 1 expr])
    LeftExpr lexpr -> prPrec i 17 (concatD [prt 0 lexpr])
    ExprInt n -> prPrec i 16 (concatD [prt 0 n])
    ExprDouble d -> prPrec i 16 (concatD [prt 0 d])
    ExprChar c -> prPrec i 16 (concatD [prt 0 c])
    ExprString str -> prPrec i 16 (concatD [prt 0 str])
    ExprTrue -> prPrec i 16 (concatD [doc (showString "true")])
    ExprFalse -> prPrec i 16 (concatD [doc (showString "false")])
    ExprFunCall pident exprs -> prPrec i 15 (concatD [prt 0 pident, doc (showString "("), prt 0 exprs, doc (showString ")")])
    ExprBoolNot expr -> prPrec i 14 (concatD [doc (showString "!"), prt 15 expr])
    ExprNegation expr -> prPrec i 14 (concatD [doc (showString "-"), prt 15 expr])
    ExprAddition expr -> prPrec i 14 (concatD [doc (showString "+"), prt 15 expr])
    ExprPower expr1 expr2 -> prPrec i 13 (concatD [prt 14 expr1, doc (showString "^"), prt 13 expr2])
    ExprMul expr1 expr2 -> prPrec i 12 (concatD [prt 12 expr1, doc (showString "*"), prt 13 expr2])
    ExprFloatDiv expr1 expr2 -> prPrec i 12 (concatD [prt 12 expr1, doc (showString "/"), prt 13 expr2])
    ExprIntDiv expr1 expr2 -> prPrec i 12 (concatD [prt 12 expr1, doc (showString "//"), prt 13 expr2])
    ExprReminder expr1 expr2 -> prPrec i 12 (concatD [prt 12 expr1, doc (showString "%"), prt 13 expr2])
    ExprModulo expr1 expr2 -> prPrec i 12 (concatD [prt 12 expr1, doc (showString "%%"), prt 13 expr2])
    ExprPlus expr1 expr2 -> prPrec i 11 (concatD [prt 11 expr1, doc (showString "+"), prt 12 expr2])
    ExprMinus expr1 expr2 -> prPrec i 11 (concatD [prt 11 expr1, doc (showString "-"), prt 12 expr2])
    ExprLt expr1 expr2 -> prPrec i 9 (concatD [prt 9 expr1, doc (showString "<"), prt 10 expr2])
    ExprGt expr1 expr2 -> prPrec i 9 (concatD [prt 9 expr1, doc (showString ">"), prt 10 expr2])
    ExprLtEq expr1 expr2 -> prPrec i 9 (concatD [prt 9 expr1, doc (showString "<="), prt 10 expr2])
    ExprGtEq expr1 expr2 -> prPrec i 9 (concatD [prt 9 expr1, doc (showString ">="), prt 10 expr2])
    ExprEq expr1 expr2 -> prPrec i 8 (concatD [prt 8 expr1, doc (showString "=="), prt 9 expr2])
    ExprNeq expr1 expr2 -> prPrec i 8 (concatD [prt 8 expr1, doc (showString "!="), prt 9 expr2])
    ExprAnd expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "&&"), prt 5 expr2])
    ExprOr expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "||"), prt 4 expr2])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print LExpr where
  prt i e = case e of
    LExprId pident -> prPrec i 0 (concatD [prt 0 pident])
    LExprDeref deref -> prPrec i 0 (concatD [prt 0 deref])
    LExprRef ref -> prPrec i 0 (concatD [prt 0 ref])
    LExprArr arr -> prPrec i 0 (concatD [prt 0 arr])

instance Print Deref where
  prt i e = case e of
    LDerefExpr lexpr -> prPrec i 0 (concatD [doc (showString "&"), prt 0 lexpr])

instance Print Ref where
  prt i e = case e of
    LRefExpr lexpr -> prPrec i 0 (concatD [doc (showString "*"), prt 0 lexpr])

instance Print Arr where
  prt i e = case e of
    LArrExpr lexpr expr -> prPrec i 0 (concatD [prt 0 lexpr, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print AssignOperator where
  prt i e = case e of
    OpAssign -> prPrec i 0 (concatD [doc (showString ":=")])
    OpOr -> prPrec i 0 (concatD [doc (showString "|=")])
    OpAnd -> prPrec i 0 (concatD [doc (showString "&=")])
    OpPlus -> prPrec i 0 (concatD [doc (showString "+=")])
    OpMinus -> prPrec i 0 (concatD [doc (showString "-=")])
    OpMul -> prPrec i 0 (concatD [doc (showString "*=")])
    OpIntDiv -> prPrec i 0 (concatD [doc (showString "//=")])
    OpFloatDiv -> prPrec i 0 (concatD [doc (showString "/=")])
    OpRemainder -> prPrec i 0 (concatD [doc (showString "%=")])
    OpModulo -> prPrec i 0 (concatD [doc (showString "%%=")])
    OpPower -> prPrec i 0 (concatD [doc (showString "^=")])

instance Print Type where
  prt i e = case e of
    TypeBool -> prPrec i 0 (concatD [doc (showString "bool")])
    TypeDouble -> prPrec i 0 (concatD [doc (showString "double")])
    TypeInt -> prPrec i 0 (concatD [doc (showString "int")])
    TypeVoid -> prPrec i 0 (concatD [doc (showString "void")])
    TypeChar -> prPrec i 0 (concatD [doc (showString "char")])
    TypeString -> prPrec i 0 (concatD [doc (showString "string")])
    TypeCompound compoundtype -> prPrec i 0 (concatD [prt 0 compoundtype])

instance Print CompoundType where
  prt i e = case e of
    TypePointer type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "*")])
    TypeArray array -> prPrec i 0 (concatD [prt 0 array])

instance Print Array where
  prt i e = case e of
    TypeMultiArray arrays -> prPrec i 0 (concatD [doc (showString "["), prt 0 arrays, doc (showString "]")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

