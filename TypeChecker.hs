module TypeChecker where

import Control.Monad

--import Data.Map (Map)
import qualified Data.Map as Map

import AbsE
import PrintE
import ErrM

type Env       = ([Sig], [(BlockType, Context)])
type Sig       = Map.Map String ([(Modality, Type)], Type)  
type Context   = Map.Map String Type

data BlockType = NormBlock | FunBlock | IterBlock
  deriving(Eq, Ord, Show, Read, Enum, Bounded)

--emptyEnv :: Env
--emptyEnv = ([Map.empty], [(NormBlock, Map.empty)]) :: ([Sig], [(BlockType, Context)])
defaultDefs =
  Map.insert "i" TypeInt Map.empty

-- Default given primitives.
defaultPrimitives :: Sig
defaultPrimitives = 
  Map.insert "writeInt"    ([(ModEmpty, TypeInt)], TypeVoid) $     -- void writeInt(int i)
  Map.insert "writeFloat"  ([(ModEmpty, TypeDouble)], TypeVoid) $  -- void writeFloat(double d)
  Map.insert "writeChar"   ([(ModEmpty, TypeChar)], TypeVoid) $    -- void writeChar(char c)
  Map.insert "writeString" ([(ModEmpty, TypeString)], TypeVoid) $  -- void writeString(string s)
  Map.insert "readInt"     ([], TypeInt) $                         -- int readInt()
  Map.insert "readDouble"  ([], TypeDouble) $                      -- double readFloat()
  Map.insert "readChar"    ([], TypeChar) $                        -- char readChar()
  Map.insert "readString"  ([], TypeString) Map.empty              -- string readString()

-- Initialize the environment with the default primitives.
initEnv :: Env
initEnv = ([defaultPrimitives], [(NormBlock, Map.empty)])

postAttach :: a -> [a] -> [a]
postAttach a [] = [a]
postAttach a (x:xs) = x : postAttach a xs

--typeCheck :: Program -> Err Env
typeCheck :: Program -> Err (Env, [Program])
typeCheck prog@(PDefs def) = foldM checkDecl (initEnv, []) def
--typeCheck (PDefs def) = foldM extendFun (Map.empty, Map.empty) def

checkDecl :: (Env, [Program]) -> Decl -> Err (Env,[Program])
checkDecl (env@(sig@(x:xs), (block:_)), prog) def = 
  case def of
    DeclFun lexpr@(LExprId (PIdent (p, fname))) args guard@(GuardType t) stmts -> 
      case lookupFun lexpr sig of
        Ok t  -> fail $ show p ++ ": function " ++ printTree fname ++ " declared twice!"
        --_ -> Ok (([addFun x fname args guard], [block]), (PTDefs [AnnotatedDecl t (DeclFun lexpr args guard stmts)]):prog)
        _ -> Ok (([addFun x fname args guard], [block]), postAttach (PTDefs [AnnotatedDecl t (DeclFun lexpr args guard stmts)]) prog)
    --[DeclStmt stmt] -> Ok (env,d)
    _ -> Bad "you should not be here"
    --_ -> Ok (env, (PAnnotatedDefs [AnnotatedDecl t [DeclStmt [StmtBreak]]))

{- checkStmt :: Env -> Stmt -> Err Env
checkStmt env@(sig, blocks@((blockType, context):xs)) stmt = case stmt of
  StmtVarInit lexpr@(LExprId (PIdent (p,ident))) guard expr _ -> do 
    case lookupVar lexpr blocks of
      Ok t -> fail $ show p ++ ": variable " ++ printTree ident ++ " declared twice!"
      _ -> Ok $ (sig, ((blockType, addVar context lexpr guard):blocks)) 
  StmtDefInit lexpr@(LExprId (PIdent (p,ident))) guard expr _ -> do 
    case lookupVar lexpr blocks of
      Ok t -> fail $ show p ++ ": constant " ++ printTree ident ++ " declared twice!"
      _ -> Ok $ (sig, ((blockType, addVar context lexpr guard):blocks))
  _ -> return env -}

lookupFun :: LExpr -> [Sig] -> Err ([(Modality, Type)], Type)
lookupFun _ [] = Bad "function is not declared!"
lookupFun lexpr@(LExprId (PIdent (p,ident))) sig@(x:xs) =
  case Map.lookup ident x of
    Just t -> Ok t
    Nothing -> lookupFun lexpr xs

lookupVar :: LExpr -> [(BlockType, Context)] -> Err Type
lookupVar _ [] = Bad "variable is not declared!"
lookupVar lexpr@(LExprId (PIdent (p,ident))) ((blockType, context):xs) = 
  case Map.lookup ident context of
    Just t -> Ok t
    Nothing -> lookupVar lexpr xs

-- Add function to signature.
-- Usage: addFun(sig,id,[args],guard) = sig  
addFun :: Sig -> String -> [Arg] -> Guard -> Sig
addFun sig fname args guard = 
  case guard of
    GuardType retType -> Map.insert fname ([(mod, t) | (ArgDecl mod _ (GuardType t)) <- args], retType) sig 
    GuardVoid -> Map.insert fname ([(mod, t) | (ArgDecl mod _ (GuardType t)) <- args], TypeVoid) sig -- procedures

addVar :: Context -> LExpr -> Guard -> Context
addVar context (LExprId (PIdent (p, ident))) guard =
  case guard of
    GuardType t -> Map.insert ident t context
    GuardVoid -> Map.insert ident TypeVoid context 



