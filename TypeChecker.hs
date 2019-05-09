module TypeChecker where

import Control.Monad

--import Data.Map (Map)
import qualified Data.Map as Map

import AbsE
import PrintE
import ErrM

-- type Env = (Signature, [(BlockType, Context)])
type Env = (Sig, Context)                -- signature and stack of contexts
type Sig = Map.Map String ([Type], Type)  
type Context = Map.Map PIdent Type        -- or Map Ident Type

emptyEnv :: Env
emptyEnv = (Map.empty, Map.empty) :: (Sig, Context)

typeCheck :: Program -> Err Env
typeCheck (PDefs def) = foldM extendFun (Map.empty, Map.empty) def

extendFun :: Env -> Def -> Err Env
extendFun env@(sig,context) def = case def of
  DFun ret (PIdent (p@(row,col),fname)) args stmt -> case Map.lookup fname sig of
    Nothing -> Ok $ (addFun sig fname args ret, context)
    Just _  -> fail $ show p ++ ": function " ++ printTree fname ++ " declared twice!"
  _ -> Ok env

addFun :: Sig -> String -> [Arg] -> Type -> Sig
addFun sig fname args ret = Map.insert fname ([t | (ADecl t arg) <- args], ret) sig 



