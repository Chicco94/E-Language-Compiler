{-# LANGUAGE DeriveDataTypeable #-}
module ThreeAddressCode where

import Data.Generics

import AbsETAC



generateTAC :: Program -> TACProgram
-- generateTAC prog@() = 