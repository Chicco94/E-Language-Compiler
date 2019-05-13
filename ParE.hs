{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParE where
import AbsE
import LexE
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Integer) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Integer)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Double) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Double)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Char) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Char)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (String) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (String)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (PIdent) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (PIdent)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Program) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Program)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([Decl]) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([Decl])
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([AnnotatedDecl]) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([AnnotatedDecl])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Decl) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Decl)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (AnnotatedDecl) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (AnnotatedDecl)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Arg]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Arg])
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([Stmt]) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([Stmt])
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Arg) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Arg)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Modality) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Modality)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Guard) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Guard)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Stmt) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Stmt)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (CompStmt) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (CompStmt)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (NormCase) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (NormCase)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (DfltCase) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (DfltCase)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([NormCase]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ([NormCase])
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([DfltCase]) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([DfltCase])
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Expr) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Expr)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (LExpr) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (LExpr)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (Ref) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Ref)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Expr) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Expr)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Expr) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Expr)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Expr) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Expr)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Expr) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Expr)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Expr) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Expr)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Expr) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Expr)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Expr) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Expr)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Expr) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Expr)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Expr) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Expr)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Expr) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Expr)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Expr) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Expr)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Expr) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Expr)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Expr) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Expr)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (Expr) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (Expr)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Expr) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Expr)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Expr) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Expr)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Expr) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Expr)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([Expr]) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ([Expr])
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (AssignOperator) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (AssignOperator)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Type) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Type)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (CompoundType) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (CompoundType)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (TypeIter) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (TypeIter)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x48\x91\x00\x00\x69\x43\x37\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x58\xc2\x02\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x04\x08\xf8\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x20\x48\x91\x00\x00\x00\x01\x02\x3e\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x40\x80\x80\x0f\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x02\x04\x7c\x00\x00\x00\x00\x00\x00\x82\x14\x09\x00\x50\x99\xa8\xe0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x10\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x80\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x14\x09\x00\x90\x36\x74\xf3\x03\x00\x00\x00\x00\x00\x10\xa4\x48\x00\x00\x80\x00\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x41\x8a\x04\x00\x00\x08\x10\xf0\x01\x00\x00\x00\x00\x00\x08\x52\x24\x00\x00\x40\x80\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x90\x22\x01\x00\x00\x02\x04\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x48\x91\x00\x00\x00\x01\x02\x3e\x00\x00\x00\x00\x00\x00\x41\x8a\x04\x00\x00\x08\x10\xf0\x01\x00\x00\x00\x00\x00\x08\x52\x24\x00\x00\x40\x80\x80\x0f\x00\x00\x00\x00\x00\x40\x90\x22\x01\x00\x00\x02\x04\x7c\x00\x00\x00\x00\x00\x00\x82\x14\x09\x00\x00\x10\x20\xe0\x03\x00\x00\x00\x00\x00\x10\xa4\x48\x00\x00\x80\x00\x01\x1f\x00\x00\x00\x00\x00\x80\x20\x45\x02\x00\x00\x04\x08\xf8\x00\x00\x00\x00\x00\x00\x04\x29\x12\x00\x00\x20\x40\xc0\x07\x00\x00\x00\x00\x00\x20\x48\x91\x00\x00\x00\x01\x02\x3e\x00\x00\x00\x00\x00\x00\x41\x8a\x04\x00\x00\x08\x10\xf0\x01\x00\x00\x00\x00\x00\x08\x52\x24\x00\x00\x40\x80\x80\x0f\x00\x00\x00\x00\x00\x40\x90\x22\x01\x00\x00\x02\x04\x7c\x00\x00\x00\x00\x00\x00\x82\x14\x09\x00\x00\x10\x20\xe0\x03\x00\x00\x00\x00\x00\x10\xa4\x48\x00\x00\x80\x00\x01\x1f\x00\x00\x00\x00\x00\x80\x20\x45\x02\x00\x00\x04\x08\xf8\x00\x00\x00\x00\x00\x00\x04\x29\x12\x00\x00\x20\x40\xc0\x07\x00\x00\x00\x00\x00\x20\x48\x91\x00\x00\x00\x01\x02\x3e\x00\x00\x00\x00\x00\x00\x41\x8a\x04\x00\x00\x08\x10\xf0\x01\x00\x00\x00\x00\x00\x08\x52\x24\x00\x00\x40\x80\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x29\x12\x00\x00\x20\x40\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x48\x91\x00\x00\x69\x43\x37\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x01\x00\x00\x00\x00\x00\x00\x40\x90\x22\x01\x00\x2a\x13\x15\x7c\x00\x00\x00\x00\x00\x00\x82\x14\x09\x00\x10\x10\x20\xe0\x03\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x82\x14\x09\x00\x00\x10\x20\xe0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x48\x91\x00\x00\x00\x01\x02\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x14\x09\x00\x00\x10\x20\xe0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x08\x00\x00\x00\x00\x00\x00\x08\x52\x24\x00\x00\x40\x80\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Integer","Double","Char","String","PIdent","Program","ListDecl","ListAnnotatedDecl","Decl","AnnotatedDecl","ListArg","ListStmt","Arg","Modality","Guard","Stmt","CompStmt","NormCase","DfltCase","ListNormCase","ListDfltCase","Expr","LExpr","Ref","Expr17","Expr16","Expr15","Expr14","Expr13","Expr12","Expr11","Expr10","Expr9","Expr8","Expr4","Expr3","Expr1","Expr2","Expr5","Expr6","Expr7","ListExpr","AssignOperator","Type","CompoundType","TypeIter","'!'","'!='","'%'","'%%'","'%%='","'%='","'&'","'&&'","'&='","'('","')'","'*'","'**'","'**='","'*='","'+'","'+='","','","'-'","'-='","'..'","'..!'","'/'","'//'","'//='","'/='","':'","':='","':]'","';'","'<'","'<='","'=='","'>'","'>='","'['","']'","'bool'","'break'","'char'","'continue'","'def'","'double'","'false'","'for'","'in'","'int'","'match'","'match _'","'return'","'string'","'switch'","'true'","'var'","'void'","'while'","'{'","'|='","'||'","'}'","L_integ","L_doubl","L_charac","L_quoted","L_PIdent","%eof"]
        bit_start = st * 115
        bit_end = (st + 1) * 115
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..114]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xc8\xff\x00\x00\xda\xff\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x87\x00\x38\x00\x1b\x00\x27\x00\xff\xff\x4b\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x02\x00\x90\x00\x02\x00\x68\x00\x68\x00\x4f\x00\x00\x00\x00\x00\x02\x00\x00\x00\x22\x00\xf9\xff\x61\x00\x00\x00\xfb\xff\x65\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x90\x00\x00\x00\x5d\x00\x02\x00\x90\x00\x90\x00\x00\x00\x59\x00\xfa\xff\x00\x00\x1b\x00\xfd\xff\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x77\x00\x8d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x87\x00\x87\x00\x38\x00\x38\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x27\x00\x27\x00\x00\x00\x8a\x00\x00\x00\x7d\x00\x00\x00\x28\x00\x88\x00\xdc\xff\x4f\x00\x75\x00\xa0\x00\xa1\x00\x8c\x00\xee\xff\xa4\x00\x00\x00\x7e\x00\x90\x00\x00\x00\x9d\x00\x80\x00\x9c\x00\x82\x00\xb8\x00\xbb\x00\xba\x00\x91\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\xc9\x00\xd5\x00\xdc\xff\xd5\x00\x00\x00\x00\x00\x00\x00\xd3\x00\xd7\x00\x00\x00\x00\x00\x00\x00\xc6\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\x90\x00\xbe\x00\x00\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x48\x00\x00\x00\x00\x00\x00\x00\x89\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xce\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x04\x41\x00\xdb\x01\x7f\x00\x80\x04\x85\x04\xbd\x00\x00\x00\x00\x00\x97\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x00\x00\xac\x00\x00\x00\xf9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\x01\x04\x02\x00\x00\xf2\x00\x30\x03\x2d\x02\x56\x02\x00\x00\x00\x00\x0c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x03\x4c\x03\x75\x03\x80\x03\xa1\x03\xab\x03\xcb\x03\xd5\x03\xf5\x03\xfe\x03\xfa\x00\x1d\x04\x25\x04\x2c\x04\x49\x04\x50\x04\x57\x04\x74\x04\xfa\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x01\x00\x00\x60\x00\xc7\x00\xeb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x01\x7f\x02\x00\x00\x0d\x01\x00\x00\x00\x00\x0e\x01\x00\x00\x00\x00\x00\x00\x18\x01\x00\x00\x00\x00\xa8\x02\x00\x00\x00\x00\x00\x00\x5f\x01\x00\x00\x00\x00\x0f\x01\x64\x00\x11\x01\x00\x00\x00\x00\x27\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x2b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x01\xd1\x02\x2d\x01\x00\x00\x2e\x01\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf8\xff\x00\x00\xfe\xff\x00\x00\xf9\xff\xca\xff\xc9\xff\xc8\xff\xc7\xff\xcf\xff\xf7\xff\xf4\xff\xf2\xff\xde\xff\x00\x00\xcc\xff\xce\xff\xc4\xff\xc2\xff\xbd\xff\xbb\xff\xb5\xff\xb2\xff\xaf\xff\xaa\xff\xa7\xff\x9e\xff\xa3\xff\xa1\xff\xd0\xff\xa2\xff\xa5\xff\xa0\xff\x9f\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\xff\xdb\xff\x00\x00\xc5\xff\x00\x00\x00\x00\x00\x00\xc6\xff\x00\x00\x00\x00\xf8\xff\xfd\xff\xfc\xff\xfb\xff\xfa\xff\x00\x00\x00\x00\xcf\xff\xe7\xff\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\xe7\xff\xcc\xff\x86\xff\x00\x00\x89\xff\x87\xff\x9d\xff\x8f\xff\x8b\xff\x8e\xff\x8d\xff\x8a\xff\x8c\xff\xbf\xff\xbe\xff\xcd\xff\x00\x00\xc0\xff\xc1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\xff\x92\xff\x98\xff\x90\xff\x95\xff\x97\xff\x96\xff\x94\xff\x93\xff\x9a\xff\x99\xff\xe5\xff\x9d\xff\x9c\xff\x00\x00\xd1\xff\xbc\xff\xb8\xff\xb9\xff\xba\xff\xb6\xff\xb7\xff\xb3\xff\xb4\xff\xb0\xff\xb1\xff\xab\xff\xad\xff\xac\xff\xae\xff\xa9\xff\xa8\xff\xa6\xff\xa4\xff\xcb\xff\x00\x00\x88\xff\x00\x00\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\x00\x00\x00\x00\xe4\xff\xe7\xff\x00\x00\x00\x00\x00\x00\xe6\xff\x00\x00\xef\xff\x00\x00\xe8\xff\xe9\xff\x00\x00\xf1\xff\x85\xff\xc3\xff\x9d\xff\x9b\xff\x00\x00\xe7\xff\xf0\xff\xe7\xff\xd9\xff\xe0\xff\xd5\xff\x00\x00\x00\x00\xda\xff\xe2\xff\xe3\xff\xd3\xff\x00\x00\xee\xff\xeb\xff\xe1\xff\xf3\xff\xd4\xff\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\xdd\xff\xd6\xff\xd7\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x0a\x00\x0a\x00\x3d\x00\x2a\x00\x0c\x00\x07\x00\x0c\x00\x1c\x00\x0a\x00\x1e\x00\x0c\x00\x0c\x00\x05\x00\x06\x00\x10\x00\x36\x00\x09\x00\x13\x00\x1b\x00\x31\x00\x1e\x00\x0e\x00\x0f\x00\x1d\x00\x11\x00\x42\x00\x41\x00\x14\x00\x24\x00\x21\x00\x3c\x00\x0a\x00\x19\x00\x1a\x00\x24\x00\x1c\x00\x1e\x00\x27\x00\x01\x00\x29\x00\x2a\x00\x0d\x00\x2c\x00\x2d\x00\x07\x00\x15\x00\x16\x00\x0a\x00\x32\x00\x0c\x00\x34\x00\x35\x00\x36\x00\x10\x00\x38\x00\x39\x00\x13\x00\x41\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x41\x00\x3a\x00\x04\x00\x1f\x00\x20\x00\x10\x00\x22\x00\x23\x00\x13\x00\x24\x00\x05\x00\x06\x00\x27\x00\x01\x00\x29\x00\x2a\x00\x08\x00\x2c\x00\x2d\x00\x07\x00\x16\x00\x17\x00\x0a\x00\x32\x00\x0c\x00\x34\x00\x35\x00\x36\x00\x10\x00\x38\x00\x39\x00\x13\x00\x41\x00\x3b\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x0a\x00\x0a\x00\x0c\x00\x0d\x00\x0a\x00\x0a\x00\x0c\x00\x0d\x00\x0a\x00\x24\x00\x0c\x00\x26\x00\x01\x00\x28\x00\x1b\x00\x0b\x00\x2b\x00\x2c\x00\x07\x00\x11\x00\x2f\x00\x0a\x00\x14\x00\x0c\x00\x33\x00\x04\x00\x35\x00\x10\x00\x37\x00\x2e\x00\x13\x00\x12\x00\x03\x00\x04\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x01\x00\x08\x00\x0c\x00\x2c\x00\x16\x00\x17\x00\x07\x00\x0b\x00\x24\x00\x0a\x00\x04\x00\x0c\x00\x35\x00\x17\x00\x18\x00\x10\x00\x2c\x00\x25\x00\x13\x00\x1c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x35\x00\x0b\x00\x0b\x00\x16\x00\x17\x00\x0b\x00\x04\x00\x25\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x39\x00\x1b\x00\x39\x00\x1e\x00\x39\x00\x2c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x16\x00\x17\x00\x0c\x00\x35\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x12\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x41\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x1e\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1b\x00\x1e\x00\x2b\x00\x2c\x00\x2d\x00\x1e\x00\x30\x00\x39\x00\x2a\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x06\x00\x0e\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x2d\x00\x10\x00\x0e\x00\x0e\x00\x04\x00\x0e\x00\x10\x00\x0e\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x13\x00\x10\x00\x12\x00\x10\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x16\x00\x17\x00\xff\xff\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\xff\xff\xff\xff\xff\xff\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x57\x00\x23\x00\x3f\x00\x8f\x00\x03\x00\xa3\x00\x26\x00\x24\x00\x8c\x00\x99\x00\x25\x00\x9a\x00\x26\x00\x26\x00\x68\x00\x69\x00\x27\x00\xa4\x00\x6a\x00\x28\x00\x90\x00\xc2\x00\x40\x00\x6b\x00\x6c\x00\x8d\x00\x6d\x00\xff\xff\xea\xff\x6e\x00\x3d\x00\x58\x00\xc3\x00\x74\x00\x6f\x00\x70\x00\x29\x00\x71\x00\x73\x00\x2a\x00\x23\x00\x2b\x00\x2c\x00\x66\x00\x2d\x00\x2e\x00\x24\x00\x5d\x00\x5e\x00\x25\x00\x2f\x00\x26\x00\x30\x00\x31\x00\x32\x00\x27\x00\x33\x00\x34\x00\x28\x00\x38\x00\x97\x00\x03\x00\x35\x00\x36\x00\x37\x00\x38\x00\x38\x00\x72\x00\x3a\x00\x59\x00\x5a\x00\x5f\x00\x5b\x00\x5c\x00\x60\x00\x29\x00\x03\x00\x04\x00\x2a\x00\x23\x00\x2b\x00\x2c\x00\x56\x00\x2d\x00\x2e\x00\x24\x00\x52\x00\x10\x00\x25\x00\x2f\x00\x26\x00\x30\x00\x31\x00\x32\x00\x27\x00\x33\x00\x34\x00\x28\x00\x38\x00\x55\x00\x03\x00\x35\x00\x36\x00\x37\x00\x38\x00\x9f\x00\x3e\x00\xa0\x00\xa1\x00\xb8\x00\x3a\x00\xa0\x00\xa1\x00\x25\x00\x48\x00\x26\x00\x49\x00\x23\x00\x4a\x00\x90\x00\x8a\x00\x4b\x00\x2d\x00\x24\x00\xbc\x00\x4c\x00\x25\x00\xbd\x00\x26\x00\x4d\x00\x3a\x00\x31\x00\x27\x00\x4e\x00\x91\x00\x28\x00\xa9\x00\x61\x00\x62\x00\x03\x00\x35\x00\x36\x00\x37\x00\x38\x00\x23\x00\x56\x00\x63\x00\x2d\x00\x50\x00\x10\x00\x24\x00\xa8\x00\x48\x00\x25\x00\x3a\x00\x26\x00\x31\x00\x64\x00\x65\x00\x27\x00\x2d\x00\xa7\x00\x28\x00\xa5\x00\x03\x00\x35\x00\x36\x00\x37\x00\x38\x00\x31\x00\x9d\x00\x9c\x00\x41\x00\x10\x00\x98\x00\x3a\x00\x9b\x00\x03\x00\x35\x00\x36\x00\x37\x00\x38\x00\x34\x00\x90\x00\xb1\x00\xb0\x00\x34\x00\x2d\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x3b\x00\x10\x00\x8c\x00\x31\x00\xae\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\xad\x00\x03\x00\x35\x00\x36\x00\x37\x00\x38\x00\x38\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x43\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x43\x00\xbb\x00\x44\x00\x45\x00\x46\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x90\x00\xb6\x00\x9e\x00\x45\x00\x46\x00\xb5\x00\xbf\x00\x34\x00\x66\x00\x40\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x38\x00\x94\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x43\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x7e\x00\x9d\x00\xb3\x00\x8d\x00\xb1\x00\xab\x00\xb9\x00\xae\x00\xb7\x00\x74\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x8a\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\xb6\x00\xbb\x00\xc0\x00\xc4\x00\xc3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x75\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xa9\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\xa5\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x95\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x76\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x88\x00\x93\x00\x10\x00\x00\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x87\x00\x20\x00\x21\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x86\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x85\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x84\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x83\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x82\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x81\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x80\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x7f\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x7d\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x7c\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x7b\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x7a\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x79\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x78\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x42\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x77\x00\x42\x00\x10\x00\x11\x00\x12\x00\x53\x00\x42\x00\x10\x00\x11\x00\x12\x00\x4f\x00\x42\x00\x10\x00\x11\x00\x12\x00\x4e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 122) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122)
	]

happy_n_terms = 67 :: Int
happy_n_nonterms = 46 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn4
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn5
		 ((read ( happy_var_1)) :: Double
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TC happy_var_1)) -> 
	happyIn6
		 ((read ( happy_var_1)) :: Char
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn7
		 (happy_var_1
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (PIdent (mkPosToken happy_var_1)
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (AbsE.PDefs (reverse happy_var_1)
	)}

happyReduce_7 = happySpecReduce_0  6# happyReduction_7
happyReduction_7  =  happyIn10
		 ([]
	)

happyReduce_8 = happySpecReduce_2  6# happyReduction_8
happyReduction_8 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_9 = happySpecReduce_0  7# happyReduction_9
happyReduction_9  =  happyIn11
		 ([]
	)

happyReduce_10 = happySpecReduce_2  7# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_11 = happySpecReduce_1  8# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (AbsE.TypedDecl happy_var_1
	)}

happyReduce_12 = happyReduce 7# 8# happyReduction_12
happyReduction_12 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut14 happy_x_4 of { happy_var_4 -> 
	case happyOut18 happy_x_6 of { happy_var_6 -> 
	case happyOut20 happy_x_7 of { happy_var_7 -> 
	happyIn12
		 (AbsE.DeclFun happy_var_2 happy_var_4 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_13 = happySpecReduce_1  8# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (AbsE.DeclStmt happy_var_1
	)}

happyReduce_14 = happyReduce 4# 9# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut47 happy_x_2 of { happy_var_2 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (AbsE.ADecl happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_15 = happySpecReduce_0  10# happyReduction_15
happyReduction_15  =  happyIn14
		 ([]
	)

happyReduce_16 = happySpecReduce_1  10# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ((:[]) happy_var_1
	)}

happyReduce_17 = happySpecReduce_3  10# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_18 = happySpecReduce_0  11# happyReduction_18
happyReduction_18  =  happyIn15
		 ([]
	)

happyReduce_19 = happySpecReduce_2  11# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_20 = happySpecReduce_3  12# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (AbsE.ArgDecl happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_21 = happySpecReduce_0  13# happyReduction_21
happyReduction_21  =  happyIn17
		 (AbsE.ModEmpty
	)

happyReduce_22 = happySpecReduce_1  13# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn17
		 (AbsE.ModVar
	)

happyReduce_23 = happySpecReduce_1  13# happyReduction_23
happyReduction_23 happy_x_1
	 =  happyIn17
		 (AbsE.ModDef
	)

happyReduce_24 = happySpecReduce_0  14# happyReduction_24
happyReduction_24  =  happyIn18
		 (AbsE.GuardVoid
	)

happyReduce_25 = happySpecReduce_2  14# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (AbsE.GuardType happy_var_2
	)}

happyReduce_26 = happySpecReduce_2  15# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (AbsE.StmtExpr happy_var_1
	)}

happyReduce_27 = happyReduce 4# 15# happyReduction_27
happyReduction_27 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (AbsE.StmtVarDecl happy_var_2 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_28 = happyReduce 6# 15# happyReduction_28
happyReduction_28 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_3 of { happy_var_3 -> 
	case happyOut18 happy_x_5 of { happy_var_5 -> 
	happyIn19
		 (AbsE.StmtIterDecl happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_29 = happyReduce 6# 15# happyReduction_29
happyReduction_29 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut25 happy_x_5 of { happy_var_5 -> 
	happyIn19
		 (AbsE.StmtVarInit happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_30 = happyReduce 6# 15# happyReduction_30
happyReduction_30 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut25 happy_x_5 of { happy_var_5 -> 
	happyIn19
		 (AbsE.StmtDefInit happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_31 = happyReduce 5# 15# happyReduction_31
happyReduction_31 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (AbsE.StmtReturn happy_var_3
	) `HappyStk` happyRest}

happyReduce_32 = happySpecReduce_2  15# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  happyIn19
		 (AbsE.StmtNoReturn
	)

happyReduce_33 = happySpecReduce_1  15# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (AbsE.SComp happy_var_1
	)}

happyReduce_34 = happyReduce 8# 15# happyReduction_34
happyReduction_34 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOut23 happy_x_6 of { happy_var_6 -> 
	case happyOut24 happy_x_7 of { happy_var_7 -> 
	happyIn19
		 (AbsE.SSwitchCase happy_var_3 (reverse happy_var_6) (reverse happy_var_7)
	) `HappyStk` happyRest}}}

happyReduce_35 = happySpecReduce_1  15# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn19
		 (AbsE.StmtBreak
	)

happyReduce_36 = happySpecReduce_1  15# happyReduction_36
happyReduction_36 happy_x_1
	 =  happyIn19
		 (AbsE.StmtContinue
	)

happyReduce_37 = happyReduce 5# 15# happyReduction_37
happyReduction_37 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOut20 happy_x_5 of { happy_var_5 -> 
	happyIn19
		 (AbsE.StmtWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_38 = happyReduce 5# 15# happyReduction_38
happyReduction_38 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut49 happy_x_4 of { happy_var_4 -> 
	case happyOut20 happy_x_5 of { happy_var_5 -> 
	happyIn19
		 (AbsE.StmtFor happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_39 = happySpecReduce_3  16# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (AbsE.StmtBlock (reverse happy_var_2)
	)}

happyReduce_40 = happySpecReduce_3  17# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (AbsE.CaseNormal happy_var_2 happy_var_3
	)}}

happyReduce_41 = happySpecReduce_2  18# happyReduction_41
happyReduction_41 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 (AbsE.CaseDefault happy_var_2
	)}

happyReduce_42 = happySpecReduce_0  19# happyReduction_42
happyReduction_42  =  happyIn23
		 ([]
	)

happyReduce_43 = happySpecReduce_2  19# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_44 = happySpecReduce_0  20# happyReduction_44
happyReduction_44  =  happyIn24
		 ([]
	)

happyReduce_45 = happySpecReduce_2  20# happyReduction_45
happyReduction_45 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_46 = happySpecReduce_3  21# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (AbsE.StmtAssign happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_47 = happySpecReduce_1  21# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_48 = happySpecReduce_1  22# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (AbsE.LExprId happy_var_1
	)}

happyReduce_49 = happySpecReduce_1  22# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (AbsE.LExprRef happy_var_1
	)}

happyReduce_50 = happySpecReduce_2  23# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (AbsE.RefExpr happy_var_2
	)}

happyReduce_51 = happySpecReduce_1  24# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (AbsE.LeftExpr happy_var_1
	)}

happyReduce_52 = happySpecReduce_3  24# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (happy_var_2
	)}

happyReduce_53 = happySpecReduce_1  25# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (AbsE.ExprInt happy_var_1
	)}

happyReduce_54 = happySpecReduce_1  25# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (AbsE.ExprDouble happy_var_1
	)}

happyReduce_55 = happySpecReduce_1  25# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (AbsE.ExprChar happy_var_1
	)}

happyReduce_56 = happySpecReduce_1  25# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (AbsE.ExprString happy_var_1
	)}

happyReduce_57 = happySpecReduce_1  25# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn29
		 (AbsE.ExprTrue
	)

happyReduce_58 = happySpecReduce_1  25# happyReduction_58
happyReduction_58 happy_x_1
	 =  happyIn29
		 (AbsE.ExprFalse
	)

happyReduce_59 = happySpecReduce_1  25# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_60 = happyReduce 4# 26# happyReduction_60
happyReduction_60 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 (AbsE.ExprFunCall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_61 = happySpecReduce_1  26# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_62 = happySpecReduce_2  27# happyReduction_62
happyReduction_62 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (AbsE.ExprBoolNot happy_var_2
	)}

happyReduce_63 = happySpecReduce_2  27# happyReduction_63
happyReduction_63 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (AbsE.ExprDeref happy_var_2
	)}

happyReduce_64 = happySpecReduce_2  27# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (AbsE.ExprNegation happy_var_2
	)}

happyReduce_65 = happySpecReduce_2  27# happyReduction_65
happyReduction_65 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (AbsE.ExprAddition happy_var_2
	)}

happyReduce_66 = happySpecReduce_1  27# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  28# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (AbsE.ExprPower happy_var_1 happy_var_3
	)}}

happyReduce_68 = happySpecReduce_1  28# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (happy_var_1
	)}

happyReduce_69 = happySpecReduce_3  29# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (AbsE.ExprMul happy_var_1 happy_var_3
	)}}

happyReduce_70 = happySpecReduce_3  29# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (AbsE.ExprFloatDiv happy_var_1 happy_var_3
	)}}

happyReduce_71 = happySpecReduce_3  29# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (AbsE.ExprIntDiv happy_var_1 happy_var_3
	)}}

happyReduce_72 = happySpecReduce_3  29# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (AbsE.ExprReminder happy_var_1 happy_var_3
	)}}

happyReduce_73 = happySpecReduce_3  29# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (AbsE.ExprModulo happy_var_1 happy_var_3
	)}}

happyReduce_74 = happySpecReduce_1  29# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (happy_var_1
	)}

happyReduce_75 = happySpecReduce_3  30# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (AbsE.ExprPlus happy_var_1 happy_var_3
	)}}

happyReduce_76 = happySpecReduce_3  30# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (AbsE.ExprMinus happy_var_1 happy_var_3
	)}}

happyReduce_77 = happySpecReduce_1  30# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (happy_var_1
	)}

happyReduce_78 = happySpecReduce_3  31# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (AbsE.ExprIntInc happy_var_1 happy_var_3
	)}}

happyReduce_79 = happySpecReduce_3  31# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (AbsE.ExprIntExc happy_var_1 happy_var_3
	)}}

happyReduce_80 = happySpecReduce_1  31# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_81 = happySpecReduce_3  32# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (AbsE.ExprLt happy_var_1 happy_var_3
	)}}

happyReduce_82 = happySpecReduce_3  32# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (AbsE.ExprGt happy_var_1 happy_var_3
	)}}

happyReduce_83 = happySpecReduce_3  32# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (AbsE.ExprLtEq happy_var_1 happy_var_3
	)}}

happyReduce_84 = happySpecReduce_3  32# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (AbsE.ExprGtEq happy_var_1 happy_var_3
	)}}

happyReduce_85 = happySpecReduce_1  32# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  33# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (AbsE.ExprEq happy_var_1 happy_var_3
	)}}

happyReduce_87 = happySpecReduce_3  33# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (AbsE.ExprNeq happy_var_1 happy_var_3
	)}}

happyReduce_88 = happySpecReduce_1  33# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (happy_var_1
	)}

happyReduce_89 = happySpecReduce_3  34# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (AbsE.ExprAnd happy_var_1 happy_var_3
	)}}

happyReduce_90 = happySpecReduce_1  34# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_91 = happySpecReduce_3  35# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (AbsE.ExprOr happy_var_1 happy_var_3
	)}}

happyReduce_92 = happySpecReduce_1  35# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

happyReduce_93 = happySpecReduce_1  36# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_94 = happySpecReduce_1  37# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_95 = happySpecReduce_1  38# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_96 = happySpecReduce_1  39# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_97 = happySpecReduce_1  40# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_98 = happySpecReduce_0  41# happyReduction_98
happyReduction_98  =  happyIn45
		 ([]
	)

happyReduce_99 = happySpecReduce_1  41# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ((:[]) happy_var_1
	)}

happyReduce_100 = happySpecReduce_3  41# happyReduction_100
happyReduction_100 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_101 = happySpecReduce_1  42# happyReduction_101
happyReduction_101 happy_x_1
	 =  happyIn46
		 (AbsE.OpAssign
	)

happyReduce_102 = happySpecReduce_1  42# happyReduction_102
happyReduction_102 happy_x_1
	 =  happyIn46
		 (AbsE.OpOr
	)

happyReduce_103 = happySpecReduce_1  42# happyReduction_103
happyReduction_103 happy_x_1
	 =  happyIn46
		 (AbsE.OpAnd
	)

happyReduce_104 = happySpecReduce_1  42# happyReduction_104
happyReduction_104 happy_x_1
	 =  happyIn46
		 (AbsE.OpPlus
	)

happyReduce_105 = happySpecReduce_1  42# happyReduction_105
happyReduction_105 happy_x_1
	 =  happyIn46
		 (AbsE.OpMinus
	)

happyReduce_106 = happySpecReduce_1  42# happyReduction_106
happyReduction_106 happy_x_1
	 =  happyIn46
		 (AbsE.OpMul
	)

happyReduce_107 = happySpecReduce_1  42# happyReduction_107
happyReduction_107 happy_x_1
	 =  happyIn46
		 (AbsE.OpIntDiv
	)

happyReduce_108 = happySpecReduce_1  42# happyReduction_108
happyReduction_108 happy_x_1
	 =  happyIn46
		 (AbsE.OpFloatDiv
	)

happyReduce_109 = happySpecReduce_1  42# happyReduction_109
happyReduction_109 happy_x_1
	 =  happyIn46
		 (AbsE.OpRemainder
	)

happyReduce_110 = happySpecReduce_1  42# happyReduction_110
happyReduction_110 happy_x_1
	 =  happyIn46
		 (AbsE.OpModulo
	)

happyReduce_111 = happySpecReduce_1  42# happyReduction_111
happyReduction_111 happy_x_1
	 =  happyIn46
		 (AbsE.OpPower
	)

happyReduce_112 = happySpecReduce_1  43# happyReduction_112
happyReduction_112 happy_x_1
	 =  happyIn47
		 (AbsE.TypeBool
	)

happyReduce_113 = happySpecReduce_1  43# happyReduction_113
happyReduction_113 happy_x_1
	 =  happyIn47
		 (AbsE.TypeDouble
	)

happyReduce_114 = happySpecReduce_1  43# happyReduction_114
happyReduction_114 happy_x_1
	 =  happyIn47
		 (AbsE.TypeInt
	)

happyReduce_115 = happySpecReduce_1  43# happyReduction_115
happyReduction_115 happy_x_1
	 =  happyIn47
		 (AbsE.TypeVoid
	)

happyReduce_116 = happySpecReduce_1  43# happyReduction_116
happyReduction_116 happy_x_1
	 =  happyIn47
		 (AbsE.TypeChar
	)

happyReduce_117 = happySpecReduce_1  43# happyReduction_117
happyReduction_117 happy_x_1
	 =  happyIn47
		 (AbsE.TypeString
	)

happyReduce_118 = happySpecReduce_1  43# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (AbsE.TypeCompound happy_var_1
	)}

happyReduce_119 = happySpecReduce_2  44# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (AbsE.TypePointer happy_var_1
	)}

happyReduce_120 = happySpecReduce_1  44# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (AbsE.TypeIterable happy_var_1
	)}

happyReduce_121 = happySpecReduce_1  45# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (AbsE.TypeIterInterval happy_var_1
	)}

happyReduce_122 = happySpecReduce_3  45# happyReduction_122
happyReduction_122 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn49
		 (AbsE.TypeIterArray happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 66# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (TS _ 53) -> cont 53#;
	PT _ (TS _ 54) -> cont 54#;
	PT _ (TS _ 55) -> cont 55#;
	PT _ (TS _ 56) -> cont 56#;
	PT _ (TS _ 57) -> cont 57#;
	PT _ (TS _ 58) -> cont 58#;
	PT _ (TS _ 59) -> cont 59#;
	PT _ (TS _ 60) -> cont 60#;
	PT _ (TI happy_dollar_dollar) -> cont 61#;
	PT _ (TD happy_dollar_dollar) -> cont 62#;
	PT _ (TC happy_dollar_dollar) -> cont 63#;
	PT _ (TL happy_dollar_dollar) -> cont 64#;
	PT _ (T_PIdent _) -> cont 65#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 66# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut9 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/tmp/ghcb5f8_0/ghc_2.h" #-}




























































































































































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
