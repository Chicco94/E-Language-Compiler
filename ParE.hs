{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParE where
import AbsE
import LexE
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn7 (Integer)
	| HappyAbsSyn8 (Char)
	| HappyAbsSyn9 (String)
	| HappyAbsSyn10 (Double)
	| HappyAbsSyn11 (Ident)
	| HappyAbsSyn12 (Boolean)
	| HappyAbsSyn13 (EndLine)
	| HappyAbsSyn14 (BasicType)
	| HappyAbsSyn15 (RExpr)
	| HappyAbsSyn30 (FunCall)
	| HappyAbsSyn31 ([RExpr])
	| HappyAbsSyn32 (LExpr)
	| HappyAbsSyn35 (BLExpr)
	| HappyAbsSyn36 (Program)
	| HappyAbsSyn37 ([Decl])
	| HappyAbsSyn38 (Decl)
	| HappyAbsSyn39 (TypeSpec)
	| HappyAbsSyn40 (CompoundType)
	| HappyAbsSyn41 ([Parameter])
	| HappyAbsSyn42 (Parameter)
	| HappyAbsSyn43 (Modality)
	| HappyAbsSyn44 (CompStmt)
	| HappyAbsSyn45 ([Stmt])
	| HappyAbsSyn46 (Stmt)
	| HappyAbsSyn47 (Assignment_op)
	| HappyAbsSyn48 (SelectionStmt)
	| HappyAbsSyn49 (SwitchLabel)
	| HappyAbsSyn50 ([SwitchLabel])
	| HappyAbsSyn51 (IterStmt)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,1059) ([0,0,0,0,0,0,0,0,0,0,0,17408,0,8584,1030,0,0,0,577,25,2048,61456,1,0,0,0,1088,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,16384,0,0,0,32768,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,49152,14,0,0,0,0,0,2176,0,0,0,0,0,0,515,48,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9216,400,32768,256,31,0,0,0,17408,0,0,0,0,0,0,577,25,2048,61456,1,0,0,32768,1088,0,1026,124,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,2048,0,0,0,0,0,19488,51352,3,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,1024,0,0,0,512,0,0,0,0,0,0,36928,1600,0,1026,124,0,0,4096,36900,1,128,7937,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,80,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,1,0,0,0,0,0,0,64,0,0,0,0,0,20,2,0,0,0,0,0,0,5120,0,0,0,0,0,0,512,4,0,0,16384,16528,6,512,31748,0,0,0,0,0,0,8,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,36928,1600,0,1026,124,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,6402,0,4104,496,0,0,0,16528,6,512,31748,0,0,0,9216,400,32768,256,31,0,0,0,25609,0,16416,1984,0,0,0,576,25,2048,61456,1,0,0,36864,1600,0,1026,124,0,0,0,36900,1,128,7937,0,0,0,2304,100,8192,49216,7,0,0,16384,6402,0,4104,496,0,0,0,16528,6,512,31748,0,0,0,9216,400,32768,256,31,0,0,0,25609,0,16416,1984,0,0,0,576,25,2048,61456,1,0,0,36864,1600,0,1026,124,0,0,4096,36900,1,128,7937,0,0,0,2308,100,8192,49216,7,0,0,16640,6402,0,4104,496,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17408,0,0,0,0,0,0,0,0,4,0,0,0,0,0,2,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16640,6402,0,4104,496,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,16,0,0,0,1024,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,768,12290,0,0,0,0,0,49152,128,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,128,0,64,0,0,0,2308,100,8192,49216,7,0,0,16640,6402,0,4104,496,0,0,0,256,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,25088,2440,1,0,0,0,2,3328,4420,0,0,0,0,0,0,20739,4,0,0,0,2048,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,8192,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,32,53248,5184,1,0,0,0,577,25,2056,61456,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2308,100,8704,49216,7,0,0,0,0,0,0,1,0,0,0,0,0,0,320,0,0,0,0,32768,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,36928,1600,0,1026,124,0,0,0,0,32768,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,1280,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,32,4096,0,0,0,0,8,13312,17680,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,577,25,2048,61456,1,0,0,36928,1600,0,1026,124,0,0,0,64,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,256,0,0,0,32,0,2,0,1,0,0,2048,0,128,0,64,0,0,0,0,0,0,0,0,0,0,0,8,13312,17680,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pStmt","%start_pRExpr","%start_pLExpr","Integer","Char","String","Double","Ident","Boolean","EndLine","BasicType","RExpr","RExpr1","RExpr2","RExpr3","RExpr4","RExpr5","RExpr6","RExpr7","RExpr8","RExpr9","RExpr10","RExpr11","RExpr12","RExpr13","RExpr14","FunCall","ListRExpr","LExpr","LExpr1","LExpr2","BLExpr","Program","ListDecl","Decl","TypeSpec","CompoundType","ListParameter","Parameter","Modality","CompStmt","ListStmt","Stmt","Assignment_op","SelectionStmt","SwitchLabel","ListSwitchLabel","IterStmt","'\n'","'!'","'!='","'%'","'%%'","'%%='","'%='","'&'","'&&'","'&='","'('","')'","'*'","'**'","'**='","'*='","'+'","'++'","'+='","','","'-'","'--'","'-='","'/'","'//'","'//='","'/='","':='","';'","'<'","'<='","'='","'=='","'>'","'>='","'['","']'","'bool'","'char'","'def'","'default'","'dev'","'do'","'else'","'false'","'float'","'for'","'if'","'in'","'int'","'match'","'string'","'switch'","'true'","'var'","'void'","'while'","'{'","'|='","'||'","'}'","L_integ","L_charac","L_quoted","L_doubl","L_ident","%eof"]
        bit_start = st * 118
        bit_end = (st + 1) * 118
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..117]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,200) ([0,149,29,148,-38,0,-34,-11,-11,0,0,0,0,49,0,20,68,0,0,0,0,51,172,992,0,83,0,0,0,0,0,0,98,192,29,114,0,0,0,0,0,0,17,95,0,60,0,0,90,84,130,29,29,0,86,144,0,92,92,92,144,57,-28,29,123,118,0,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-8,0,0,98,98,98,98,98,98,98,98,98,98,98,98,98,98,29,29,29,0,0,222,146,-11,0,146,29,0,0,-17,171,179,0,0,0,0,0,0,0,992,992,0,0,0,0,0,0,0,12,29,29,-4,0,0,147,99,135,187,-36,169,0,0,0,0,0,0,175,17,0,99,-1,0,0,4,151,57,-18,0,0,29,-21,0,0,0,0,167,57,0,154,53,99,0,0,0,29,29,210,203,176,0,0,-36,185,12,12,0,99,0,0,185,0,0,0,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,200) ([59,34,303,232,0,0,0,1,72,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,667,258,329,990,0,0,0,0,0,0,278,8,0,0,0,0,272,306,0,355,381,297,0,304,0,332,349,357,108,325,0,407,0,0,0,433,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,885,894,920,929,955,964,849,859,693,719,745,771,797,823,641,615,251,0,0,0,0,75,0,0,459,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,373,485,511,0,344,0,103,2,398,0,211,0,0,0,0,0,0,0,0,382,0,97,537,0,0,216,376,377,0,0,0,277,0,0,0,0,0,0,394,0,402,0,101,0,0,0,563,589,0,0,436,0,0,214,420,460,477,0,124,0,0,428,0,0,0,0,0,0,0
	])

happyAdjustOffset :: Int -> Int
happyAdjustOffset = id

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,200) ([-74,0,0,0,0,-5,0,0,0,-54,-55,-56,-57,0,-58,0,-20,-22,-24,-26,-27,-28,-35,-36,-39,-45,-47,-48,-51,-53,-52,-60,0,0,0,0,-11,-10,-6,-7,-8,-9,0,0,-94,0,-97,-96,0,0,0,0,0,-74,0,-73,-75,0,0,0,-92,0,0,0,0,0,-99,0,-13,-109,-108,-102,-110,-105,-103,-104,-107,-106,-100,-12,-101,-95,-49,0,-50,-25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-62,-71,-66,0,-69,0,-65,-72,0,-67,-68,-63,0,-21,-23,-34,-33,-29,-32,-31,-30,-38,-37,-42,-41,-40,-44,-43,-46,-59,0,0,0,0,-115,-117,0,0,0,0,-85,0,-14,-15,-17,-18,-16,-19,-80,0,-81,0,0,-93,-91,0,0,0,0,-98,-61,-62,0,-70,-64,-118,-119,-111,0,-116,0,0,0,-80,-84,-76,0,0,0,-86,0,-90,-88,-85,0,0,0,-83,0,-113,-114,0,-112,-82,-77,-78,-79,-87
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,1059) ([-1,2,13,20,12,4,2,8,12,7,11,29,8,1,6,11,37,18,1,55,21,22,18,51,62,21,22,26,27,28,66,2,60,67,32,33,37,8,4,60,11,29,60,60,45,41,29,18,40,45,21,22,60,54,3,66,60,23,54,25,11,62,63,64,65,66,62,63,64,65,66,37,60,39,45,41,4,9,44,4,60,30,31,54,33,34,35,67,29,30,37,62,63,64,65,66,1,14,26,27,28,6,7,28,7,10,8,4,7,11,15,16,13,60,19,58,18,60,23,21,22,26,27,28,29,11,23,67,25,32,33,7,18,32,33,36,22,38,39,31,37,11,39,45,41,46,38,44,58,50,66,52,54,67,59,56,32,33,66,45,62,63,64,65,66,18,18,18,54,22,22,22,49,38,39,57,62,63,64,65,66,46,36,12,40,50,42,52,9,17,43,56,43,21,47,48,47,48,11,55,53,32,53,28,57,58,57,58,61,58,18,44,58,66,22,66,0,1,2,3,4,5,12,20,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,18,25,66,58,22,34,35,36,34,35,36,0,1,2,3,4,5,25,42,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,0,1,2,3,4,5,25,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,0,1,2,3,4,5,37,4,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,30,25,0,1,2,3,4,5,31,4,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,4,25,0,1,2,3,4,5,4,37,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,6,25,0,1,2,3,4,5,43,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,7,25,0,1,2,3,4,5,37,37,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,37,25,0,1,2,3,4,5,37,4,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,37,25,0,1,2,3,4,5,37,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,6,25,0,1,2,3,4,5,-1,-1,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,10,11,12,13,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,-1,11,12,13,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,-1,-1,-1,-1,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,-1,-1,-1,-1,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,-1,-1,-1,-1,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,-1,-1,-1,-1,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,-1,-1,-1,-1,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,-1,-1,-1,-1,14,15,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,-1,0,1,2,3,4,5,16,17,18,19,20,21,22,23,-1,25,16,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,0,1,2,3,4,5,-1,-1,17,18,19,20,21,22,23,-1,25,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,0,1,2,3,4,5,-1,-1,17,18,19,20,21,22,23,-1,25,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,-1,-1,-1,0,1,2,3,4,5,-1,-1,17,18,19,20,21,22,23,-1,25,17,18,19,20,21,22,23,-1,25,0,1,2,3,4,5,4,5,-1,-1,-1,-1,-1,-1,-1,13,-1,-1,-1,-1,20,21,22,23,-1,25,24,25,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,1059) ([0,33,108,164,132,103,33,34,159,150,35,168,34,69,66,35,166,8,69,184,36,9,8,137,6,36,9,108,105,106,-89,33,102,-1,151,152,175,34,13,102,35,80,102,102,37,173,80,8,67,37,36,9,102,38,95,42,102,42,38,43,103,6,39,40,41,42,6,39,40,41,42,44,102,45,37,46,103,101,47,103,102,96,97,38,98,99,100,-1,54,55,191,6,39,40,41,42,69,87,104,105,106,70,71,109,175,72,34,13,175,35,73,74,154,102,75,54,8,102,76,36,9,77,78,79,80,35,42,-1,43,176,152,175,8,189,152,155,9,145,146,56,44,64,155,37,46,147,138,47,54,148,42,149,38,-1,81,150,195,152,42,37,6,39,40,41,42,8,8,8,38,9,9,9,135,145,146,134,6,39,40,41,42,147,111,163,58,148,59,149,101,93,49,150,49,94,50,51,50,51,143,60,52,180,52,179,53,54,53,54,157,54,8,194,54,42,9,42,9,10,11,12,13,14,187,186,170,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,112,31,42,54,113,180,181,182,199,181,182,9,10,11,12,13,14,6,171,113,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,114,31,9,10,11,12,13,14,84,81,113,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,166,31,9,10,11,12,13,14,65,64,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,60,31,9,10,11,12,13,14,56,141,83,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,140,31,9,10,11,12,13,14,139,137,62,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,161,31,9,10,11,12,13,14,157,177,61,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,143,31,9,10,11,12,13,14,169,168,135,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,192,31,9,10,11,12,13,14,191,184,132,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,198,31,9,10,11,12,13,14,194,197,164,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,196,31,9,10,11,12,13,14,0,0,160,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,159,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,173,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,188,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,187,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,115,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,116,18,19,20,21,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,0,85,19,20,21,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,0,0,0,0,122,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,0,0,0,0,121,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,0,0,0,0,120,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,0,0,0,0,119,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,0,0,0,0,118,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,0,0,0,0,117,22,23,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,0,9,10,11,12,13,14,124,24,25,26,27,28,29,30,0,31,123,24,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,9,10,11,12,13,14,0,0,130,25,26,27,28,29,30,0,31,129,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,9,10,11,12,13,14,0,0,128,25,26,27,28,29,30,0,31,127,25,26,27,28,29,30,0,31,9,10,11,12,13,14,0,0,0,9,10,11,12,13,14,0,0,126,25,26,27,28,29,30,0,31,125,25,26,27,28,29,30,0,31,9,10,11,12,13,14,88,89,0,0,0,0,0,0,0,90,0,0,0,0,82,28,29,30,0,31,91,92,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (4, 118) [
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
	(118 , happyReduce_118)
	]

happy_n_terms = 68 :: Int
happy_n_nonterms = 45 :: Int

happyReduce_4 = happySpecReduce_1  0 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn7
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  1 happyReduction_5
happyReduction_5 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn8
		 ((read ( happy_var_1)) :: Char
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  2 happyReduction_6
happyReduction_6 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  3 happyReduction_7
happyReduction_7 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn10
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn11
		 (Ident happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn12
		 (AbsE.Boolean_true
	)

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn12
		 (AbsE.Boolean_false
	)

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn13
		 (AbsE.EndLine1
	)

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn13
		 (AbsE.EndLine2
	)

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn14
		 (AbsE.BasicType_bool
	)

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn14
		 (AbsE.BasicType_char
	)

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn14
		 (AbsE.BasicType_string
	)

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn14
		 (AbsE.BasicType_float
	)

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn14
		 (AbsE.BasicType_int
	)

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn14
		 (AbsE.BasicType_void
	)

happyReduce_19 = happySpecReduce_1  8 happyReduction_19
happyReduction_19 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Or happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  9 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  9 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.And happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  10 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  10 happyReduction_24
happyReduction_24 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (AbsE.Not happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  11 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  12 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  13 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  13 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Eq happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Neq happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Lt happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.LtE happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  13 happyReduction_32
happyReduction_32 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Gt happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  13 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.GtE happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  14 happyReduction_34
happyReduction_34 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  15 happyReduction_35
happyReduction_35 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  15 happyReduction_36
happyReduction_36 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Add happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  15 happyReduction_37
happyReduction_37 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Sub happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  16 happyReduction_38
happyReduction_38 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  16 happyReduction_39
happyReduction_39 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Mul happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  16 happyReduction_40
happyReduction_40 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.FloDiv happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  16 happyReduction_41
happyReduction_41 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.IntDiv happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  16 happyReduction_42
happyReduction_42 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Rem happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  16 happyReduction_43
happyReduction_43 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Mod happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  17 happyReduction_44
happyReduction_44 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  17 happyReduction_45
happyReduction_45 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Pow happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  18 happyReduction_46
happyReduction_46 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  19 happyReduction_47
happyReduction_47 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  19 happyReduction_48
happyReduction_48 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (AbsE.Neg happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  19 happyReduction_49
happyReduction_49 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (AbsE.Ref happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  20 happyReduction_50
happyReduction_50 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  20 happyReduction_51
happyReduction_51 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.FCall happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  21 happyReduction_52
happyReduction_52 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  21 happyReduction_53
happyReduction_53 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Int happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  21 happyReduction_54
happyReduction_54 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Char happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  21 happyReduction_55
happyReduction_55 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.String happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  21 happyReduction_56
happyReduction_56 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Float happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  21 happyReduction_57
happyReduction_57 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Bool happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  22 happyReduction_58
happyReduction_58 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  22 happyReduction_59
happyReduction_59 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsE.Lexpr happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happyReduce 4 23 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (AbsE.Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_0  24 happyReduction_61
happyReduction_61  =  HappyAbsSyn31
		 ([]
	)

happyReduce_62 = happySpecReduce_1  24 happyReduction_62
happyReduction_62 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn31
		 ((:[]) happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  24 happyReduction_63
happyReduction_63 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn31
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  25 happyReduction_64
happyReduction_64 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (AbsE.PreInc happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  25 happyReduction_65
happyReduction_65 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (AbsE.PreDecr happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  26 happyReduction_66
happyReduction_66 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (AbsE.PostInc happy_var_1
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  26 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (AbsE.PostDecr happy_var_1
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  27 happyReduction_68
happyReduction_68 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn32
		 (AbsE.BasLExpr happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happyReduce 4 28 happyReduction_69
happyReduction_69 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn35
		 (AbsE.ArrayEl happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_70 = happySpecReduce_1  28 happyReduction_70
happyReduction_70 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn35
		 (AbsE.Id happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  28 happyReduction_71
happyReduction_71 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (AbsE.Deref happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  29 happyReduction_72
happyReduction_72 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (AbsE.Prog (reverse happy_var_1)
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_0  30 happyReduction_73
happyReduction_73  =  HappyAbsSyn37
		 ([]
	)

happyReduce_74 = happySpecReduce_2  30 happyReduction_74
happyReduction_74 (HappyAbsSyn38  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happyReduce 4 31 happyReduction_75
happyReduction_75 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (AbsE.Dvar happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_76 = happyReduce 6 31 happyReduction_76
happyReduction_76 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (AbsE.DvarAss happy_var_2 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_77 = happyReduce 6 31 happyReduction_77
happyReduction_77 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (AbsE.Dconst happy_var_2 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_78 = happyReduce 6 31 happyReduction_78
happyReduction_78 ((HappyAbsSyn44  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (AbsE.Dfun happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_1  32 happyReduction_79
happyReduction_79 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn39
		 (AbsE.BasTyp happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  32 happyReduction_80
happyReduction_80 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn39
		 (AbsE.CompType happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happyReduce 4 33 happyReduction_81
happyReduction_81 ((HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn40
		 (AbsE.ArrDef happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_3  33 happyReduction_82
happyReduction_82 (HappyAbsSyn39  happy_var_3)
	_
	_
	 =  HappyAbsSyn40
		 (AbsE.ArrUnDef happy_var_3
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_2  33 happyReduction_83
happyReduction_83 (HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (AbsE.Pointer happy_var_2
	)
happyReduction_83 _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_0  34 happyReduction_84
happyReduction_84  =  HappyAbsSyn41
		 ([]
	)

happyReduce_85 = happySpecReduce_1  34 happyReduction_85
happyReduction_85 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn41
		 ((:[]) happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  34 happyReduction_86
happyReduction_86 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn41
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_2  35 happyReduction_87
happyReduction_87 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsE.Param happy_var_1 happy_var_2
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_0  36 happyReduction_88
happyReduction_88  =  HappyAbsSyn43
		 (AbsE.Modality1
	)

happyReduce_89 = happySpecReduce_1  36 happyReduction_89
happyReduction_89 _
	 =  HappyAbsSyn43
		 (AbsE.Modality_var
	)

happyReduce_90 = happyReduce 4 37 happyReduction_90
happyReduction_90 (_ `HappyStk`
	(HappyAbsSyn45  happy_var_3) `HappyStk`
	(HappyAbsSyn37  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn44
		 (AbsE.BlockDecl (reverse happy_var_2) (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_91 = happySpecReduce_0  38 happyReduction_91
happyReduction_91  =  HappyAbsSyn45
		 ([]
	)

happyReduce_92 = happySpecReduce_2  38 happyReduction_92
happyReduction_92 (HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_92 _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  39 happyReduction_93
happyReduction_93 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn46
		 (AbsE.Comp happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_2  39 happyReduction_94
happyReduction_94 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn46
		 (AbsE.ProcCall happy_var_1 happy_var_2
	)
happyReduction_94 _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  39 happyReduction_95
happyReduction_95 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn46
		 (AbsE.Iter happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  39 happyReduction_96
happyReduction_96 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn46
		 (AbsE.Sel happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happyReduce 4 39 happyReduction_97
happyReduction_97 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	(HappyAbsSyn47  happy_var_2) `HappyStk`
	(HappyAbsSyn32  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (AbsE.Assgn happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_98 = happySpecReduce_2  39 happyReduction_98
happyReduction_98 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn46
		 (AbsE.LExprStmt happy_var_1 happy_var_2
	)
happyReduction_98 _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  40 happyReduction_99
happyReduction_99 _
	 =  HappyAbsSyn47
		 (AbsE.Assign
	)

happyReduce_100 = happySpecReduce_1  40 happyReduction_100
happyReduction_100 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnOr
	)

happyReduce_101 = happySpecReduce_1  40 happyReduction_101
happyReduction_101 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnAnd
	)

happyReduce_102 = happySpecReduce_1  40 happyReduction_102
happyReduction_102 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnAdd
	)

happyReduce_103 = happySpecReduce_1  40 happyReduction_103
happyReduction_103 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnSub
	)

happyReduce_104 = happySpecReduce_1  40 happyReduction_104
happyReduction_104 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnMul
	)

happyReduce_105 = happySpecReduce_1  40 happyReduction_105
happyReduction_105 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnDiv
	)

happyReduce_106 = happySpecReduce_1  40 happyReduction_106
happyReduction_106 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnDivInt
	)

happyReduce_107 = happySpecReduce_1  40 happyReduction_107
happyReduction_107 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnRem
	)

happyReduce_108 = happySpecReduce_1  40 happyReduction_108
happyReduction_108 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnMod
	)

happyReduce_109 = happySpecReduce_1  40 happyReduction_109
happyReduction_109 _
	 =  HappyAbsSyn47
		 (AbsE.AssgnPow
	)

happyReduce_110 = happyReduce 5 41 happyReduction_110
happyReduction_110 ((HappyAbsSyn44  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (AbsE.IfNoElse happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_111 = happyReduce 7 41 happyReduction_111
happyReduction_111 ((HappyAbsSyn44  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (AbsE.IfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_112 = happyReduce 6 41 happyReduction_112
happyReduction_112 ((HappyAbsSyn44  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (AbsE.Switch happy_var_2 (reverse happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_113 = happySpecReduce_2  42 happyReduction_113
happyReduction_113 (HappyAbsSyn44  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn49
		 (AbsE.SwitchL happy_var_1 happy_var_2
	)
happyReduction_113 _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_0  43 happyReduction_114
happyReduction_114  =  HappyAbsSyn50
		 ([]
	)

happyReduce_115 = happySpecReduce_2  43 happyReduction_115
happyReduction_115 (HappyAbsSyn49  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_3  44 happyReduction_116
happyReduction_116 (HappyAbsSyn44  happy_var_3)
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn51
		 (AbsE.While happy_var_2 happy_var_3
	)
happyReduction_116 _ _ _  = notHappyAtAll 

happyReduce_117 = happyReduce 5 44 happyReduction_117
happyReduction_117 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn51
		 (AbsE.DoWhile happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_118 = happyReduce 5 44 happyReduction_118
happyReduction_118 ((HappyAbsSyn44  happy_var_5) `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn51
		 (AbsE.For happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	happyDoAction 67 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1;
	PT _ (TS _ 2) -> cont 2;
	PT _ (TS _ 3) -> cont 3;
	PT _ (TS _ 4) -> cont 4;
	PT _ (TS _ 5) -> cont 5;
	PT _ (TS _ 6) -> cont 6;
	PT _ (TS _ 7) -> cont 7;
	PT _ (TS _ 8) -> cont 8;
	PT _ (TS _ 9) -> cont 9;
	PT _ (TS _ 10) -> cont 10;
	PT _ (TS _ 11) -> cont 11;
	PT _ (TS _ 12) -> cont 12;
	PT _ (TS _ 13) -> cont 13;
	PT _ (TS _ 14) -> cont 14;
	PT _ (TS _ 15) -> cont 15;
	PT _ (TS _ 16) -> cont 16;
	PT _ (TS _ 17) -> cont 17;
	PT _ (TS _ 18) -> cont 18;
	PT _ (TS _ 19) -> cont 19;
	PT _ (TS _ 20) -> cont 20;
	PT _ (TS _ 21) -> cont 21;
	PT _ (TS _ 22) -> cont 22;
	PT _ (TS _ 23) -> cont 23;
	PT _ (TS _ 24) -> cont 24;
	PT _ (TS _ 25) -> cont 25;
	PT _ (TS _ 26) -> cont 26;
	PT _ (TS _ 27) -> cont 27;
	PT _ (TS _ 28) -> cont 28;
	PT _ (TS _ 29) -> cont 29;
	PT _ (TS _ 30) -> cont 30;
	PT _ (TS _ 31) -> cont 31;
	PT _ (TS _ 32) -> cont 32;
	PT _ (TS _ 33) -> cont 33;
	PT _ (TS _ 34) -> cont 34;
	PT _ (TS _ 35) -> cont 35;
	PT _ (TS _ 36) -> cont 36;
	PT _ (TS _ 37) -> cont 37;
	PT _ (TS _ 38) -> cont 38;
	PT _ (TS _ 39) -> cont 39;
	PT _ (TS _ 40) -> cont 40;
	PT _ (TS _ 41) -> cont 41;
	PT _ (TS _ 42) -> cont 42;
	PT _ (TS _ 43) -> cont 43;
	PT _ (TS _ 44) -> cont 44;
	PT _ (TS _ 45) -> cont 45;
	PT _ (TS _ 46) -> cont 46;
	PT _ (TS _ 47) -> cont 47;
	PT _ (TS _ 48) -> cont 48;
	PT _ (TS _ 49) -> cont 49;
	PT _ (TS _ 50) -> cont 50;
	PT _ (TS _ 51) -> cont 51;
	PT _ (TS _ 52) -> cont 52;
	PT _ (TS _ 53) -> cont 53;
	PT _ (TS _ 54) -> cont 54;
	PT _ (TS _ 55) -> cont 55;
	PT _ (TS _ 56) -> cont 56;
	PT _ (TS _ 57) -> cont 57;
	PT _ (TS _ 58) -> cont 58;
	PT _ (TS _ 59) -> cont 59;
	PT _ (TS _ 60) -> cont 60;
	PT _ (TS _ 61) -> cont 61;
	PT _ (TI happy_dollar_dollar) -> cont 62;
	PT _ (TC happy_dollar_dollar) -> cont 63;
	PT _ (TL happy_dollar_dollar) -> cont 64;
	PT _ (TD happy_dollar_dollar) -> cont 65;
	PT _ (TV happy_dollar_dollar) -> cont 66;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 67 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn36 z -> happyReturn z; _other -> notHappyAtAll })

pStmt tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1 tks) (\x -> case x of {HappyAbsSyn46 z -> happyReturn z; _other -> notHappyAtAll })

pRExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

pLExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3 tks) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

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
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghcac8b_0/ghc_2.h" #-}




























































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}



happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr




infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (0), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (0) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = (happyTrace ("state: " ++ show (st) ++                        ",\ttoken: " ++ show (i) ++                       ",\taction: ")) $


          case action of
                (0)           -> (happyTrace ("fail.\n")) $
                                     happyFail (happyExpListPerState ((st) :: Int)) i tk st
                (-1)          -> (happyTrace ("accept.\n")) $
                                     happyAccept i tk st
                n | (n < ((0) :: Int)) -> (happyTrace ("reduce (rule " ++ show rule                                                                ++ ")")) $

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = ((negate ((n + ((1) :: Int)))))
                n                 -> (happyTrace ("shift, enter state "                                                  ++ show (new_state)                                                  ++ "\n")) $


                                     happyShift new_state i tk st
                                     where new_state = (n - ((1) :: Int))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off +  i)
         check  = if (off_i >= ((0) :: Int))
                  then (indexShortOffAddr happyCheck off_i ==  i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st



{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (0) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off +  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   (happyTrace (", goto state " ++ show (new_state) ++ "\n")) $
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off +  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery ((0) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (0) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (0) tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction (0) tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction (0) tk action sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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
