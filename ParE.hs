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

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Integer)
	| HappyAbsSyn5 (Double)
	| HappyAbsSyn6 (Char)
	| HappyAbsSyn7 (String)
	| HappyAbsSyn8 (PIdent)
	| HappyAbsSyn9 (Program)
	| HappyAbsSyn10 ([Decl])
	| HappyAbsSyn11 ([AnnDecl])
	| HappyAbsSyn12 (AnnDecl)
	| HappyAbsSyn13 (Decl)
	| HappyAbsSyn14 ([Arg])
	| HappyAbsSyn15 ([Stmt])
	| HappyAbsSyn16 (Arg)
	| HappyAbsSyn17 (Modality)
	| HappyAbsSyn18 (Guard)
	| HappyAbsSyn19 (Stmt)
	| HappyAbsSyn20 (NormCase)
	| HappyAbsSyn21 (DfltCase)
	| HappyAbsSyn22 ([NormCase])
	| HappyAbsSyn23 ([DfltCase])
	| HappyAbsSyn24 (Expr)
	| HappyAbsSyn25 (LExpr)
	| HappyAbsSyn26 (Ref)
	| HappyAbsSyn44 ([Expr])
	| HappyAbsSyn45 (AssignOperator)
	| HappyAbsSyn46 (Type)
	| HappyAbsSyn47 (CompoundType)
	| HappyAbsSyn48 (TypeIter)
	| HappyAbsSyn49 (EndLine)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,1651) ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,38018,4,29504,50920,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,9728,45206,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,1,12,0,0,0,0,0,0,0,0,0,0,0,0,0,768,0,0,0,0,0,0,0,1728,0,0,0,0,16384,0,4096,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5120,0,4096,49216,7,0,0,0,1,0,0,64,0,0,33280,1172,0,16400,1984,0,0,0,256,0,0,16384,0,0,0,5120,0,4096,49216,7,0,0,16384,1,0,1025,124,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,64,0,0,0,0,0,0,38018,4,4096,49216,7,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,32768,0,16384,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33280,1172,16384,59507,2022,0,0,8192,18760,0,256,31748,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,16,0,0,1024,0,0,8192,18760,0,256,31748,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,33280,1172,0,16400,1984,0,0,0,0,0,2048,0,0,0,0,1024,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,38018,4,4096,49216,7,0,0,18464,73,0,1025,124,0,0,33280,1172,0,16400,1984,0,0,8192,18760,0,256,31748,0,0,0,38018,4,4096,49216,7,0,0,18464,73,0,1025,124,0,0,33280,1172,0,16400,1984,0,0,8192,18760,0,256,31748,0,0,0,38018,4,4096,49216,7,0,0,18464,73,0,1025,124,0,0,33280,1172,0,16400,1984,0,0,8192,18760,0,256,31748,0,0,0,38018,4,4096,49216,7,0,0,18464,73,0,1025,124,0,0,33280,1172,0,16400,1984,0,0,8192,18760,0,256,31748,0,0,0,38018,4,4096,49216,7,0,0,18464,73,0,1025,124,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,8,0,0,0,2048,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,1,12,0,0,0,0,0,24,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,48,0,0,0,0,0,0,768,0,0,0,0,0,0,12288,0,0,0,0,0,0,0,27648,0,0,0,0,0,0,49152,6,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,8192,2048,0,0,0,33280,1172,43008,20756,1985,0,0,8192,18760,32768,256,31748,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,33280,1172,0,16400,1984,0,0,0,128,0,0,0,0,0,0,0,0,16,0,0,0,0,0,32768,2,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,38018,4,29504,50920,7,0,0,0,0,0,0,0,0,0,33280,1172,0,16400,1984,0,0,0,0,64,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,33280,1172,16384,59507,1990,0,0,0,0,3,0,0,0,0,0,38018,4,29504,50920,7,0,0,18464,73,0,1025,124,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,38018,4,4096,49216,7,0,0,0,16384,0,0,0,0,0,0,0,0,32770,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,38018,4,29504,50920,7,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8196,0,0,0,18464,73,0,1025,124,0,0,33280,1172,16384,59507,1990,0,0,0,0,0,0,0,0,0,0,38018,4,29504,50920,7,0,0,0,0,0,0,0,0,0,33280,1172,16384,59507,2022,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Integer","Double","Char","String","PIdent","Program","ListDecl","ListAnnDecl","AnnDecl","Decl","ListArg","ListStmt","Arg","Modality","Guard","Stmt","NormCase","DfltCase","ListNormCase","ListDfltCase","Expr","LExpr","Ref","Expr17","Expr16","Expr15","Expr14","Expr12","Expr11","Expr10","Expr9","Expr8","Expr4","Expr3","Expr1","Expr2","Expr5","Expr6","Expr7","Expr13","ListExpr","AssignOperator","Type","CompoundType","TypeIter","EndLine","'!'","'!='","'%'","'%%'","'%%='","'%='","'&'","'&&'","'&='","'('","')'","'*'","'**='","'*='","'+'","'+='","','","'-'","'-='","'..'","'..!'","'/'","'//'","'//='","'/='","':'","':='","':]'","';'","'<'","'<='","'=='","'>'","'>='","'['","']'","'bool'","'break'","'char'","'continue'","'def'","'double'","'else'","'false'","'for'","'if'","'in'","'int'","'match'","'match _'","'return'","'string'","'switch'","'true'","'var'","'void'","'while'","'{'","'|='","'||'","'}'","L_integ","L_doubl","L_charac","L_quoted","L_PIdent","%eof"]
        bit_start = st * 116
        bit_end = (st + 1) * 116
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..115]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,199) ([0,-50,0,-45,133,0,0,0,0,7,0,0,24,90,0,0,0,0,0,11,22,23,131,-1,27,-30,0,0,0,0,0,0,26,-6,284,-6,26,26,0,0,-6,0,9,51,284,62,0,-9,68,0,0,0,0,0,1,284,0,55,-6,284,85,81,284,37,-5,0,0,0,0,105,0,0,284,284,284,284,284,284,284,284,284,284,284,284,284,284,284,284,284,284,0,0,0,0,0,0,0,0,0,0,0,0,0,-37,108,106,60,0,0,0,0,0,0,0,0,11,11,22,22,23,23,23,23,131,131,0,119,0,114,-37,253,268,143,0,284,144,127,44,159,0,193,0,284,116,109,0,193,23,193,284,160,0,0,0,0,0,0,0,0,164,284,150,-37,0,0,0,151,155,0,146,0,-6,142,0,154,154,0,0,0,152,193,163,0,148,0,0,0,0,-41,284,193,0,193,0,67,0,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,199) ([92,0,0,0,522,0,0,0,0,0,0,0,147,161,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,135,3,842,6,1553,1584,0,0,28,0,189,0,399,0,0,65,0,198,0,0,0,0,522,882,0,195,72,922,0,162,962,0,196,0,0,0,0,0,0,0,1162,1202,1242,1273,1304,1313,1344,1353,1384,1393,1424,1433,1464,1473,1504,1513,1544,1122,0,0,0,0,0,0,0,0,0,0,0,0,0,134,0,0,208,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,140,339,369,0,0,440,0,0,168,0,0,562,0,1002,200,0,0,602,0,642,481,0,0,0,0,0,0,0,0,0,0,1042,201,156,0,0,0,171,203,0,0,0,79,0,202,173,174,0,0,0,32,682,207,0,0,0,211,0,0,206,1082,722,0,762,0,802,0,0,0,0,0
	])

happyAdjustOffset :: Int -> Int
happyAdjustOffset = id

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,199) ([-8,0,-2,0,-7,-54,-55,-56,-57,-49,-9,-15,0,-52,-50,-60,-62,-67,-97,-76,-79,-84,-87,-96,-91,-93,-48,-92,-89,-94,-95,-73,0,0,0,0,0,0,-37,-38,0,-59,0,0,-98,0,-58,0,0,-8,-3,-4,-5,-6,0,0,-49,-25,0,0,-99,0,0,0,-25,-52,-65,-66,-51,0,-64,-63,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-110,-109,-103,-111,-106,-104,-105,-107,-108,-101,-102,-27,-123,-16,0,-17,0,-24,-23,-47,-70,-69,-68,-72,-71,-75,-74,-78,-77,-83,-81,-82,-80,-85,-86,-88,-90,-53,0,-16,0,0,0,-32,-98,0,0,0,0,-33,0,-28,0,-25,0,-100,0,-121,0,-98,-26,-118,-120,-112,-116,-113,-114,-117,-115,0,0,-25,-16,-61,-18,-21,0,-25,-119,0,-40,0,-35,-43,0,0,-39,-30,-29,-45,0,-25,-122,0,-31,-19,-34,-44,0,0,0,-46,0,-36,0,-20,-14,-42,-41
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,1651) ([-1,2,1,12,41,10,12,4,7,50,4,10,62,12,3,4,15,10,55,18,61,26,67,12,21,22,35,21,22,66,60,32,4,22,23,8,10,15,12,38,18,40,41,20,21,44,45,46,16,21,22,19,51,29,53,54,55,66,57,58,66,10,61,62,63,64,65,66,1,4,44,27,10,29,7,66,4,10,10,12,54,26,15,4,47,18,21,22,62,63,64,65,66,21,22,5,6,5,6,9,21,22,17,13,14,38,16,40,41,19,29,44,45,46,24,25,11,27,51,11,53,54,55,17,57,58,66,8,61,62,63,64,65,66,1,0,1,2,3,4,7,27,26,10,10,12,12,13,15,59,10,18,12,13,11,11,21,22,23,24,25,30,31,36,33,34,10,58,12,13,11,38,12,40,41,11,26,44,45,46,29,26,36,29,51,43,53,54,55,26,57,58,45,4,1,62,63,64,65,66,7,49,41,10,6,12,58,45,15,14,14,18,4,45,14,14,45,14,45,45,18,14,11,17,-1,-1,-1,-1,-1,-1,-1,38,-1,40,41,-1,-1,44,45,46,-1,-1,-1,-1,51,-1,53,54,55,-1,57,58,-1,-1,1,62,63,64,65,66,7,-1,-1,10,-1,12,-1,-1,15,1,-1,18,-1,-1,-1,7,-1,-1,10,-1,12,-1,-1,15,-1,1,18,-1,35,-1,37,7,39,-1,10,42,12,44,-1,15,-1,48,18,35,-1,52,-1,54,-1,56,-1,-1,44,-1,-1,62,63,64,65,66,-1,-1,54,-1,-1,-1,-1,-1,44,-1,62,63,64,65,66,-1,-1,-1,54,0,1,2,3,4,-1,-1,62,63,64,65,66,-1,-1,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,0,1,2,3,4,-1,-1,-1,-1,39,-1,-1,42,43,44,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,0,1,2,3,4,-1,-1,-1,-1,39,-1,-1,-1,-1,44,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,0,1,2,3,4,-1,-1,-1,-1,9,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,30,31,32,-1,-1,-1,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,30,31,-1,-1,-1,-1,36,37,38,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,30,0,1,2,3,4,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,30,0,1,2,3,4,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,21,22,23,24,25,26,27,28,29,39,0,1,2,3,4,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,29,21,22,23,24,25,26,27,28,29,39,0,1,2,3,4,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,28,-1,21,22,23,24,25,26,27,28,-1,39,0,1,2,3,4,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,27,-1,-1,21,22,23,24,25,26,27,-1,-1,39,0,1,2,3,4,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,-1,-1,-1,21,22,23,24,25,26,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,-1,-1,-1,21,22,23,24,25,26,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,26,-1,-1,-1,21,22,23,24,25,-1,-1,-1,-1,39,0,1,2,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,21,22,23,24,25,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,1651) ([0,75,33,36,108,130,36,56,34,193,56,35,3,36,85,86,37,104,109,38,194,131,-1,87,70,14,59,68,14,-22,73,76,56,88,89,74,35,83,36,39,84,40,41,81,82,42,43,44,187,64,14,188,45,103,46,47,48,54,49,50,54,63,140,3,51,52,53,54,33,56,42,143,60,103,34,54,56,35,56,36,47,131,37,56,132,38,57,14,3,51,52,53,54,136,14,91,92,3,4,93,181,14,135,94,95,39,96,40,172,97,103,42,43,44,98,99,128,100,45,164,46,47,48,163,49,50,54,74,197,3,51,52,53,54,33,5,6,7,8,9,34,161,131,35,104,36,105,106,37,101,159,38,105,106,147,145,65,14,15,16,71,77,78,144,79,80,164,174,105,106,141,39,169,40,41,168,131,42,43,44,103,131,183,103,45,181,46,47,48,131,49,50,101,63,33,3,51,52,53,54,34,190,89,35,54,36,186,133,37,137,128,38,161,141,174,165,184,183,178,177,179,128,194,191,0,0,0,0,0,0,0,39,0,40,172,0,0,42,43,44,0,0,0,0,45,0,46,47,48,0,49,50,0,0,33,3,51,52,53,54,34,0,0,35,0,36,0,0,37,33,0,38,0,0,0,34,0,0,35,0,36,0,0,37,0,33,38,0,150,0,154,34,155,0,35,156,36,42,0,37,0,157,38,150,0,158,0,47,0,159,0,0,42,0,0,3,51,52,53,54,0,0,47,0,0,0,0,0,42,0,3,51,52,53,54,0,0,0,47,5,6,7,8,9,0,0,3,51,52,53,54,0,0,0,0,0,0,0,0,0,65,14,15,16,17,18,19,20,147,5,6,7,8,9,0,0,0,0,31,0,0,150,151,152,0,0,0,0,0,0,65,14,15,16,17,18,19,20,147,5,6,7,8,9,0,0,0,0,31,0,0,0,0,148,0,0,0,0,0,60,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,61,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,60,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,145,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,60,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,169,5,6,7,8,9,0,0,0,0,10,0,0,0,0,0,11,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,176,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,172,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,170,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,186,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,198,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,197,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,195,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,69,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,138,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,135,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,132,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,175,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,166,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,190,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65,14,15,16,17,18,19,20,21,22,23,24,25,109,27,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65,14,15,16,17,18,19,20,21,22,23,126,0,0,0,28,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65,14,15,16,17,18,19,20,21,22,23,0,0,0,0,125,29,30,31,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65,14,15,16,17,18,19,20,21,124,5,6,7,8,9,0,0,0,31,0,0,0,0,0,0,0,0,0,0,0,0,65,14,15,16,17,18,19,20,21,123,5,6,7,8,9,0,0,0,31,5,6,7,8,9,0,0,0,0,0,0,0,65,14,15,16,17,18,19,20,122,65,14,15,16,17,18,19,20,121,31,5,6,7,8,9,0,0,0,31,5,6,7,8,9,0,0,0,0,0,0,0,65,14,15,16,17,18,19,20,120,65,14,15,16,17,18,19,20,119,31,5,6,7,8,9,0,0,0,31,5,6,7,8,9,0,0,0,0,0,0,0,65,14,15,16,17,18,19,118,0,65,14,15,16,17,18,19,117,0,31,5,6,7,8,9,0,0,0,31,5,6,7,8,9,0,0,0,0,0,0,0,65,14,15,16,17,18,116,0,0,65,14,15,16,17,18,115,0,0,31,5,6,7,8,9,0,0,0,31,5,6,7,8,9,0,0,0,0,0,0,0,65,14,15,16,17,18,0,0,0,65,14,15,16,17,18,0,0,0,114,5,6,7,8,9,0,0,0,113,5,6,7,8,9,0,0,0,0,0,0,0,65,14,15,16,17,18,0,0,0,65,14,15,16,17,18,0,0,0,112,5,6,7,8,9,0,0,0,111,5,6,7,8,9,0,0,0,0,0,0,0,65,14,15,16,17,18,0,0,0,65,14,15,16,67,0,0,0,0,110,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65,14,15,16,66,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

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

happy_n_terms = 68 :: Int
happy_n_nonterms = 46 :: Int

happyReduce_1 = happySpecReduce_1  0 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn4
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  1 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  2 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn6
		 ((read ( happy_var_1)) :: Char
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  3 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (PIdent (mkPosToken happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (AbsE.PDefs (reverse happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  6 happyReduction_7
happyReduction_7  =  HappyAbsSyn10
		 ([]
	)

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  7 happyReduction_9
happyReduction_9  =  HappyAbsSyn11
		 ([]
	)

happyReduce_10 = happySpecReduce_2  7 happyReduction_10
happyReduction_10 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (AbsE.LabeledDecl happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AbsE.AnnotatedDecl happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 9 9 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (AbsE.DeclFun happy_var_2 happy_var_4 happy_var_6 (reverse happy_var_8)
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn13
		 (AbsE.DeclStmt happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  10 happyReduction_15
happyReduction_15  =  HappyAbsSyn14
		 ([]
	)

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 ((:[]) happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  11 happyReduction_18
happyReduction_18  =  HappyAbsSyn15
		 ([]
	)

happyReduce_19 = happySpecReduce_2  11 happyReduction_19
happyReduction_19 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyAbsSyn18  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (AbsE.ArgDecl happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  13 happyReduction_21
happyReduction_21  =  HappyAbsSyn17
		 (AbsE.ModEmpty
	)

happyReduce_22 = happySpecReduce_1  13 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn17
		 (AbsE.ModVar
	)

happyReduce_23 = happySpecReduce_1  13 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn17
		 (AbsE.ModDef
	)

happyReduce_24 = happySpecReduce_0  14 happyReduction_24
happyReduction_24  =  HappyAbsSyn18
		 (AbsE.GuardVoid
	)

happyReduce_25 = happySpecReduce_2  14 happyReduction_25
happyReduction_25 (HappyAbsSyn46  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (AbsE.GuardType happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  15 happyReduction_26
happyReduction_26 (HappyAbsSyn49  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsE.StmtExpr happy_var_1 happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 15 happyReduction_27
happyReduction_27 ((HappyAbsSyn49  happy_var_4) `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.StmtDecl happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 15 happyReduction_28
happyReduction_28 ((HappyAbsSyn49  happy_var_6) `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.StmtIterDecl happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 6 15 happyReduction_29
happyReduction_29 ((HappyAbsSyn49  happy_var_6) `HappyStk`
	(HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.StmtVarInit happy_var_2 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 6 15 happyReduction_30
happyReduction_30 ((HappyAbsSyn49  happy_var_6) `HappyStk`
	(HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.StmtDefInit happy_var_2 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 (HappyAbsSyn49  happy_var_3)
	(HappyAbsSyn44  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (AbsE.StmtReturn happy_var_2 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  15 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (AbsE.StmtBlock (reverse happy_var_2)
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 7 15 happyReduction_33
happyReduction_33 ((HappyAbsSyn19  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.StmtIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 5 15 happyReduction_34
happyReduction_34 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.StmtIfNoElse happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 8 15 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_7) `HappyStk`
	(HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.SSwitchCase happy_var_3 (reverse happy_var_6) (reverse happy_var_7)
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  15 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn19
		 (AbsE.StmtBreak
	)

happyReduce_37 = happySpecReduce_1  15 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn19
		 (AbsE.StmtContinue
	)

happyReduce_38 = happyReduce 5 15 happyReduction_38
happyReduction_38 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.StmtWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 5 15 happyReduction_39
happyReduction_39 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	(HappyAbsSyn48  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsE.StmtFor happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  16 happyReduction_40
happyReduction_40 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (AbsE.CaseNormal happy_var_2 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  17 happyReduction_41
happyReduction_41 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (AbsE.CaseDefault happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  18 happyReduction_42
happyReduction_42  =  HappyAbsSyn22
		 ([]
	)

happyReduce_43 = happySpecReduce_2  18 happyReduction_43
happyReduction_43 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_0  19 happyReduction_44
happyReduction_44  =  HappyAbsSyn23
		 ([]
	)

happyReduce_45 = happySpecReduce_2  19 happyReduction_45
happyReduction_45 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  20 happyReduction_46
happyReduction_46 (HappyAbsSyn24  happy_var_3)
	(HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.StmtAssign happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  20 happyReduction_47
happyReduction_47 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  21 happyReduction_48
happyReduction_48 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn25
		 (AbsE.LExprId happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  21 happyReduction_49
happyReduction_49 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (AbsE.LExprRef happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  22 happyReduction_50
happyReduction_50 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (AbsE.RefExpr happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  23 happyReduction_51
happyReduction_51 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.LeftExpr happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  23 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  24 happyReduction_53
happyReduction_53 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprInt happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  24 happyReduction_54
happyReduction_54 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprDouble happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  24 happyReduction_55
happyReduction_55 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprChar happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  24 happyReduction_56
happyReduction_56 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprString happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  24 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn24
		 (AbsE.ExprTrue
	)

happyReduce_58 = happySpecReduce_1  24 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn24
		 (AbsE.ExprFalse
	)

happyReduce_59 = happySpecReduce_1  24 happyReduction_59
happyReduction_59 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happyReduce 4 25 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (AbsE.ExprFunCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_1  25 happyReduction_61
happyReduction_61 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  26 happyReduction_62
happyReduction_62 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (AbsE.ExprBoolNot happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  26 happyReduction_63
happyReduction_63 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (AbsE.ExprDeref happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  26 happyReduction_64
happyReduction_64 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (AbsE.ExprNegation happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  26 happyReduction_65
happyReduction_65 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (AbsE.ExprAddition happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  26 happyReduction_66
happyReduction_66 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  27 happyReduction_67
happyReduction_67 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprMul happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  27 happyReduction_68
happyReduction_68 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprFloatDiv happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  27 happyReduction_69
happyReduction_69 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprIntDiv happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  27 happyReduction_70
happyReduction_70 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprReminder happy_var_1 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  27 happyReduction_71
happyReduction_71 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprModulo happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  27 happyReduction_72
happyReduction_72 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  28 happyReduction_73
happyReduction_73 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprPlus happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  28 happyReduction_74
happyReduction_74 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprMinus happy_var_1 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  28 happyReduction_75
happyReduction_75 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  29 happyReduction_76
happyReduction_76 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprIntInc happy_var_1 happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  29 happyReduction_77
happyReduction_77 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprIntExc happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  29 happyReduction_78
happyReduction_78 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  30 happyReduction_79
happyReduction_79 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprLt happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  30 happyReduction_80
happyReduction_80 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprGt happy_var_1 happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  30 happyReduction_81
happyReduction_81 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprLtEq happy_var_1 happy_var_3
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  30 happyReduction_82
happyReduction_82 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprGtEq happy_var_1 happy_var_3
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  30 happyReduction_83
happyReduction_83 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  31 happyReduction_84
happyReduction_84 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprEq happy_var_1 happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_3  31 happyReduction_85
happyReduction_85 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprNeq happy_var_1 happy_var_3
	)
happyReduction_85 _ _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  31 happyReduction_86
happyReduction_86 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3  32 happyReduction_87
happyReduction_87 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprAnd happy_var_1 happy_var_3
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  32 happyReduction_88
happyReduction_88 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  33 happyReduction_89
happyReduction_89 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (AbsE.ExprOr happy_var_1 happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  33 happyReduction_90
happyReduction_90 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  34 happyReduction_91
happyReduction_91 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  35 happyReduction_92
happyReduction_92 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  36 happyReduction_93
happyReduction_93 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  37 happyReduction_94
happyReduction_94 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  38 happyReduction_95
happyReduction_95 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  39 happyReduction_96
happyReduction_96 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_0  40 happyReduction_97
happyReduction_97  =  HappyAbsSyn44
		 ([]
	)

happyReduce_98 = happySpecReduce_1  40 happyReduction_98
happyReduction_98 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn44
		 ((:[]) happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  40 happyReduction_99
happyReduction_99 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn44
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  41 happyReduction_100
happyReduction_100 _
	 =  HappyAbsSyn45
		 (AbsE.OpAssign
	)

happyReduce_101 = happySpecReduce_1  41 happyReduction_101
happyReduction_101 _
	 =  HappyAbsSyn45
		 (AbsE.OpOr
	)

happyReduce_102 = happySpecReduce_1  41 happyReduction_102
happyReduction_102 _
	 =  HappyAbsSyn45
		 (AbsE.OpAnd
	)

happyReduce_103 = happySpecReduce_1  41 happyReduction_103
happyReduction_103 _
	 =  HappyAbsSyn45
		 (AbsE.OpPlus
	)

happyReduce_104 = happySpecReduce_1  41 happyReduction_104
happyReduction_104 _
	 =  HappyAbsSyn45
		 (AbsE.OpMinus
	)

happyReduce_105 = happySpecReduce_1  41 happyReduction_105
happyReduction_105 _
	 =  HappyAbsSyn45
		 (AbsE.OpMul
	)

happyReduce_106 = happySpecReduce_1  41 happyReduction_106
happyReduction_106 _
	 =  HappyAbsSyn45
		 (AbsE.OpIntDiv
	)

happyReduce_107 = happySpecReduce_1  41 happyReduction_107
happyReduction_107 _
	 =  HappyAbsSyn45
		 (AbsE.OpFloatDiv
	)

happyReduce_108 = happySpecReduce_1  41 happyReduction_108
happyReduction_108 _
	 =  HappyAbsSyn45
		 (AbsE.OpRemainder
	)

happyReduce_109 = happySpecReduce_1  41 happyReduction_109
happyReduction_109 _
	 =  HappyAbsSyn45
		 (AbsE.OpModulo
	)

happyReduce_110 = happySpecReduce_1  41 happyReduction_110
happyReduction_110 _
	 =  HappyAbsSyn45
		 (AbsE.OpPower
	)

happyReduce_111 = happySpecReduce_1  42 happyReduction_111
happyReduction_111 _
	 =  HappyAbsSyn46
		 (AbsE.TypeBool
	)

happyReduce_112 = happySpecReduce_1  42 happyReduction_112
happyReduction_112 _
	 =  HappyAbsSyn46
		 (AbsE.TypeDouble
	)

happyReduce_113 = happySpecReduce_1  42 happyReduction_113
happyReduction_113 _
	 =  HappyAbsSyn46
		 (AbsE.TypeInt
	)

happyReduce_114 = happySpecReduce_1  42 happyReduction_114
happyReduction_114 _
	 =  HappyAbsSyn46
		 (AbsE.TypeVoid
	)

happyReduce_115 = happySpecReduce_1  42 happyReduction_115
happyReduction_115 _
	 =  HappyAbsSyn46
		 (AbsE.TypeChar
	)

happyReduce_116 = happySpecReduce_1  42 happyReduction_116
happyReduction_116 _
	 =  HappyAbsSyn46
		 (AbsE.TypeString
	)

happyReduce_117 = happySpecReduce_1  42 happyReduction_117
happyReduction_117 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn46
		 (AbsE.TypeCompound happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_2  43 happyReduction_118
happyReduction_118 _
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn47
		 (AbsE.TypePointer happy_var_1
	)
happyReduction_118 _ _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  43 happyReduction_119
happyReduction_119 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn47
		 (AbsE.TypeIterable happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  44 happyReduction_120
happyReduction_120 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn48
		 (AbsE.TypeIterInterval happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_3  44 happyReduction_121
happyReduction_121 _
	(HappyAbsSyn44  happy_var_2)
	_
	 =  HappyAbsSyn48
		 (AbsE.TypeIterArray happy_var_2
	)
happyReduction_121 _ _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  45 happyReduction_122
happyReduction_122 _
	 =  HappyAbsSyn49
		 (AbsE.Semicolon
	)

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
	PT _ (TD happy_dollar_dollar) -> cont 63;
	PT _ (TC happy_dollar_dollar) -> cont 64;
	PT _ (TL happy_dollar_dollar) -> cont 65;
	PT _ (T_PIdent _) -> cont 66;
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
 happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

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
{-# LINE 9 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc7936_0/ghc_2.h" #-}




















































































































































































{-# LINE 9 "<command-line>" #-}
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
