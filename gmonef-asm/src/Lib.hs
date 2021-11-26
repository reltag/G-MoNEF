module Lib
    ( getGMoNEF_version
    ) where

{-/ require module "attoparsec" /-}
import Data.Attoparsec.ByteString

{-/ from builtins /-}
import Data.Function
import Data.List

{-/ from builtins "Functor" /-}
import Data.Functor

{-/ from builtins "Applicative" /-}
import Control.Applicative

{-/ from builtins "Monad" /-}
import Control.Monad

-- major.minor.monthyear
data GMoNEFVersion = GMoNEFVersion Int Int Int

getGMoNEF_version :: GMoNEFVersion
getGMoNEF_version =
	GMoNEFVersion 0 0 1121 -- major.minor.monthyear

instance Show GMoNEFVersion where
	show (GMoNEFVersion major minor yearmonth) =
		intercalate "." . map show $
			[major, minor, yearmonth]

-- SEGM asmline-attoparsec

data AsmLine = AsmLine
	{ instructionOp :: AsmOp
	, instructionParams :: [RegID]
	, instructionAlters :: [RegID]
	}

data AsmOp = AsmOp
	{ operator :: AsmOperator
	, redir :: RedirList
	}

data AsmOperator
	= NoOp
	| InvokeInsSet InsSetItem
	| InvokeWinAPI WinAPIItem

type InsSetItem = String

type WinAPIItem = String

data RegID
	= InstructionR InstructionRegister
	| StackPtr StackPtrType
	| RegDeref RegIDPtr

data InstructionRegister = InstructionRegister
	{ extended :: InstructionRegisterExtended
	, refType :: InstructionRegisterRefType
	}

data InstructionRegisterExtended
	= ExtendedEAX
	| ExtendedEBX
	| ExtendedECX
	| ExtendedEDX

data InstructionRegisterRefType
	= RegisterExtended
	| RegisterLow
	| RegisterHigh

data StackPtrType
	= StackPtrNorm
	| StackPtrBase

newtype RegDynamicDeref = RegDynamicDeref
	{ ptr :: RegIDPtr }

data RegIDPtr
	= PtrAt RegID
	| PtrOffset RegIDPtr RegIDPtr
	| PtrRawValue Int

type RedirList = [(RegID, RegID)]

{-/ impl TODO /-}
parseAsmLine :: Parser AsmLine

parseRegID :: Parser RegID
parseRegID =
	(InstructionR <$> parseInstructionRegister)	<|>
	(StackPtr <$> parseStackPtr)			<|>
	(RegDeref <$> parseRegDynamicDeref)

parseRegDynamicDeref :: Parser RegDynamicDeref
parseRegDynamicDeref = do
	char '['
	ptrM <- parseRegIDPtr
	char ']'
	return $ RegDynamicDeref { ptr = ptrM }

parseStackPtr :: Parser StackPtrType
parseStackPtr =
	string <|$>~
		[ ("esp", StackPtrNorm)
		, ("ebp", StackPtrBase)
		]

parseInstructionRegister :: Parser InstructionRegister
parseInstructionRegister 
	string <|$>~
		[ ("eax", InstructionRegister ExtendedEAX RegisterExtended)
		, ("ebx", InstructionRegister ExtendedEBX RegisterExtended)
		, ("ecx", InstructionRegister ExtendedECX RegisterExtended)
		, ("ecx", InstructionRegister ExtendedEDX RegisterExtended)
		, ("al", InstructionRegister ExtendedEAX RegisterLow)
		, ("ah", InstructionRegister ExtendedEAX RegisterHigh)
		, ("bl", InstructionRegister ExtendedEBX RegisterLow)
		, ("bh", InstructionRegister ExtendedEBX RegisterHigh)
		, ("cl", InstructionRegister ExtendedECX RegisterLow)
		, ("ch", InstructionRegister ExtendedECX RegisterHigh)
		, ("dl", InstructionRegister ExtendedEDX RegisterLow)
		, ("dh", InstructionRegister ExtendedEDX RegisterHigh)
		]

(<|$>~) ::
	(Foldable t, Alternative f, Functor f) =>
		(a -> f b) -> t (a, c) -> f c
(<|$>~) f x =
	asum $ uncurry ($>) . first f <$> x

{-
	asum :: (Foldable t, Alternative f) => t (f a) -> f a
	($) :: (a -> b) -> a -> b
	uncurry :: (a -> b -> c) -> (a, b) -> c
	($>) :: (Functor f) => f a -> b -> f b
	(.) :: (b -> c) -> (a -> b) -> a -> c
	first :: (Arrow a) => a b c -> a (b, d) (c, d)
	(<$>) :: (Functor f) => (a -> b) -> f a -> f b
-}

movRedir :: RegID -> RegID -> RedirList
movRedir target x =
	[(x, target)]

xchgRedir :: RegID -> RegID -> RedirList
xchgRedir a b =
	(a, b) : (b, a) : []

