module Lib
    ( getGMoNEF_version
    ) where

import Data.Function
import Data.List

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
	, redir :: [(RegID, RegID)]
	}

data AsmOperator
	= NoOp
	| InvokeInsSet InsSetItem
	| InvokeWinAPI WinAPIItem

type InsSetItem = String

type WinAPIItem = String

data RegID
	= InstructionR InstructionRegister
	| Deref RegIDPtr

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

data RegIDPtr
	= PtrAt RegID
	| PtrOffset RegIDPtr RegIDPtr
	| PtrRawValue Int

parseAsmLine :: String -> AsmLine

