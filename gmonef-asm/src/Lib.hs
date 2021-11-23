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
	{ instr :: String
	}

parseAsmLine :: String -> AsmLine

