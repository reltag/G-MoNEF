module Main where

import Lib

main :: IO ()
main = do
	putStrLn . ("[INFO] version "++) . show $
		getGMoNEF_version
	putStrLn "TODO: impl" -- not yet written

