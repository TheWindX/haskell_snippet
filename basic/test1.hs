
module Main where

import System.IO
import Data.Char

myLookup :: (Eq a)=>[(a, b)] -> a -> Maybe b

myLookup al k =
	case al of
		[] -> Nothing
		x@(key, val):xs -> 
			if key == k
				then Just val
				else myLookup xs k




readSimpleIni :: String -> IO [(String, String)]
readSimpleIni fpath = do
	content <- readFile fpath
	let ls = lines content
	return $ (map parseLine $ ls)


parseLine :: String -> (String, String)
parseLine ln = 
	let 
		(first, others) = span (\c -> c /= '=') ln
		truncate s = reverse
			$ dropWhile (\c -> isSpace c) 
			$ reverse 
			$ dropWhile (\c -> isSpace c) s
	
	in (truncate first, truncate $ drop 1 others)



