
module Main where
import Control.Monad
import Json.Parser
import System.IO
import System.Directory
import Data.List
import Data.Text.Encoding

import qualified Text.ParserCombinators.Parsec as P

type Path = String

--目录下所有 .eft
findAllEft :: Path -> IO [Path]
findAllEft path = do
	items <- getDirectoryContents path
	efts <- forM items (\item -> do
		--putStrLn item
		if item == ".." || item == "."
			then return []
			else do
				let rpath = path ++ "\\" ++ (item::String)
				isDir <- doesDirectoryExist rpath
				if isDir
					then findAllEft rpath
					else do
						--putStrLn $ "here:"++rpath++", "++(show $ 	isSuffixOf ".eft" rpath)
						if isSuffixOf ".eft" rpath
						then return [rpath]
						else return [])

	return $ Data.List.concat efts


file2Json :: Path -> IO Jval
file2Json p = do
	handle <- openFile p ReadMode
	hSetEncoding handle utf8_bom
	len <- hFileSize handle
	putStrLn $ show len
	
	content <- hGetContents handle
	putStrLn $ content
	
	let pres = P.parse jvalP "" content
	
	hClose handle
	case pres of
		Left _ -> return $ JMap []
		Right j -> return j


getURL :: Jval -> Maybe String
getURL v = 
	let 
		urlss = do
			item <- (iter v)
			case item of
				Left (k, v) -> if k == "Url"
					then case v of
						JString s -> return [s]
						_ -> return []
					else return []
				_ -> return []
		urls = concat urlss
	in 
		if (length urls) >= 1
			then Just $ (urls !! 0)
			else Nothing



--changeOldEffect :: Jval -> Jval
--changeOldEffect = case item of
--					("OldEffects", v) -> ("Effects", Jnull)
--					_ -> item
--					) 


changeFile :: Path -> IO ()
changeFile path = do
	jv <- file2Json path
	case jv of
		JMap m -> do
			let newMap = foldr (\item acc-> case item of
					("OldEffects", v) -> 
						let url = getURL v
						in case url of
							Just u -> ("Effects", JString u):acc
							Nothing -> acc
					(_:'1':[], v) -> if v == JArray []
						then acc
						else (item:acc)
					_ -> item:acc) [] m

			putStrLn $ show $ JMap newMap
			--let oe = lookup "OldEffects" m
			--case oe of
			--	Just oeval -> 
			--		putStrLn $ show oeval
			--	Nothing -> putStrLn "nothing"
		_ -> putStrLn "nothing"

main = do
	let r = P.parse jvalP "" "[3, 4, 5]"
	putStrLn $ show r
