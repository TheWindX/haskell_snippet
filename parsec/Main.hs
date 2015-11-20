
module Main where
import System.Environment
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
	putStrLn content
	
	let pres = P.parse jvalP "" content
	--putStrLn $ show pres
	hClose handle
	case pres of
		Left _ -> return $ JMap []
		Right j -> return j


data EffectInfo = 
	EffectInfo 
	{
		url::String,
		delay:: Float,
		lastTime :: Float,
		bindType :: String,
		scale :: Float
	}
	


getValue :: Jval -> String -> Maybe Jval
getValue jv key = 
	let 
		vals = do
			item <- (iter jv)
			founds <- case item of
				Left (k, v) -> if k == key
					then return [v]
					else return []
				_ -> return []
			return founds
		val = concat vals

	in
		if length val >= 1
			then Just $ (val !! 0)
			else Nothing



genNewEffect :: Jval -> Jval
genNewEffect jv = 
	let
		find = getValue jv
		murl = case find "Url" of
			Just v -> case v of
				JString s -> s
				_ -> ""
			_ -> ""
		mdelay = case find "Delay" of
			Just v -> case v of
				JFloat num -> num
				_ -> 0
			_ -> 0
		mLastTime = case find "LastTime" of
			Just v -> case v of
				JFloat num -> num
				_ -> 2
			_ -> 2
		mScale = case find "ScaleX" of
			Just v -> case v of
				JFloat num -> num
				_ -> 1
			_ -> 1
	in
		JArray [
			JMap 
			[
				("Url", JString murl), 
				("BindType", JString "eFoot"),
				("BoneName", JString ""),
				("Frames",
					JArray 
					[
						JMap [
							("TweenType", JFloat 0),
							("Visible", JBool True),
							("PositionX", JFloat 0),
							("PositionY", JFloat 0),
							("PositionZ", JFloat 0),
							("ScaleX", JFloat mScale),
							("ScaleY", JFloat mScale),
							("ScaleZ", JFloat mScale),
							("RotationX", JFloat 0),
							("RotationY", JFloat 0),
							("RotationZ", JFloat 0),
							("Delay",JFloat mdelay)
						],
						JMap [
							("TweenType", JFloat 0),
							("Visible", JBool False),
							("PositionX", JFloat 0),
							("PositionY", JFloat 0),
							("PositionZ", JFloat 0),
							("ScaleX", JFloat mScale),
							("ScaleY", JFloat mScale),
							("ScaleZ", JFloat mScale),
							("RotationX", JFloat 0),
							("RotationY", JFloat 0),
							("RotationZ", JFloat 0),
							("Delay",JFloat (mdelay + mLastTime))
						]
					]
				)
			]
		]


changeFile :: Path -> IO ()
changeFile path = do
	jv <- file2Json path
	case jv of
		JMap m -> do
			let newMap = foldr (\item acc-> case item of
					("OldEffects", v) -> 
						case v of
							JArray [] -> acc
							_ -> ("Effects", genNewEffect v):acc
					other -> case other of
						(_, v) -> case v of
							JArray [] -> acc
							t -> other:acc
					) [] m
			
			handle <- openFile path WriteMode
			hSetEncoding handle utf8_bom
			hPutStr handle $ bprint 0 (JMap newMap)
			hClose handle
			--writeFile path $ bprint 0 (JMap newMap)
			--putStrLn $ bprint 0 (JMap newMap)
			--let oe = lookup "OldEffects" m
			--case oe of
			--	Just oeval -> 
			--		putStrLn $ show oeval
			--	Nothing -> putStrLn "nothing"
		_ -> putStrLn "nothing"

main = do
	args <- getArgs
	if length args == 1
		then do
			files <- findAllEft $ args !! 0
			forM_ files (\f -> do putStrLn f; changeFile f)
		else
			putStrLn "Usage: *.exe <Directory>"
	
	
	--return $ flip map files (\f -> changeFile f)

test1 = do
	let (Right jv) = P.parse jvalP "" "[\n\t3\n, [3, 4, 5], {\"a\":\"[1,2,3]\",\"b\":4,\"c\":4}, 5]"
	putStrLn $ bprint 0 jv

test2 = do
	changeFile "attack1_1000.eft"


