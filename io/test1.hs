
import System.IO
import System.Directory
import Control.DeepSeq
import Control.Monad

data FileItem = Fold {path::String} | File {path::String} deriving (Show) 

type Path = String



dirCurrent :: String -> IO ()
dirCurrent path = do
	paths <- getDirectoryContents path
	let topPath = path
	paths <- return $ filter (\item -> notElem item [".", ".."]) paths
	forM_ paths (\item -> putStrLn item)
	return ()



myForM :: [String] -> (String -> IO String) -> IO [String]
myForM items fb = do
	let mbs = fmap fb items
	--seq :: [Monad b]->Monad [b]
	let seq = \ms -> case (ms::[IO String]) of
		[] -> return []
		(mx:mxs) -> do
			x <- mx
			xs <- (seq mxs)::(IO [String])
			return $ x:xs
	seq mbs


myForM_ :: [String] -> (String -> IO ()) -> IO ()
myForM_ items fb = do
	let mbs = fmap fb items
	--seq :: [Monad b]->Monad [b]
	let seq = \ms -> case (ms::[IO ()]) of
		[] -> return ()
		(mx:mxs) -> do
			mx
			seq mxs
	seq mbs


dirCurrent1 :: String -> IO ()
dirCurrent1 path = do
	paths <- getDirectoryContents path
	let topPath = path
	paths <- return $ filter (\item -> notElem item [".", ".."]) paths
	myForM_ paths (\item -> putStrLn item)
	return ()

-- this show 
dirCurrent2 :: String -> IO ()
dirCurrent2 path = do
	paths <- getDirectoryContents path
	let topPath = path
	paths <- return $ filter (\item -> notElem item [".", ".."]) paths
	items <- forM paths 
		(\path -> do
			let longpath = topPath ++ "/" ++ path
			isDir <- doesDirectoryExist longpath
			if isDir
				then return $ Fold longpath
				else return $ File longpath)
	forM_ items (\item -> putStrLn $ show item) 