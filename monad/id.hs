module Main where

import Control.Monad.Identity

data ID a = ID {getID::a}

instance Functor ID where
	fmap ff fa = ID $ ff $ getID fa 

instance Applicative ID where
	(<*>) af aa = fmap (getID af) $ aa

instance Monad ID where
	return v = ID v
	(>>=) i f = f (getID i)


testID :: Int -> ID Int
testID n = do
	n1 <- return $ n*2
	n2 <- return $ n1*3
	return $ n2*4


testID2 :: Int -> Identity Int
testID2 n = do
	n1 <- return $ n*3
	n2 <- return $ n1*4
	return $ n2*5

main = do
	putStrLn $ show $ getID $ testID 100


main2 = do
	putStrLn $ show $ runIdentity $ testID2 100