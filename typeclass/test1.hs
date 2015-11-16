module Main where

data MyMaybe a = MJ a | MN


instance Functor MyMaybe where
	fmap f fa = 
		case fa of
			(MJ a) -> MJ $ f a
			MN -> MN

instance Applicative MyMaybe where
	pure a = MJ a
	(<*>) ff fa = case ff of
		MJ f -> case fa of
			MJ a -> MJ $ f a
			MN -> MN
		MN -> MN

instance Monad MyMaybe where
	(>>=) (MJ _a) f = f _a
	(>>=) MN f = MN

	return a = MJ a


instance Show a => Show (MyMaybe a) where
	show (MJ n) = show n
	show MN = "Nothing"

doubleM :: (Num n)=>n->MyMaybe n
doubleM n = do
	k <- MJ n
	return $ k*2


main = case (doubleM 1000) of
		MJ n -> putStrLn $ show n
		MN -> putStrLn "nothing"

