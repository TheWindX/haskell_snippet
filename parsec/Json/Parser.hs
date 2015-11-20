module Json.Parser where

import qualified Text.ParserCombinators.Parsec as P
import Numeric
import Text.Printf

--parse digit
dec1 :: P.Parser Integer
dec1 = do
	cs <- P.many P.digit
	let num = (read cs) :: Integer
	return num

--parse dec1 "parserName" "323"

--parse digit
dec2 :: P.Parser Integer
dec2 = do
	ssign <- P.optionMaybe $ P.oneOf "+-"
	cs <- P.many P.digit
	let num = (read cs) :: Integer
	let ret = case ssign of
		Just s -> if s == '+'
			then num
			else 0 - num
		_ -> num
	return ret
--parse dec1 "parserName" "-323"

--parse listQuotoString "\"3\"
quotoString1 :: P.Parser String
quotoString1 = do
	P.char '\"'
	str <- P.many $ P.noneOf "\""
	P.char '\"'
	return str

--parse listQuotoString "" "\"3\"     \"4 \" \"5\""
listQuotoString = do
	qs <- P.sepBy quotoString1 P.spaces
	return qs

--parse sep "    , "
sep = do
	P.spaces
	P.optionMaybe (P.char ',')
	P.spaces

--parse listQuotoStringByDot "" "\"3\", \"4 \"   \"5\""
listQuotoStringByDot = do
	qs <- P.sepBy quotoString1 sep
	return qs

{-|
null
bool
number (float)
string
array
map
-}


data Jval = JNull
	|JBool Bool
	|JFloat Float
	|JString String
	|JArray [Jval]
	|JMap [(String, Jval)]
	deriving (Show, Eq)



nullP :: P.Parser Jval
nullP = do
	P.string "null"
	return JNull

boolP :: P.Parser Jval
boolP = (JBool True) <$ P.string "true"
     P.<|> (JBool False) <$ P.string "false"

quotoString :: P.Parser Jval
quotoString = do
	P.char '\"'
	str <- P.many $ P.noneOf "\""
	P.char '\"'
	return $ JString str

numberP :: P.Parser Jval
numberP = do 
	s <- P.getInput
	case (readSigned readFloat s)  of
		[(n, s')] -> ((JFloat n) <$ P.setInput s')
		_ -> P.pzero

arrayP :: P.Parser Jval
arrayP = do
	P.char '['
	P.spaces
	vs <- P.sepBy jvalP sep
	P.spaces
	P.char ']'
	return $ JArray vs

pairP :: P.Parser (String, Jval)
pairP = do
	key <- quotoString
	P.spaces
	P.char ':'
	val <- jvalP
	case key of
		JString str -> return (str, val)
		_ -> return ("", val)

mapP :: P.Parser Jval
mapP = do
	P.char '{'
	P.spaces
	vs <- P.sepBy pairP sep
	P.char '}'
	return $ JMap vs


jvalP :: P.Parser Jval
jvalP = do
	P.spaces
	ret <- nullP
		P.<|> boolP
		P.<|> quotoString
		P.<|> numberP
		P.<|> mapP
		P.<|> arrayP
	P.spaces
	return ret
	

class (Show a) => Builder a where
	iter :: a -> [Either (String, Jval) Jval] 
	bprint :: Int -> a -> String

drop1InBracket ls = 
	case ls of
		[] -> []
		_ -> reverse $ '\n':((drop 2) $ reverse ls)




instance Builder Jval where
	iter v = case v of
		JArray arr -> (Right v):(foldr (\a b -> (iter a) ++ b ) [] arr)
		JMap m -> (Right v):(foldr (\(k, v) b -> ((Left (k, v)):(iter v)) ++ b) [] m)
		_ -> [Right v]

	bprint len v = 
		case v of
			JArray arr -> genSpace ++ "[" ++ "\n"
				++ drop1InBracket (foldl (\acc item-> acc 
					++ (bprint (len+4) item) ++ ",\n" ) "" arr)
				++ genSpace ++ "]"
			JMap m -> genSpace ++ "{" ++ "\n"
				++ drop1InBracket (foldl (\acc (k, v) -> acc ++
					genSpace ++ "    \"" ++ k ++ "\":" ++ (drop (len+4)
					(bprint (len+4) v)) ++ ",\n" ) "" m)
				++ genSpace ++ "}"
			JString str -> genSpace++"\"" ++ str ++ "\""
			JNull -> genSpace++"null"
			JBool b -> case b of
				True -> genSpace++"true"
				False -> genSpace++"false"
			JFloat num -> genSpace++(printf "%.2f" num)
		where genSpace = map (\i->' ') [1..len]

