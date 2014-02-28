module Json.Internal where

import Data.Char (isSpace)
import qualified Text.Parse as Parse
import qualified Text.ParserCombinators.Poly.Plain as PolyPlain
import qualified Text.ParserCombinators.Poly.Base as PolyBase

comp2 :: (a -> b) -> (c -> d -> a) -> c -> d -> b
comp2 f1 f2 x y = f1 $ f2 x y

runParser :: Parse.TextParser a -> String -> Either String a
runParser = fst `comp2` PolyPlain.runParser

wrapString2 :: Char -> Char -> String -> String
wrapString2 start end str = start : str ++ cend
	where 
		cend = [end]

wrapString :: Char -> String -> String
wrapString a = wrapString2 a a

wrapQuote :: String -> String
wrapQuote = wrapString '"'

wrapBracket :: String -> String
wrapBracket = wrapString2 '[' ']'

wrapBrace :: String -> String
wrapBrace = wrapString2 '{' '}'

matchQuote :: Parse.TextParser Char
matchQuote = PolyPlain.satisfy (=='"')

dropWhitespace :: Parse.TextParser ()
dropWhitespace = do 
	m <- (PolyPlain.optional $ PolyPlain.satisfy isSpace)
	case m of
		Nothing -> return ()
		Just _ -> dropWhitespace 

dropLeadingWhitespace :: Parse.TextParser a -> Parse.TextParser a
dropLeadingWhitespace = (dropWhitespace >>)

parseQuoted :: Parse.TextParser a -> Parse.TextParser a
parseQuoted parser = do { matchQuote; result <- parser; matchQuote; return result }

