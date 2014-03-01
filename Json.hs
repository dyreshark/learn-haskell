{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances #-}

module Json (
	Jsonable(..),
	JsonArray,
	JsonObject
	) where

import Data.Char (toLower, toUpper)
import Data.List.Utils (join)
import qualified Text.Parse as Parse
import qualified Text.ParserCombinators.Poly.Plain as PolyPlain
import qualified Text.ParserCombinators.Poly.Base as PolyBase
import Json.Internal

type JsonArray a = [a]
type JsonObject a = [(String, a)]

class Jsonable a where
	readJson :: Jsonable a => String -> Either String a
	readJson = runParser parser

	writeJson :: Jsonable a => a -> String

	parser :: Parse.TextParser a

instance Jsonable String where 
	writeJson = show
	parser = Parse.parseByRead "String"

instance Jsonable Integer where
	writeJson = wrapQuote . show
	parser = parseQuoted $ Parse.parseSigned Parse.parseDec

instance Jsonable Float where
	writeJson = wrapQuote . show
	parser = parseQuoted $ Parse.parseSigned Parse.parseFloat

-- Extra complication in here because I want to use the builtin methods to read/write
-- booleans, but in JSON booleans are all lowercase; in Haskell, they're uppercase for
-- the first letter.
instance Jsonable Bool where
	readJson s = runParser Parse.word s >>= makeBool
		where makeBool s = case s of 
			"true" -> Right True
			"false" -> Right False
			m -> Left $ m ++ " is not one of [true, false]"

	-- Show gives capital T/F, and lowercase is "correct"
	writeJson = firstToLower . show
		where 
			firstToLower (x:xs) = toLower x:xs

	-- A bit complicated because JSON bools are all lowercase
	parser = Parse.word >>= readJsonBool
		where
			readJsonBool s = case s of 
								"true" -> return True
								"false" -> return False
								e -> fail $ "Expected true or false, got " ++ e

-- [ele1, ele2, ele3, ...]
instance (Jsonable a) => Jsonable (JsonArray a) where
	writeJson = wrapBracket . join "," . map writeJson
	parser = PolyBase.bracketSep lb sep rb parser
		where 
			lb = satisfyAndDropWhitespace (=='[')
			sep = satisfyAndDropWhitespace (==',')
			rb = PolyPlain.satisfy (==']')

-- {"name": val1, "name": val2, ...}
instance (Jsonable a) => Jsonable (JsonObject a) where
	writeJson = wrapBrace . join "," . map (\(a, b) -> writeJson a ++ ":" ++ writeJson b)
	parser = PolyBase.bracketSep lb sep rb recordParser
		where
			recordParser = do 
							 name <- parser
							 satisfyAndDropWhitespace (==':')
							 val <- parser; return (name, val) 
			lb = satisfyAndDropWhitespace (=='{')
			sep = satisfyAndDropWhitespace (==',')
			rb = PolyPlain.satisfy (=='}')
