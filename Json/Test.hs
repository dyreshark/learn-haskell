{-# LANGUAGE TemplateHaskell #-}
module Json.Test where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Json
import Json.Internal
import Test.QuickCheck
import Test.QuickCheck.All 
import Text.Parse (manyFinally, TextParser, next, eof, parseByRead)

-- Util methods for testing

parseByRead' :: (Read a) => TextParser a
parseByRead' = parseByRead "DUMMY"

drain :: TextParser String
drain = manyFinally next eof

rightEq :: (Eq b) => (Either a b) -> b -> Bool
rightEq (Right s1) s2 = s1 == s2
rightEq _ _ = False

eqOrFailed :: (Eq b) => (Either a b) -> (Either c b) -> Bool
eqOrFailed (Left _) (Left _) = True
eqOrFailed (Right r1) (Right r2) = r1 == r2 
eqOrFailed _ _ = False

-- Testing internals
prop_Comp2ComposesMethods a b = compfn a b == (a + b) * 2
	where
		compfn = (*2) `comp2` (+)

-- wrapString...
prop_WrapString2WrapsString :: String -> Char -> Char -> Bool
prop_WrapString2WrapsString s c1 c2 = wrapString2 c1 c2 s == c1 : s ++ [c2]

prop_WrapStringWrapsString :: String -> Char -> Bool
prop_WrapStringWrapsString s c = wrapString c s == c : s ++ [c]

prop_WrapQuoteWrapsWithQuotes :: String -> Bool
prop_WrapQuoteWrapsWithQuotes s = wrapQuote s == '"' : s ++ ['"']

prop_WrapBracesWrapsWithBraces :: String -> Bool
prop_WrapBracesWrapsWithBraces s = wrapBrace s == "{" ++ s ++ "}"

prop_WrapBracketsWrapsWithBrackets :: String -> Bool
prop_WrapBracketsWrapsWithBrackets s = wrapBracket s == "[" ++ s ++ "]"

-- TODO:
-- prop_MatchQuoteMatchesOnlyQuote c = isRight parseResult && isQuote
-- 	where 
-- 		parseResult = runParser matchQuote [c]
-- 		isQuote = c == '"'
-- 		isRight (Right _) = True
-- 		isRight _ = False


newtype Whitespace = Whitespace String deriving (Show, Read, Eq)

toWs :: String -> Maybe Whitespace
toWs s = if any (not . isSpace) s then Nothing else Just (Whitespace s)

toWs' :: String -> Whitespace 
toWs' s = case toWs s of 
	Nothing -> error $ "Could not convert " ++ show s ++ " to whitespace"
	Just r -> r

fromWs :: Whitespace -> String
fromWs (Whitespace s) = s
	
joinWs :: [Whitespace] -> String
joinWs = foldr (++) "" . map fromWs

instance Arbitrary Whitespace where
	-- Stolen from isSpace. :)
	arbitrary = elements wsElems
		where
			wsElems = map charToWs [' ', '\t', '\n', '\r', '\f', '\v', '\xa0']
			charToWs = toWs' . (:[])
	shrink _ = []

prop_DropWhitespaceConsumesAllWhitespace :: [Whitespace] -> String -> Bool
prop_DropWhitespaceConsumesAllWhitespace ws s = parseResult `rightEq` expResult
	where 
		parseResult = runParser (dropWhitespace >> drain) fullString
		expResult = dropWhile isSpace s
		fullString = joinWs ws ++ s

prop_DropLeadingWhitespaceDropsLeadingWhitespace :: [Whitespace] -> String -> Bool
prop_DropLeadingWhitespaceDropsLeadingWhitespace ws s = parseResult `rightEq` expResult
	where
		parseResult = runParser (dropLeadingWhitespace drain) fullString
		expResult = dropWhile isSpace fullString
		fullString = joinWs ws ++ s


prop_DropLeadingWhitespaceLeavesTrailingWhitespace :: [Whitespace] -> String -> Bool
prop_DropLeadingWhitespaceLeavesTrailingWhitespace ws s = parseResult `rightEq` expResult
	where
		parseResult = runParser (dropLeadingWhitespace drain) fullString
		fullString = joinWs ws ++ s ++ joinWs ws
		expResult = dropWhile isSpace fullString

prop_ParseQuotedConsumesQuotes :: String -> Bool
prop_ParseQuotedConsumesQuotes s = parseResult `rightEq` s
	where
		parseResult = runParser parser str
		-- This will double-quote the string; this is desired behavior.
		str = wrapQuote . show $ s
		parser = parseQuoted parseByRead' :: TextParser String

-- q1: Left quote? q2: Right quote? 
prop_ParseQuotedFailsOnMissingQuotes :: Bool -> Bool -> String -> Bool
prop_ParseQuotedFailsOnMissingQuotes q1 q2 s = expResult `eqOrFailed` expResult
	where 
		expResult = if expSuccess then Right s else Left ""
		expSuccess = and [q1, q2]
		str = (quoteLeft . quoteRight . show) s
		quoteLeft = if q1 then ('"':) else id
		quoteRight = if q2 then (++"\"") else id

-- Testing Json
testReadWrite :: (Jsonable a) => (Eq a) => a -> Bool
testReadWrite j = jsonPass j `rightEq` j
	where
		-- fromJust because we want loud failure if we couldn't
		-- read the JSON back
		jsonPass = readJson . writeJson

prop_StringReadWriteSuccess :: String -> Bool
prop_StringReadWriteSuccess = testReadWrite

prop_FloatReadWriteSuccess :: Float -> Bool
prop_FloatReadWriteSuccess = testReadWrite

prop_BoolReadWriteSuccess :: Bool -> Bool
prop_BoolReadWriteSuccess = testReadWrite

prop_IntegerReadWriteSuccess :: Integer -> Bool
prop_IntegerReadWriteSuccess = testReadWrite

-- Arbitrarily chose integer array
prop_IntegerArrayReadWriteSuccess :: JsonArray Integer -> Bool
prop_IntegerArrayReadWriteSuccess = testReadWrite

prop_StringArrayReadWriteSuccess :: JsonArray String -> Bool
prop_StringArrayReadWriteSuccess = testReadWrite

-- Arbitrarily chose String->Integer pairs
prop_IntegerObjectReadWriteSuccess :: JsonObject Integer -> Bool
prop_IntegerObjectReadWriteSuccess = testReadWrite

prop_StringObjectReadWriteSuccess :: JsonObject String -> Bool
prop_StringObjectReadWriteSuccess = testReadWrite

runTests = $quickCheckAll

