{-# LANGUAGE TemplateHaskell #-}

module Json.Test where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.String.Utils (join)
import Json
import Json.Internal
import Test.QuickCheck
import Test.QuickCheck.All 
import Text.Parse (manyFinally, TextParser, next, eof, parseByRead, word)

-- Util methods for testing

-- Convenience function so I can use parseByRead without having to
-- specify a name
parseByRead' :: (Read a) => TextParser a
parseByRead' = parseByRead "DUMMY"

-- Returns the remainder of the data to be parsed
drain :: TextParser String
drain = manyFinally next eof

rightEq :: (Eq b) => (Either a b) -> b -> Bool
rightEq (Right s1) s2 = s1 == s2
rightEq _ _ = False

-- Equality test of Either, discarding the error
-- message in Left instances
eqOrFailed :: (Eq b) => (Either a b) -> (Either c b) -> Bool
eqOrFailed (Left _) (Left _) = True
eqOrFailed (Right r1) (Right r2) = r1 == r2 
eqOrFailed _ _ = False

-- TODO: Change the names from butchered camelCase maybe?
-- QuickCheck.All only autoruns things starting with 
-- prop_, but most of my code is camelCase already

-- Testing Internals
prop_Comp2ComposesMethods a b = compfn a b == (a + b) * 2
    where
        compfn = (*2) `comp2` (+)

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

-- TODO: Custom generator instead of this.
-- This is made as a way to test my dropWhitespace suite of functions
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

prop_DropTrailingWhitespaceDropsTrailingWhitespace :: [Whitespace] -> Bool
prop_DropTrailingWhitespaceDropsTrailingWhitespace ws = parseResult `rightEq` c
    where
        parseResult = runParser parser parseStr
        parser = dropTrailingWhitespace next >> next
        parseStr = [c] ++ wsStr ++ [c]
        wsStr = joinWs ws
        c = 'X'

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
        -- fromJust with no check because loud failure is best failure
        jsonPass = readJson . writeJson

prop_StringReadWriteSuccess :: String -> Bool
prop_StringReadWriteSuccess = testReadWrite

prop_FloatReadWriteSuccess :: Float -> Bool
prop_FloatReadWriteSuccess = testReadWrite

prop_BoolReadWriteSuccess :: Bool -> Bool
prop_BoolReadWriteSuccess = testReadWrite

prop_IntegerReadWriteSuccess :: Integer -> Bool
prop_IntegerReadWriteSuccess = testReadWrite

-- Arbitrarily chose Integer/String arrays
prop_IntegerArrayReadWriteSuccess :: JsonArray Integer -> Bool
prop_IntegerArrayReadWriteSuccess = testReadWrite

prop_StringArrayReadWriteSuccess :: JsonArray String -> Bool
prop_StringArrayReadWriteSuccess = testReadWrite

-- Arbitrarily chose String->Integer & String->String pairs
prop_IntegerObjectReadWriteSuccess :: JsonObject Integer -> Bool
prop_IntegerObjectReadWriteSuccess = testReadWrite

prop_StringObjectReadWriteSuccess :: JsonObject String -> Bool
prop_StringObjectReadWriteSuccess = testReadWrite

-- Testing Json object/array with arbitrary spacing between elements
prop_ReadArrayShouldAcceptArbitrarySpacing :: [Whitespace] -> [Integer] -> Bool
prop_ReadArrayShouldAcceptArbitrarySpacing ws elems = readJson readStr `rightEq` elems
    where
        readStr = wrapBracket . join sep $ elemStrs
        sep = ',' : wsSep
        wsSep = joinWs ws
        elemStrs = map writeJson elems 

-- Because I need to learn how to QuickCheck better, this will be a String->String mapping.
prop_ReadObjectShouldAcceptArbitrarySpacing :: [Whitespace] -> [String] -> Bool
prop_ReadObjectShouldAcceptArbitrarySpacing ws elems = readJson readStr `rightEq` elemTuples
    where
        readStr = wrapBrace . join sep $ elemStrs
        sep = ',' : wsSep
        wsSep = joinWs ws
        elemStrs = map (\(a, b) -> show a ++ ":" ++ wsSep ++ show b) elemTuples
        elemTuples = zip elems elems

runTests = $quickCheckAll
