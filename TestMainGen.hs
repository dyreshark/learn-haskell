module TestMainGen where

import Data.String.Utils
import System.Directory
import System.FilePath.Posix

filterTests :: [FilePath] -> [FilePath]
filterTests = filter isTest . map takeFileName
	where
		isTest = (==) "Test.hs"

locateTestsIn :: FilePath -> IO [FilePath]
locateTestsIn path = do
	contents <- getDirectoryContents path
	files <- filterM doesFileExist contents
	dirs <- filterM doesDirectoryExist contents
	subdirTests <- mapM locateTestsIn dirs
	let myTests = filterTests files
	in return . concat $ myTests:subdirTests

main = do
	putStrLn "Finding tests..."