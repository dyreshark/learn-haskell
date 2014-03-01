module TestMainGen where

import Control.Monad (filterM)
import Data.String.Utils (join, startswith, replace)
import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents fp = getDirectoryContents fp >>= return . filter (not . startswith ".") 

filterTests :: [FilePath] -> [FilePath]
filterTests = filter isTest
    where
        isTest = ("Test.hs"==) . takeFileName

locateTestsIn :: FilePath -> IO [FilePath]
locateTestsIn path = do
    rawContents <- getVisibleDirectoryContents path
    let contents = map (path </>) rawContents
    files <- filterM doesFileExist contents
    dirs <- filterM doesDirectoryExist contents
    subdirTests <- mapM locateTestsIn dirs
    return . concat $ filterTests files:subdirTests

deriveImportsFrom :: FilePath -> [FilePath] -> [(FilePath, String)]
deriveImportsFrom curpath = map (pathToImportPair . makeRelative curpath)
    where 
        pathToImportPair p = (p, pathToImport p)
        pathToImport = replace [pathSeparator] "." . dropExtension 

generateMainBody :: [String] -> String
generateMainBody modules = join "\n" [mainHeader ++ " {", mainStatements, "}"]
    where
        mainHeader = "main = do"
        mainStatements = join "\n" . map (fmt . callRunTests) $ modules
        fmt = ('\t':)
        callRunTests = (++".runTests")

generateMainContents :: FilePath -> [FilePath] -> String
generateMainContents curdir tests = join "\n" [mainHeader, importStatements, mainBody]
    where 
        mainHeader = "module Main where"
        importStatements = join "\n" . map ("import qualified " ++) $ imports 
        imports = map snd $ deriveImportsFrom curdir tests
        mainBody = generateMainBody imports

hasVerboseFlag :: [String] -> Bool
hasVerboseFlag = not . null . filter isVerboseFlag
    where
        isVerboseFlag s = s == "--verbose" || s == "-v"

discardM :: (Monad m) => a -> m ()
discardM _ = return ()

main = do
    args <- getArgs
    let verbosePrint = if hasVerboseFlag args then putStrLn else discardM
    verbosePrint "Finding tests..."
    allTests <- locateTestsIn "."
    verbosePrint $ "Found tests: " ++ show allTests
    verbosePrint "Generating main testing file..."
    let mainContents = generateMainContents "." allTests
    verbosePrint "Complete. Writing..."
    writeFile "Main.hs" mainContents
    verbosePrint "Success."