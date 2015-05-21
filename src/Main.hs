{-# LANGUAGE RecordWildCards, TupleSections #-}

module Main(main) where

import Control.Applicative
import Control.Exception.Extra
import Control.Monad
import System.Directory.Extra
import System.Process.Extra
import Data.List.Extra
import Data.Maybe
import Data.Hashable
import Data.IORef
import System.IO.Extra
import Data.Tuple.Extra
import System.Environment
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Dependencies
import Variables
import Type
import Prelude


test :: [Action] -> IO ()
test actions = do
    a <- run "ninja"
    b <- run "shake"
    forM_ (nubOrd $ map fst $ a ++ b) $ \x -> do
        let aa = lookup x a
        let bb = lookup x b
        when (aa /= bb) $ do
            putStrLn $ "Different " ++ (if isJust aa == isJust bb then "value" else "existance") ++ " for " ++ show x
    when (a /= b) $ error "Mismatch"
    where
        record = do
            files <- sort . filter (not . isPrefixOf ".") <$> listFiles ""
            forM files $ \file -> (file,) . hash <$> BS.readFile file

        run exe = do
            try_ $ removeDirectoryRecursive ("temp-" ++ exe)
            createDirectory $ "temp-" ++ exe
            copyFile "temp-record/record.exe" ("temp-" ++ exe </> "record.exe")
            ref <- newIORef []
            let add = do x <- record; modifyIORef ref (x:)
            withCurrentDirectory ("temp-" ++ exe) $ do
                forM_ (zip [1..] actions) $ \(i,x) -> case x of
                    WriteFile file x -> writeFile file x
                    CopyFile from to -> copyFile from to
                    WriteNinja stmts -> writeNinja stmts
                    RunNinja args -> do
                        setEnv "RECORD" $ "record" ++ show i
                        system_ $ unwords $ exe:args; add
                    Fails msg -> do
                        putStrLn $ "EXPECTED FAILURE: " ++ msg
                        writeIORef ref []
            xs <- readIORef ref
            return $ concat $ zipWith (\i xs -> map (first (i,)) xs) [1..] $ reverse xs


compileRecord :: IO ()
compileRecord = do
    putStr "Compiling record... "
    createDirectoryIfMissing True "temp-record"
    system_ "ghc --make src/Record.hs -outputdir temp-record -o temp-record/record.exe"
    putStrLn "done"


main :: IO ()
main = do
    compileRecord

    examples <- listFiles "examples"
    forM_ (filter ((==) ".ninja" . takeExtension) examples) $ \ex -> do
        putStrLn $ "# Testing example " ++ takeFileName ex
        acts <- map (read . drop 2) . takeWhile (/= "") . lines <$> readFile' ex
        when (null acts) $ error $ "Example " ++ ex ++ " forgot its actions"
        test $
            [CopyFile (".." </> x) (if x == ex then "build.ninja" else takeFileName x) | x <- examples] ++
            acts

    forM_ [1..10] $ \i -> do
        putStrLn $ "# Testing variables " ++ show i
        test =<< variables

    forM_ [1..10] $ \i -> do
        putStrLn $ "# Testing dependencies " ++ show i
        test =<< dependencies
