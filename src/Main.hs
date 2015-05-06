{-# LANGUAGE RecordWildCards, TupleSections #-}

module Main(main) where

import Control.Exception.Extra
import Control.Monad
import System.Directory.Extra
import System.Process.Extra
import Data.List
import Data.Hashable
import Data.IORef
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Variables
import Type


test :: [Action] -> IO ()
test actions = do
    a <- run "ninja"
    b <- run "shake"
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
                forM_ actions $ \x -> case x of
                    Prepare act -> act
                    WriteNinja stmts -> writeNinja stmts
                    RunNinja args -> do system_ $ unwords $ exe:args; add
            readIORef ref


main = do
    putStr "Compiling record... "
    createDirectoryIfMissing True "temp-record"
    system_ "ghc --make src/Record.hs -outputdir temp-record -o temp-record/record.exe"
    putStrLn "done"

    examples <- listFiles "examples"
    forM_ (filter ((==) ".ninja" . takeExtension) examples) $ \ex -> do
        putStrLn $ "# Testing example " ++ takeFileName ex
        test
            [Prepare $ forM_ examples $ \x -> copyFile (".." </> x) (takeFileName x)
            ,Prepare $ renameFile (takeFileName ex) "build.ninja"
            ,RunNinja []]

    forM_ [1..2] $ \i -> do
        putStrLn $ "# Testing variables " ++ show i
        test =<< variables
