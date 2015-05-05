{-# LANGUAGE RecordWildCards, TupleSections #-}

import Control.Exception.Extra
import Control.Monad
import System.Directory.Extra
import System.IO.Extra
import System.Process.Extra
import Data.List
import Data.Hashable
import Data.IORef
import System.FilePath
import qualified Data.ByteString.Char8 as BS


test :: (([String] -> IO ()) -> IO ()) -> IO ()
test act = do
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
                act $ \args -> do
                    system_ $ unwords $ exe:args
                    add
                add
            readIORef ref


main = do
    createDirectoryIfMissing True "temp-record"
    system_ "ghc --make Record.hs -outputdir temp-record -o temp-record/record.exe"

    examples <- listFiles "examples"
    forM_ (filter ((==) ".ninja" . takeExtension) examples) $ \ex -> test $ \go -> do
        forM_ examples $ \x -> copyFile (".." </> x) (takeFileName x)
        renameFile (takeFileName ex) "build.ninja"
        go []
