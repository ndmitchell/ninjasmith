{-# LANGUAGE RecordWildCards, TupleSections #-}

module Util(
    digits, unique,
    pick, pick1, split, reps, reps1,
    ) where

import Control.Monad
import Data.IORef
import Data.Tuple.Extra
import System.IO.Unsafe
import System.Random hiding (split)


digits :: String -> [String]
digits pre = [pre ++ show i | i <- [1..9]]


{-# NOINLINE uniqueRef #-}
uniqueRef :: IORef Integer
uniqueRef = unsafePerformIO $ newIORef 0

unique :: IO String
unique = do
    i <- atomicModifyIORef uniqueRef $ both succ . dupe
    return $ "unique" ++ show i


pick :: [a] -> IO a
pick xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

pick1 :: [a -> IO b] -> (a -> IO b)
pick1 fs x = ($ x) =<< pick fs

split :: [a] -> IO ([a], [a])
split xs = do
    i <- randomRIO (0, length xs)
    return $ splitAt i xs

reps :: (Int, Int) -> IO a -> IO [a]
reps rng x = do
    i <- randomRIO rng
    replicateM i x

reps1 :: (Int,Int) -> (a -> IO a) -> a -> IO a
reps1 rng op x = foldM (flip ($)) x =<< reps rng (return op)
