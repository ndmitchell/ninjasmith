{-# LANGUAGE RecordWildCards, TupleSections #-}

module Util(
    digits, digitsF,
    pick, pick1, split, reps, reps1,
    ) where

import Control.Monad
import System.Random hiding (split)


digits :: [String]
digits = map show [1..9]

digitsF :: [String -> String]
digitsF = map (flip (++)) digits

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
