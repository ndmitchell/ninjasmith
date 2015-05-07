{-# LANGUAGE RecordWildCards, TupleSections #-}

module Util(
    digits, digitsF,
    pick, pick1, split, apply,
    ) where

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

apply :: (Int,Int) -> (a -> IO a) -> a -> IO a
apply rng op x = do
    i <- randomRIO rng
    f i x
    where
        f 0 x = return x
        f i x = f (i-1) =<< op x
