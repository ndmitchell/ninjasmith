{-# LANGUAGE RecordWildCards, TupleSections #-}

module Util(
    digits, digitsF,
    randomElem, randomSplit, oneOf, perturb,
    ) where

import System.Random


digits :: [String]
digits = map show [1..9]

digitsF :: [String -> String]
digitsF = map (flip (++)) digits

randomElem :: [a] -> IO a
randomElem xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

randomSplit :: [a] -> IO ([a], [a])
randomSplit xs = do
    i <- randomRIO (0, length xs)
    return $ splitAt i xs

oneOf :: [a -> IO a] -> (a -> IO a)
oneOf fs x = ($ x) =<< randomElem fs

perturb :: Int -> (a -> IO a) -> a -> IO a
perturb mx op x = do
    i <- randomRIO (0,mx)
    f i x
    where
        f 0 x = return x
        f i x = f (i-1) =<< op x
