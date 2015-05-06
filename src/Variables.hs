{-# LANGUAGE RecordWildCards, TupleSections #-}

module Variables(variables) where

import Type
import Control.Monad
import System.Random
import Data.Unique


variables :: IO [Action]
variables = do
    p <- perturb 50 (oneOf [addTopVar, addBuildVar]) zero
    p <- perturb 6 (oneOf [addGroup Include, addGroup Subninja]) p
    let nin = Rule "r" [("command","record --out $out --lit " ++ unwords (map ($ "$v") digitsF))] : p
    return [WriteNinja nin, RunNinja ["-j5"]]

addGroup op xs = do
    u <- newUnique
    (pre, midpost) <- randomSplit xs
    (mid, post) <- randomSplit midpost
    return $ pre ++ [op (show (hashUnique u) ++ ".ninja") mid] ++ post

addTopVar xs = do
    (pre, post) <- randomSplit xs
    to <- randomElem $ map ("v"++) digits
    from <- randomElem $ digits ++ map ("$v"++) digits
    return $ pre ++ [Variable to from] ++ post

addBuildVar xs = forM xs $ \x -> case x of
    b@Build{} -> do
        to <- randomElem $ map ("v"++) digits
        from <- randomElem $ digits ++ map ("$v"++) digits
        return b{buildBind = (to, from) : buildBind b}
    x -> return x

zero =
    [Build [i "f"] "r" [] [] [] [] | i <- digitsF]

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

{-
options:

descend into an Include
descend into a Subninja
write out a rule
write out a build
write out a variable
-}
