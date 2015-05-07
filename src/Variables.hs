{-# LANGUAGE RecordWildCards, TupleSections #-}

module Variables(variables) where

import Type
import Util
import Control.Monad
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
