{-# LANGUAGE RecordWildCards, TupleSections #-}

module Variables(variables) where

import Type
import Util
import Control.Monad
import Data.Unique


variables :: IO [Action]
variables = do
    p <- reps1 (0,50) (pick1 [addTopVar, addBuildVar]) zero
    p <- reps1 (0,6 ) (pick1 [addGroup Include, addGroup Subninja]) p
    let nin = Rule "r" [("command","record --out $out --lit " ++ unwords (digits "$v"))] : p
    return [WriteNinja nin, RunNinja ["-j5"]]

addGroup op xs = do
    u <- newUnique
    (pre, midpost) <- split xs
    (mid, post) <- split midpost
    return $ pre ++ [op (show (hashUnique u) ++ ".ninja") mid] ++ post

addTopVar xs = do
    (pre, post) <- split xs
    to <- pick $ digits "v"
    from <- pick $ digits "" ++ digits "$v"
    return $ pre ++ [Variable to from] ++ post

addBuildVar xs = forM xs $ \x -> case x of
    b@Build{} -> do
        to <- pick $ digits "v"
        from <- pick $ digits "" ++ digits "$v"
        return b{buildBind = (to, from) : buildBind b}
    x -> return x

zero =
    [Build [i] "r" [] [] [] [] | i <- digits "f"]
