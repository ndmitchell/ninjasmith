{-# LANGUAGE RecordWildCards, TupleSections #-}

module Dependencies(dependencies) where

import Type
import Util
import Control.Monad
import Data.List



-- RULES:
-- * N input/source files
-- * M output/generated files
-- * Each value M depends on a set of M/N (without cycles)
-- * If you depend on a value you must either depend on it, or order-only on it and write out a deps
-- * Randomly use phony as a barrier
-- * Touch some subset of N
-- * Each build depeneds on a set of files


dependencies :: IO [Action]
dependencies = do
    ds <- addDeps (map (,[]) $ digits "i") (digits "o")
    is <- reps (1,3) $ pick $ digits "i"
    us1 <- replicateM (length $ digits "i") unique
    us2 <- replicateM (length is) unique
    let nin = Rule "r" [("command","record --out $out --env RECORD --in $in")] :
              [Build [a] "r" b [] [] [] | (a,b) <- ds, b /= []]
    return $
        [WriteNinja nin] ++
        zipWith WriteFile (digits "i") us1 ++
        [RunNinja []
        ,RunNinja []] ++
        zipWith WriteFile is us2 ++
        [RunNinja []
        ,RunNinja []]


-- given the existing dependencies, and a list of things to add to the graph, produce more dependencies
addDeps :: Eq a => [(a, [a])] -> [a] -> IO [(a, [a])]
addDeps deps [] = return deps
addDeps deps xs = do
    x <- pick xs
    ds <- reps (1,length deps) $ pick deps
    addDeps ((x,map fst ds):deps) (delete x xs)
