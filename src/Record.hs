
-- | Utility module used in the rules

module Main(main) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Environment
import Data.Hashable
import qualified Data.ByteString.Char8 as BS


data Arg = Out FilePath | Lit String | Env String | In FilePath | DepFile FilePath | Dep String

parseArgs :: [String] -> [Arg]
parseArgs = f (error "Needs an argument first")
    where
        f c ("--out":xs) = f Out xs
        f c ("--lit":xs) = f Lit xs
        f c ("--env":xs) = f Env xs
        f c ("--in":xs) = f In xs
        f c ("--depfile":xs) = f DepFile xs
        f c ("--dep":xs) = f Dep xs
        f c (x:xs) = c x : f c xs
        f c [] = []

main = do
    xs <- parseArgs <$> getArgs
    forM_ xs $ \x -> case x of
        Out x -> do
            res <- fmap catMaybes $ forM xs $ \x -> case x of
                Lit s -> return $ Just s
                Env s -> Just <$> getEnv s
                In s -> do src <- BS.readFile s; return $ Just $ s ++ " = " ++ show (hash src)
                _ -> return Nothing
            writeFile x $ unlines res
        DepFile x ->
            writeFile x $ unlines [x | Dep x <- xs]
        _ -> return ()
