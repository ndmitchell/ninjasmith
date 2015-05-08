{-# LANGUAGE RecordWildCards #-}

module Type(Stmt(..), Action(..), writeNinja) where

import Control.Monad.Extra

data Action
    = WriteFile FilePath String
    | CopyFile FilePath FilePath
    | WriteNinja [Stmt]
    | RunNinja [String]
      deriving (Show,Read)

data Stmt
    = Rule {ruleName :: String, ruleBind :: [(String, String)]}
    | Variable {varName :: String, varExpr :: String}
    | Build {buildOut :: [String], buildRule :: String, buildDep :: [String], buildImp :: [String], buildOrd :: [String], buildBind :: [(String, String)]}
    | Default {defName :: String}
    | Include {incExpr :: String, incBody :: [Stmt]}
    | Subninja {incExpr :: String, incBody :: [Stmt]}
      deriving (Show,Read)

writeNinja :: [Stmt] -> IO ()
writeNinja = output "build.ninja"
    where
        output file xs = writeFile file . unlines =<< concatMapM showStmt xs

        showStmt Rule{..} = return $ ("rule " ++ ruleName) : showBind ruleBind
        showStmt Variable{..} = return [varName ++ " = " ++ varExpr]
        showStmt Build{..} = return $ ("build " ++ unwords buildOut ++ ": " ++ buildRule ++ " " ++ unwords buildDep) : showBind buildBind
        showStmt Include{..} = do
            output incExpr incBody
            return ["include " ++ incExpr]
        showStmt Subninja{..} = do
            output incExpr incBody
            return ["subninja " ++ incExpr]

        showBind xs = ["  " ++ a ++ " = " ++ b | (a,b) <- xs]
