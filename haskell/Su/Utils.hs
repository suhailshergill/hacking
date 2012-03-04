module Su.Utils where

import System.Environment (getEnv)

safeReadFile :: FilePath -> IO String
safeReadFile = mkSafe readFile

safeGetEnv :: String -> IO String
safeGetEnv = mkSafe System.Environment.getEnv

mkSafe = mkSafe2 []

mkSafe2 :: defaultValType -> (a -> IO defaultValType) -> a -> IO defaultValType
mkSafe2 = (.) . flip catch . const . return
