module Su.Utils where

import System.Environment (getEnv)

safeReadFile :: FilePath -> IO String
safeReadFile = mkSafe readFile

safeGetEnv :: String -> IO String
safeGetEnv = mkSafe System.Environment.getEnv

tmpDir :: IO FilePath
tmpDir = do
  tmpDir <- safeGetEnv "TMPDIR"
  if tmpDir == "" 
    then return "/tmp"
    else return tmpDir

mkSafe = mkSafe2 []

mkSafe2 :: defaultValType -> (a -> IO defaultValType) -> a -> IO defaultValType
mkSafe2 = (.) . flip catch . const . return
