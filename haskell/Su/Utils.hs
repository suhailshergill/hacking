module Su.Utils where

import System.Environment (getEnv)
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map as M

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


type StateMap a b = S.State (M.Map a b) b

memoizeM :: (Show a, Show b, Ord a) => 
            ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = S.evalState (f x) M.empty where
  g x = do
    y <- t f x  
    m <- S.get
    S.put $ M.insert x y m
    return $ y
  f x = S.get >>= \m -> maybe (g x) return (M.lookup x m)
