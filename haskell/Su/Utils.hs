{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Su.Utils
       ( safeReadFile
       , safeGetEnv
       , tmpDir
       , quoteArgs
       ) where

import Prelude
import Control.Exception as E
import System.Environment (getEnv)
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map as M
import qualified Data.Typeable as T

safeReadFile :: FilePath -> IO String
safeReadFile = mkSafe readFile

safeGetEnv :: String -> IO String
safeGetEnv = mkSafe System.Environment.getEnv

tmpDir :: IO FilePath
tmpDir = do
  tmpDir_ <- safeGetEnv "TMPDIR"
  if tmpDir_ == ""
    then return "/tmp"
    else return tmpDir_

homeDir :: IO FilePath
homeDir = safeGetEnv "HOME"

mkSafe :: (a -> IO [a1]) -> a -> IO [a1]
mkSafe = mkSafe2 []

mkSafe2 :: defaultValType -> (a -> IO defaultValType) -> a -> IO defaultValType
mkSafe2 d f x = E.handle (\SomeException{} -> return d) (f x)

type StateMap a b = S.State (M.Map a b) b

memoizeM :: (Show a, Show b, Ord a) =>
            ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = S.evalState (f x) M.empty where
  g x_ = do
    y <- t f x_
    m <- S.get
    S.put $ M.insert x_ y m
    return $ y
  f x_ = S.get >>= \m -> maybe (g x_) return (M.lookup x_ m)


data AnyShow = forall s. Show s => AS s
deriving instance Show (AnyShow) -- this wraps the AS constructor =/
showIt :: AnyShow -> String
showIt (AS s) = show s

quoteArgs :: (Show a, T.Typeable a) => a -> String
quoteArgs x
  | T.typeOf x == T.typeOf ("" :: String) = show x -- adds the quotes we need
  | otherwise = show . show $ x -- one to convert it to string, and the other to
                                -- properly quote it
