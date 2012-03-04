{-# LANGUAGE FlexibleInstances #-}

import Data.List (intersperse)

lambdabot :: (Lambdabot r) => String -> r
lambdabot cmd = lambdabotImpl cmd []

class Lambdabot r where
  lambdabotImpl :: String -> [String] -> r
  
instance Lambdabot (IO String) where
  lambdabotImpl cmd args = return $ 
                           (
                             ":!lambdabot -e \"" 
                             ++ cmd ++ " "
                             ++ (concat $ intersperse " " args)
                             ++ "\""
                           )
                           
instance (Show a, Lambdabot r) => Lambdabot (a -> r) where
  lambdabotImpl cmd args = \a -> lambdabotImpl cmd ((show a) : args)
  
main = do
  lambdabot "docs" "Data.Monoid" >>= putStrLn
