{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Time.Format (readTime, formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Clock (UTCTime, utctDayTime, getCurrentTime)
import Data.List.Split (splitOn)

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "timezone.h isDst"
  c_isDst :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt

isDST :: UTCTime -> Bool
isDST date =
  let [month,date,year,day,hr,min,sec] = map fromIntegral args
      in
  (0 /=) $ c_isDst year month date hr min sec
  where
    args = map read (splitOn ":" $ formatTime
                     defaultTimeLocale
                     "%m:%d:%Y:%w:%T" date) :: [Int]


main = do
  date <- getCurrentTime
  print $ isDST date
