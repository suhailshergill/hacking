module Su.Date
       ( fromOrgDateGetLocal
         , parseOrgDate
         , parseOrgDateTz
         , parseDateUTC
       ) where


import Data.Time.Format (readTime, formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Clock (UTCTime, utctDayTime, getCurrentTime)
import Data.Time.LocalTime (TimeZone, getTimeZone)
import Data.List.Split (splitOn)

parseOrgDate :: String -> UTCTime
parseOrgDate = readTime defaultTimeLocale "%F %a %R"

parseOrgDateTz :: String -> UTCTime
parseOrgDateTz = readTime defaultTimeLocale "%F %a %R %Z"

parseDateUTC :: String -> UTCTime
parseDateUTC = readTime defaultTimeLocale "%F %T %Z"

fromOrgDateGetLocal :: String -> IO UTCTime
fromOrgDateGetLocal ts = do
  tz <- getTimeZone $ parseOrgDate ts
  return $! parseOrgDateTz $ ts ++ " " ++ (show tz)


main = do
  fromOrgDateGetLocal "2011-06-16 Thu 01:59"
