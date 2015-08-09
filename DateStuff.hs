module DateStuff (ExpectedDays, generateLastNDays , getTime, makeDateString) where

import Control.Monad(liftM2, liftM)
import System.Locale (defaultTimeLocale)
import Data.Time (formatTime, showGregorian, addDays, localDay, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Control.Applicative ((<$>))

type ExpectedDays = [String]

stringNDaysAgo :: Integer -> IO String
stringNDaysAgo n = do
  time <- liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime
  return $ showGregorian $ addDays (-n) $ localDay time

getTime :: IO String
getTime =
  formatTime defaultTimeLocale "%H:%M:%S"
  <$> liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime

generateLastNDays :: Integer -> IO ExpectedDays
generateLastNDays n = liftM reverse $ mapM stringNDaysAgo [0..(n-1)]

makeDateString :: String -> String -> String
makeDateString t d =
  " --date=\"" ++ d ++ " " ++ t ++ "\""
