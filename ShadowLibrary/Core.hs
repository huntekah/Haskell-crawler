{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShadowLibrary.Core where

-- import Database.Persist
-- import Database.Persist.TH
-- import Database.Persist.Postgresql
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)

import Control.Monad.Reader

import Data.Time
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Zones
import Data.Time.Zones.All

import Data.String.Utils (strip)
import Data.List.Utils as DLU

import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Control.Monad.Trans
import Text.XML.HXT.XPath
-- import Text.XML.HXT.Curl
import Text.XML.HXT.HTTP

import Text.Regex.TDFA

import Data.List (isInfixOf, intercalate)

import Data.List.Utils (replace)

-- import Network.Curl.Opts

polishTimeZone = TimeZone {
  timeZoneMinutes = 120,
  timeZoneSummerOnly = True,
  timeZoneName = ""}

openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

getWebPage :: String -> IO (IOSArrow XmlTree (NTree XNode))
getWebPage url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withParseHTML yes, withWarnings no] (fromMaybe "" contents)

downloadDocument = readFromDocument [withParseHTML yes,
                                     withWarnings no,
                                     withEncodingErrors no,
                                     withPreserveComment yes,
                                     withStrictInput yes,
                                     withHTTP []
--                                     withCurl [("curl--user-agent","AMU Digital Libraries Indexing Agent")]
                                    ]

downloadDocumentWithEncoding enc = readFromDocument [withParseHTML yes,
                                                     withWarnings no,
                                                     withEncodingErrors no,
                                                     withPreserveComment yes,
                                                     withInputEncoding enc,
					             withHTTP []]		
--                                                     withCurl []]

downloadXmlDocument = readFromDocument [withWarnings no,
                                        withEncodingErrors no,
					withHTTP []]
--                                        withCurl [] ]


data ShadowLibrary = ShadowLibrary { logoUrl :: Maybe String,
                                     lname :: String,
                                     abbrev :: String,
                                     webpage :: String,
                                     lLevel :: Int }


data ShadowItem = ShadowItem {
  url :: Maybe String,
  title :: String,
  itype :: String,
  originalDate :: Maybe String,
  creator :: Maybe String,
  format :: Maybe String,
  lang :: Maybe String,
  finalUrl :: String,
  description :: Maybe String
  } deriving (Show)

defaultShadowItem url title = ShadowItem {
  url = Just url,
  title = title,
  itype = "periodical",
  originalDate = Nothing,
  creator = Nothing,
  format = Just "pdf",
  lang = Just "pol",
  finalUrl = url,
  description = Nothing }


getDuration :: Maybe String -> (Maybe UTCTime, Maybe UTCTime)
getDuration Nothing = (Nothing, Nothing)
getDuration (Just date) =
  case date =~~ ("^(1[6789]|20)[0-9][0-9]$" :: String) of
    Just year -> (Just (yearStart year), Just (yearEnd year))
    otherwise ->
      case date =~~ ("^((1[789]|20)[0-9][0-9])-(0[1-9]|1[0-2])$" :: String) :: Maybe [[String]] of
        Just [[_, year, _, month]] -> (Just (monthStart year month), Just (monthEnd year month))
        otherwise ->
          case date =~~ ("^((1[789]|20)[0-9][0-9])-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$" :: String) :: Maybe [[String]] of
            Just [[_, year, _, month, day]] -> (Just (dayStart year month day), Just (dayEnd year month day))
            otherwise ->
              case date =~~ ("^((1[789]|20)[0-9][0-9])-((1[789]|20)[0-9][0-9])$" :: String) :: Maybe [[String]] of
                Just [[_, yearb, _, yeare, _]] -> (Just (yearStart yearb), Just (yearEnd yeare))
                otherwise -> (Nothing, Nothing)
  where
    yearAsInteger year = (read year) :: Integer
    monthAsInt month = (read month) :: Int
    dayAsInt day = (read day) :: Int
    yearStart year = localTimeToTimeStamp $ LocalTime {
      localDay = fromGregorian (yearAsInteger year) 1 1,
      localTimeOfDay = midnight }
    yearEnd year = localTimeToTimeStamp $ LocalTime {
      localDay = fromGregorian (yearAsInteger year) 12 31,
      localTimeOfDay = lastSecond }
    monthStart year month = localTimeToTimeStamp $ LocalTime {
      localDay = fromGregorian (yearAsInteger year) (monthAsInt month) 1,
      localTimeOfDay = midnight }
    monthEnd year month = localTimeToTimeStamp $ LocalTime {
      localDay = fromGregorian (yearAsInteger year) (monthAsInt month) 31,
      localTimeOfDay = lastSecond }
    dayStart year month day = localTimeToTimeStamp $ LocalTime {
      localDay = fromGregorian (yearAsInteger year) (monthAsInt month) (dayAsInt day),
      localTimeOfDay = midnight }
    dayEnd year month day = localTimeToTimeStamp $ LocalTime {
      localDay = fromGregorian (yearAsInteger year) (monthAsInt month) (dayAsInt day),
      localTimeOfDay = lastSecond }
    lastSecond = TimeOfDay {todHour = 23, todMin = 59, todSec = 59 }

localTimeToTimeStamp ltime = localTimeToUTCTZ (tzByLabel Europe__Warsaw) ltime

--  zonedTimeToUTC $ ZonedTime {
--  zonedTimeToLocalTime = ltime,
--  zonedTimeZone = timeZoneForUTCTime Europe__Warsaw }

extractLinks xpathCondition = (downloadDocument &&& this)
                              >>> first (getXPathTrees xpathCondition
                                         >>> getAttrValue "href")
                              >>> expandURIFixed

extractLinksGeneralized xpathCondition attr = (downloadDocument &&& this)
                                              >>> first (getXPathTrees xpathCondition
                                                         >>> getAttrValue attr)
                                              >>> expandURIFixed

rotateSecTh ((a, b), c) = ((a, c), b)

extractLinksWithText xpathCondition = (downloadDocument &&& this)
                                      >>> first (getXPathTrees xpathCondition
                                                 >>> (getAttrValue "href"
                                                      &&& (listA (deep isText >>> getText)
                                                           >>> arr (intercalate " "))))
                                      >>> arr rotateSecTh
                                      >>> first expandURIFixed

extractLinksWithTitle xpathCondition = (downloadDocument &&& this)
                                       >>> first (getXPathTrees xpathCondition
                                                  >>> (getAttrValue "href"
                                                       &&& getAttrValue "title"))
                                       >>> arr rotateSecTh
                                       >>> first expandURIFixed

urlPreFixer = arr (Data.List.Utils.replace "[" stupidLeftBracketMarker . Data.List.Utils.replace "]" stupidRightBracketMarker)
urlPostFixer = arr (Data.List.Utils.replace stupidLeftBracketMarker "%5B" . Data.List.Utils.replace stupidRightBracketMarker "%5D")

stupidLeftBracketMarker = "ddsfdfdfdfgfgfrrtrtrrtr"
stupidRightBracketMarker = "wqweweerererrtrtrtrtrtr"

expandURIFixed = (urlPreFixer *** urlPreFixer) >>> expandURI >>> urlPostFixer


extractText = (listA (deep isText >>> getText)
               >>> arr (intercalate " "))

loopNext extract xpathConditionForNext = initialStep
                                         >>> loopNextCore extract xpathConditionForNext
                                         >>> arr fst
                                         >>> unlistA

initialList :: [(String, a)]
initialList = []

initialStep = arr (const initialList) &&& this


loopNextCore extract xpathConditionForNext = second
                                               (listA extract &&& extractNext xpathConditionForNext)
                                             >>> arr expandList
                                             >>> ifP (nextFound)
                                                    (second (arr fromJust >>> (downloadDocument &&& this))
                                                     >>> loopNextCore extract xpathConditionForNext)
                                                    (this)

nextFound :: ([(String,a)], Maybe String) -> Bool
nextFound (_, Just _) = True
nextFound (_, Nothing) = False


expandList (l, (e, n)) = (l ++ e, n)

extractNext xpathConditionForNext = listA
                                          (first (getXPathTrees ("(" ++ xpathConditionForNext ++ ")[1]")
                                                  >>> getAttrValue "href")
                                           >>> expandURIFixed)
                                    >>> arr listToMaybe


extractFormat :: String -> Maybe String
extractFormat finalUrl
  | ".gif" `isInfixOf` finalUrl = Just "gif"
  | ".jpg" `isInfixOf` finalUrl = Just "jpg"
  | ".djvu" `isInfixOf` finalUrl = Just "djvu"
  | ".pdf" `isInfixOf` finalUrl = Just "pdf"
  | ".doc" `isInfixOf` finalUrl = Just "doc"
  | otherwise = Nothing



baseMonthNameToNumber :: String -> Maybe String
baseMonthNameToNumber "styczeń"     = Just "01"
baseMonthNameToNumber "styczen"     = Just "01"
baseMonthNameToNumber "stycznia"     = Just "01"
baseMonthNameToNumber "luty"        = Just "02"
baseMonthNameToNumber "lutego"        = Just "02"
baseMonthNameToNumber "marzec"      = Just "03"
baseMonthNameToNumber "marca"      = Just "03"
baseMonthNameToNumber "kwiecień"    = Just "04"
baseMonthNameToNumber "kwiecien"    = Just "04"
baseMonthNameToNumber "kwietnia"    = Just "04"
baseMonthNameToNumber "maj"         = Just "05"
baseMonthNameToNumber "maja"         = Just "05"
baseMonthNameToNumber "czerwiec"    = Just "06"
baseMonthNameToNumber "czeerwiec"    = Just "06"
baseMonthNameToNumber "czerwca"    = Just "06"
baseMonthNameToNumber "lipiec"      = Just "07"
baseMonthNameToNumber "lipca"      = Just "07"
baseMonthNameToNumber "sierpień"    = Just "08"
baseMonthNameToNumber "sierpien"    = Just "08"
baseMonthNameToNumber "sierpnia"    = Just "08"
baseMonthNameToNumber "wrzesień"    = Just "09"
baseMonthNameToNumber "wrzesien"    = Just "09"
baseMonthNameToNumber "września"    = Just "09"
baseMonthNameToNumber "wrzesnia"    = Just "09"
baseMonthNameToNumber "październik" = Just "10"
baseMonthNameToNumber "pażdziernik" = Just "10"
baseMonthNameToNumber "pazdziernik" = Just "10"
baseMonthNameToNumber "października" = Just "10"
baseMonthNameToNumber "pazdziernika" = Just "10"
baseMonthNameToNumber "listopad"    = Just "11"
baseMonthNameToNumber "listopada"    = Just "11"
baseMonthNameToNumber "grudzień"    = Just "12"
baseMonthNameToNumber "grudzien"    = Just "12"
baseMonthNameToNumber "grudnia"    = Just "12"
baseMonthNameToNumber "jesien"    = Just "10"
baseMonthNameToNumber _             = Nothing

extractYear :: String -> Maybe String
extractYear n =
  case n =~~ ("(1[6789]|20)[0-9][0-9]" :: String) of
    Just year -> Just year
    otherwise -> Nothing


joinAlts :: [String] -> String
joinAlts = intercalate " // "

clean = arr (DLU.replace "\n" "")
        >>> arr strip

extractItems shadowLibrary start extractor = do
  page <- getWebPage start
  items <- runX $ extractor start page
--  insertIntoDatabase shadowLibrary items
  putStrLn (show items)

extractItemsStartingFromUrl shadowLibrary start extractor = do
  items <- runX $ (arr (const start) >>> setTraceLevel 1 >>> extractor)
--  insertIntoDatabase shadowLibrary items
  mapM_ (putStrLn . show) items
