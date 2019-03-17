
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import ShadowLibrary.Core

import Text.XML.HXT.Core
import Text.XML.HXT.XPath
-- import Text.XML.HXT.Curl
import Data.List
import Data.List.Utils (replace)

import Text.Regex.Posix
import Text.Printf

extractRecords = extractLinksWithText "//a[@class='red'][contains(@href,'.pdf')]"
--                 >>> second (arr $ replace "\r\n              " "")
--                 >>> first (arr ((++"tr") . init))

toShadowItem :: (String, String) -> ShadowItem
toShadowItem (url, articleTitle) =
  (defaultShadowItem url title) {
    originalDate = Just date,
    itype = "periodical",
    format = Just "pdf",
    finalUrl = url
    }
  where title = "Edukacja Ustawiczna " ++ " " ++ (replace "\r\n" "" (replace "\r\n          " "" articleTitle))
        date = getDate url

getDate url =
  case url =~~ "/(19[0-9][0-9]|20[0-9][0-9])/" :: Maybe [[String]] of
    Just [[_, year]] -> year
    otherwise -> error $ "unexpected url: " ++ url


main = do
    let start = "http://www.edukacjaustawicznadoroslych.eu/archiwum.html"
    let shadowLibrary = ShadowLibrary {logoUrl=Nothing,
                                       lname="Edukacja Ustawiczna Doroslych",
                                       abbrev="EUD",
                                       lLevel=0,
                                       webpage=start}
    extractItemsStartingFromUrl shadowLibrary start (extractRecords >>> arr toShadowItem)
