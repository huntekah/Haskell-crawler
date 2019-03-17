
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import ShadowLibrary.Core

import Text.XML.HXT.Core
import Text.XML.HXT.XPath
-- import Text.XML.HXT.Curl
import Data.List
import Data.List.Utils (replace)

import Text.Regex.Posix
import Text.Printf

--getFirst :: ([Char], [Char]) -> [Char]
--getFirst ([a],[b]) = fst
eatArg (a,b) = a

extractRecords = extractLinksWithText "//li//ul//div[@class='artifact-title']//a"
--                 >>> second (arr $ replace "\r\n              " "")
--                 >>> first (arr ((++"tr") . init))
                 >>> first (extractLinksWithText "//div[@class='artifact-title']//a")
                -- >>> second (first (arr $ replace "\r\n              " "" .init))
 --                >>> first (first (arr ((++"tr") . init)))
                 >>> first (first (extractLinksWithText "//div[@class='file-link']/a[contains(@href,'.pdf')]"))
  
toShadowItem :: (((String, String), String), String) -> ShadowItem
toShadowItem (((url, articleTitle), yearlyTitle), _) =
  (defaultShadowItem url title) {
    originalDate = Just date,
    itype = "periodical",
    format = Just "pdf",
    finalUrl = url
    }
  where title = "PBStitle" ++ yearlyTitle ++ " " ++ (replace "\r\n" "" (replace "\r\n          " "" articleTitle))
        date = getDate url

getDate url =
  case url =~~ "/(19[0-9][0-9]|20[0-9][0-9])/" :: Maybe [[String]] of
    Just [[_, year]] -> year
    otherwise -> error $ "unexpected url: " ++ url


main = do
    let start = "https://pbsociety.org.pl/repository/community-list"
    let shadowLibrary = ShadowLibrary {logoUrl=Nothing,
                                       lname="Polskie Towarzystwo Botaniczne",
                                       abbrev="pbsociety",
                                       lLevel=0,
                                       webpage=start}
    extractItemsStartingFromUrl shadowLibrary start (extractRecords >>> arr toShadowItem)
