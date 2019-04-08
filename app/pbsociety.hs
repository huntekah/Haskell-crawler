
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
--eatArg (a,b) = a

extractRecords = extractLinksWithText "(//ul//li//div[@class='artifact-title']//a)"
                 >>> second (arr $ replace "\r\n              " "")
                -- >>> first (extractLinksWithText "//div[@class='artifact-title']//a")
                -- >>> snd (first (arr $ replace "\r\n              " "" .init))
 --                >>> first (first (arr ((++"tr") . init)))
                 >>> first (extractLinksWithText "//div[@class='file-link']/a[contains(@href,'.pdf')]")
  
toShadowItem :: ((String, String), String) -> ShadowItem
toShadowItem ((url, _), sectionTitle) =
  (defaultShadowItem url title) {
    originalDate = Just date,
    itype = "periodical",
    format = Just "pdf",
    finalUrl = url
    }
  where title = "PBS: " ++ (replace "\n" ""  sectionTitle)
        date = getDate url

getDate url =
  case url =~~ "/(19[0-9][0-9]|20[0-9][0-9])." :: Maybe [[String]] of
    Just [[_, year]] -> year
    Just [[[_,_],year],_] -> year
    otherwise -> error $ "unexpected url: " ++ url


main = do
    let start = "https://pbsociety.org.pl/repository/discover?rpp=1000" 
    --let start = "https://pbsociety.org.pl/repository/community-list"
    let shadowLibrary = ShadowLibrary {logoUrl=Nothing,
                                       lname="Polskie Towarzystwo Botaniczne",
                                       abbrev="pbsociety",
                                       lLevel=0,
                                       webpage=start}
    extractItemsStartingFromUrl shadowLibrary start (extractRecords >>> arr toShadowItem)
