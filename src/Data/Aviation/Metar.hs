{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Metar where

import Network.HTTP
import Network.URI

import Prelude
import Text.Parser.Char
import Text.Parser.Combinators(eof)
import Data.Aviation.WX
import Data.Attoparsec.Text(Parser, parse, parseOnly, Result)
import Data.Text(Text)

-- curl http://www.bom.gov.au/aviation/php/process.php -d page=metar-speci -d state=Queensland
r :: Request String
r =
  Request
    (URI "http:" (Just (URIAuth "" "www.bom.gov.au" "")) "/aviation/php/process.php" "" "")
    POST
    [ 
      mkHeader HdrContentLength "33"
    , mkHeader HdrContentType "application/x-www-form-urlencoded"
    ]
    "page=metar-speci&state=Queensland"

s :: IO String
s =
  simpleHTTP r >>= getResponseBody

testout :: Text
testout =
  "15/09/2016 UTC</h3><p class=\"product\">METAR YBTL 150800Z AUTO 06009KT 9999 // NCD 25/21 Q1014<br />RMK RF00.0/000.0</p></div><p>t TTF automatically supersedes the TAF for the 3 hr validity of the TTF, unless otherwise specified in the TTF.</p></div><div id=\"Area-bottom'><h3>BURKETOWN YBKT  15/09/2016 UTC</h3><p class=\"product\">METAR YBKT 150800Z AUTO 01012KT //// // ////// 29/22 Q1011<br />RMK margin-small pad-bottom'><h3>* CAIRNS YBCS  15/09/2016 UTC</h3><p class=\"product\">METAR YBCS 150800Z 04006KT 9999 FEW028 SCT175 26/22 iv><div class='margin-small pad-bottom'><h3>CAPE FLATTERY YCFL  15/09/2016 UTC</h3><p class=\"product\">SPECI YCFL 150800Z AUTO 14012KT K RF00.0/000.0</p></div><div class='margin-small pad-bottom'><h3>COEN YCOE  15/09/2016 UTC</h3><p class=\"product\">METAR YCOE 150800Z A1012<br />RMK RF00.0/000.0</p></div><div class='margin-small pad-bottom'><h3>COOKTOWN YCKN  15/09/2016 UTC</h3>"

testout2 :: Text
testout2 = "METAR YBBN 150800Z 24005KT 9999 NOSIG="-- METAR YBBN 150800Z 24005KT 9999 -RA FEW015 SCT034 BKN060 19/19 Q1015"

pars ::
  Parser Weather
pars =
  do  -- _ <- string "<p class=\"product\">"
      w <- weatherParser
      
      return w

testpars ::
  Text
  -> Either String Weather
testpars =
  parseOnly pars
