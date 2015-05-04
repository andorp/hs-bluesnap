module Bluesnap.API.Test where

import Bluesnap.API.Request  as Req
import Bluesnap.API.Response as Rsp
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Lex
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

parseXMLReqRsp p = runParser p . getXmlContents . xmlParse "req-rsp"
  where
    getXmlContents (Document _ _ e _) = [CElem e noPos]
