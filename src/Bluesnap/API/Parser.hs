module Bluesnap.API.Parser where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Lex
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

parse p = fst . runParser p . getXmlContents . xmlParse "req-rsp"
  where
    getXmlContents (Document _ _ e _) = [CElem e noPos]

