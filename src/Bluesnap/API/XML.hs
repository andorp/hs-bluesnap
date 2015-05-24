module Bluesnap.API.XML (
    parse
  , render
  , SchemaType(..)
  , XMLParser
  , Content(..)
  ) where

import qualified Text.PrettyPrint as Pretty
import           Text.XML.HaXml.XmlContent
import           Text.XML.HaXml.Lex
import           Text.XML.HaXml.Parse
import           Text.XML.HaXml.Posn
import           Text.XML.HaXml.Pretty as XMLPretty
import           Text.XML.HaXml.Schema.Schema (SchemaType(..))
import           Text.XML.HaXml.Types
import           Text.XML.HaXml.XmlContent.Parser (XMLParser)

parse p = fst . runParser p . getXmlContents . xmlParse "req-rsp"
  where
    getXmlContents (Document _ _ e _) = [CElem e noPos]

render = Pretty.render . XMLPretty.content
