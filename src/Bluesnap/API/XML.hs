module Bluesnap.API.XML (
    parse
  , render
  , SchemaType(..)
  , XMLParser
  , Content(..)
  ) where

import qualified Text.PrettyPrint.HughesPJ as Pretty
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
    getXmlContents (Document _ _ e _) = [CElem (fixNamespace e) noPos]

fixNamespace :: Element i -> Element i
fixNamespace (Elem qname attributes contents) = Elem qname (fix attributes) contents
  where
    fix attrs = (N "xmlns",AttValue [Left "http://ws.plimus.com"]):attrs

render = Pretty.renderStyle (Pretty.Style Pretty.OneLineMode 0 0) . XMLPretty.content . fixContent

fixContent :: Content i -> Content i
fixContent (CElem element i) = (CElem (fixNamespace element) i)
