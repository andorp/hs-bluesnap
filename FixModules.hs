
import Data.Char (toUpper)
import Data.List (find)
import System.Environment (getArgs)

capModule qname ('m':'o':'d':'u':'l':'e':' ':c:cs)               = concat ["module ", qname, ".", [toUpper c], cs]
capModule qname (' ':' ':p:' ':'m':'o':'d':'u':'l':'e':' ':c:cs) = concat ["  ", [p], " module ", qname, ".", [toUpper c], cs]
capModule _ s = s

fixImport qname imports@('i':'m':'p':'o':'r':'t':' ':xs) =
  if isQualifiedName xs
    then imports
    else concat ["import ", qname, ".", xs]
  where
    isQualifiedName = maybe False (const True) . find (=='.')
fixImport _ s = s

fixXsd :: String -> String
fixXsd "import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs" = "import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd\nimport qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs"
fixXsd s = s

fixModuleNames :: String -> String -> String
fixModuleNames qname = unlines . map (fixXsd . fixImport qname . capModule qname) . lines


main = do
  qualifiedName:_ <- getArgs
  content <- getContents
  let content' = fixModuleNames qualifiedName content
  putStr content'
