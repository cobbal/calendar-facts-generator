module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."

camelTestData :: Array (Tuple String (Array String))
camelTestData = [ Tuple ""                     []
                , Tuple "lowercase"            ["lowercase"]
                , Tuple "Class"                ["Class"]
                , Tuple "MyClass"              ["My", "Class"]
                , Tuple "MyC"                  ["My", "C"]
                , Tuple "HTML"                 ["HTML"]
                , Tuple "PDFLoader"            ["PDF", "Loader"]
                , Tuple "AString"              ["A", "String"]
                , Tuple "SimpleXMLParser"      ["Simple", "XML", "Parser"]
                , Tuple "vimRPCPlugin"         ["vim", "RPC", "Plugin"]
                , Tuple "GL11Version"          ["GL", "11", "Version"]
                , Tuple "99Bottles"            ["99", "Bottles"]
                , Tuple "May5"                 ["May", "5"]
                , Tuple "BFG9000"              ["BFG", "9000"]
                , Tuple "BöseÜberraschung"     ["Böse", "Überraschung"]
                , Tuple "Two  spaces"          ["Two", "  ", "spaces"]
                ]
