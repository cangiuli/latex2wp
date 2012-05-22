module Main where

import Text.Pandoc
import System.Process

wpLatex (Math InlineMath y) = Math InlineMath ("latex " ++ y)
--TODO
wpLatex (Math DisplayMath y) = Str $ "$latex " ++ y ++ "$"
wpLatex x = x

readDoc :: String -> Pandoc
readDoc = readLaTeX defaultParserState

writeDoc :: Pandoc -> String
writeDoc = writeHtmlString $
  defaultWriterOptions {writerHTMLMathMethod = LaTeXMathML Nothing}

main = interact (writeDoc . bottomUp wpLatex . readDoc)

