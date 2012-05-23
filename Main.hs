module Main where

import Text.Pandoc
import System.Process
import System.IO

main = do
  latex <- getContents
  pandoc <- bottomUpM wpLatex $ readDoc latex
  putStrLn $ writeDoc pandoc

-- Going from/to Pandoc
-- Translates $x$ to <span class="LaTeX">$x$</span>

readDoc = readLaTeX defaultParserState
writeDoc = writeHtmlString $
  defaultWriterOptions {writerHTMLMathMethod = LaTeXMathML Nothing}

-- Rewriting Pandoc

wpLatex e = case e of
  -- start inline math with $latex
  Math InlineMath x -> return $ Math InlineMath ("latex " ++ x)
  Math DisplayMath x -> displayMath x
  _ -> return e

-- render display math with latex2png script
-- TODO alt/title text
displayMath x = do
  (stdin,stdout,_,_) <- runInteractiveCommand "./math2png"
  hPutStr stdin x
  filename <- hGetContents stdout
  return $ Image [] (filename,"")

