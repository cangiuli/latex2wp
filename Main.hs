-- latex2wp
-- Carlo Angiuli

module Main where

import Text.Pandoc
import System.Process
import System.IO
import System.Environment

main = do
  args <- getArgs
  if null args then putStr usage else do
  latex <- readFile $ head args
  pandoc <- bottomUpM wpLatex $ readDoc latex
  putStrLn $ writeDoc pandoc

usage = unlines
  ["Usage: latex2wp filename",
   "",
   "Convert LaTeX input files to HTML source suitable for WordPress.",
   "Uses WP-LaTeX syntax ($latex $) for inline math,",
   "and converts display math to PNG."]

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

-- render display math with math2png script
displayMath x = do
  (stdin,stdout,_,_) <- runInteractiveCommand "./math2png"
  hPutStr stdin x
  filename <- hGetContents stdout
  return $ Image [] (filename,x)

