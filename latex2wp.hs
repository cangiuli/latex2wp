-- latex2wp
-- Carlo Angiuli

module Main where

import Text.Pandoc
import System.Process
import System.IO
import System.Environment
import System.FilePath.Posix

-- Main

main = do
  args <- getArgs
  if null args then putStr usage else do
  let filename = head args
  latex <- readFile filename
  pandoc <- bottomUpM wpLatex $ readDoc latex
  writeFile (takeBaseName filename ++ ".html") $ writeDoc pandoc

usage = unlines
  ["Usage: latex2wp filename",
   "",
   "Convert LaTeX input files to HTML source suitable for WordPress.",
   "Uses WP-LaTeX syntax ($latex $) for inline math,",
   "and converts display math to PNG."]

-- Rewriting Pandoc

readDoc = readLaTeX defaultParserState

-- $x$ --> <span class="LaTeX">$x$</span>
writeDoc = writeHtmlString $
  defaultWriterOptions {writerHTMLMathMethod = LaTeXMathML Nothing}

wpLatex e = case e of
  -- start inline math with $latex
  Math InlineMath x -> return $ Math InlineMath ("latex " ++ x)
  Math DisplayMath x -> displayMath x
  _ -> return e

-- Render display math

displayMath x = do
  (stdin,stdout,_,_) <- runInteractiveCommand "./math2png"
  hPutStr stdin x
  hClose stdin
  filename <- hGetContents stdout
  return $ Image [] (filename,x)

