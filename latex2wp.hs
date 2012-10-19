-- latex2wp
-- Carlo Angiuli

module Main where

import Text.Pandoc
import qualified Data.Text as Text
import System.Exit
import System.Process
import System.IO
import System.Environment
import System.FilePath.Posix
import System.Console.GetOpt
import Control.Monad.Reader
import Data.Hash.MD5

-- Main {{{

main = do
  (opts,filename) <- parseArgs
  latex <- readFile filename
  let preamble = getPreamble latex
  let doc = readDoc latex
  pandoc <- bottomUpM (wpLatex (opts,preamble)) doc
  writeFile (outputDir opts </> filename <.> "html") $ writeDoc pandoc

getPreamble latex = Text.unpack preamble
  where (preamble,_) = Text.breakOn begin (Text.pack latex)
        begin = Text.pack "\\begin{document}" 

-- CLI

data Options = Options
  { imgSrc :: String
  , outputDir :: String
  , tempDir :: String }

defaultOptions = Options
  { imgSrc = ""
  , outputDir = "."
  , tempDir = "/tmp" }

parseArgs = do
  args <- getArgs
  case getOpt Permute options args of
    (opts,[file],[]) -> return (foldl (flip id) defaultOptions opts,file)
    (_,_,errs) -> do hPutStr stderr (concat errs ++ usageInfo usage options)
                     exitFailure

usage = unlines
  ["Usage: latex2wp [OPTION] ... FILE",
   "Convert LaTeX input files to HTML source suitable for WordPress.",
   "Uses WP-LaTeX syntax ($latex $) for inline math,",
   "and converts display math to PNG."]

options = [
  Option ['i'] ["img-src"]
    (ReqArg (\path opts -> opts { imgSrc = path }) "PATH") $
    "Prepend PATH to image URLs in HTML.\n" ++
    "Does not affect where images will be saved.",
  Option ['o'] ["output-dir"]
    (ReqArg (\dir opts -> opts { outputDir = dir }) "DIR")
    "Output to DIR instead of current directory.",
  Option ['t'] ["temp-dir"]
    (ReqArg (\dir opts -> opts { tempDir = dir }) "DIR")
    "Use DIR for temporary files instead of /tmp."]

-- }}}
-- Rewriting Pandoc {{{

readDoc = readLaTeX defaultParserState

-- $x$ --> <span class="LaTeX">$x$</span>
writeDoc = writeHtmlString $
  defaultWriterOptions {writerHTMLMathMethod = LaTeXMathML Nothing}

wpLatex opts e = case e of
  -- start inline math with $latex
  Math InlineMath x -> return $ Math InlineMath ("latex " ++ x)
  Math DisplayMath x -> displayMath opts x
  _ -> return e

-- }}}
-- Render display math {{{

displayMath (opts,preamble) x = do
  let tmpDir = tempDir opts
  let hash = md5s (Data.Hash.MD5.Str x)
  let name = tmpDir </> hash
  writeFile (name ++ ".tex") (preamble ++ latexSrc x)
  system $ 
    "pdflatex -interaction=batchmode -output-directory " ++ tmpDir ++ " " ++
    name ++ ".tex && " ++
    "convert -density 144 -trim " ++ name ++ ".pdf " ++ name ++ ".png && " ++
    "mv -f " ++ name ++ ".png " ++ (outputDir opts)
  return $ Image [] (imgSrc opts ++ hash ++ ".png",x)

latexSrc x = unlines
  ["\\pagestyle{empty}",
   "\\begin{document} \\[",
   x,
   "\\] \\end{document}"]

-- }}}
