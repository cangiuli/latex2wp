latex2wp
========

A LaTeX -> WordPress utility built on Pandoc. It converts a LaTeX input file to
HTML suitable for WordPress: inline math uses WP-LaTeX syntax (`$latex $`), and
display math is converted to PNG images.

Usage
-----
Usage: latex2wp [OPTION] ... FILE
Convert LaTeX input files to HTML source suitable for WordPress.
Uses WP-LaTeX syntax ($latex $) for inline math,
and converts display math to PNG.

  -o DIR  --output-dir=DIR  Output to DIR instead of current directory.
  -t DIR  --temp-dir=DIR    Use DIR for temporary files instead of /tmp.

You'll also need
----------------

- Haskell (Text.Pandoc, Data.Hash.MD5)
- pdflatex
- ImageMagick
