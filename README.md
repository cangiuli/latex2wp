latex2wp
========

A LaTeX -> WordPress utility built on Pandoc. It converts a LaTeX input file to
HTML suitable for WordPress: inline math uses WP-LaTeX syntax (`$latex $`), and
display math is converted to PNG images.

Usage
-----
`latex2wp filename`

You'll also need
----------------

- Haskell (Text.Pandoc, Data.Hash.MD5)
- pdflatex
- ImageMagick
