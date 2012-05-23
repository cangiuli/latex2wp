.PHONY: test

test: tests/test.tex
	runhaskell latex2wp.hs tests/test.tex

clean:
	-rm -f *.png *.html
