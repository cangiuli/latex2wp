.PHONY: test clean cleanall

latex2wp: latex2wp.hs
	ghc --make $<

test: latex2wp tests/test.tex
	./latex2wp tests/test.tex

clean:
	-rm -f *.hi *.o *.png *.html

cleanall: clean
	-rm latex2wp
