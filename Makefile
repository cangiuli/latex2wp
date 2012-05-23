.PHONY: test clean cleanall

latex2wp: latex2wp.hs
	ghc --make $<

test: example.tex latex2wp
	./latex2wp $<

clean:
	-rm -f *.hi *.o *.png *.html

cleanall: clean
	-rm latex2wp
