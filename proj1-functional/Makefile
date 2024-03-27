all: flp-fun

flp-fun: main.hs
	ghc -Wall --make $^ -o $@

clean:
	rm -f *.o *.hi flp-fun

zip:
	zip xzmitk01.zip main.hs Makefile

.PHONY: all clean zip
