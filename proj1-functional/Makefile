all: flp-fun

flp-fun: main.hs
	ghc -Wall --make $^ -o $@

clean:
	rm -f *.o *.hi flp-fun

.PHONY: all clean
