HSFLAGS = -fwarn-name-shadowing  -XOverloadedStrings
CLG = $(HSFLAGS) -dynamic --make -O2 -threaded -rtsopts

PROGS=jpd

all:$(PROGS)

% : %.hs
	ghc $(CLG) -o $@ $<

clean:
		-rm *.hi *.o $(PROGS)
