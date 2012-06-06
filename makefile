HSFLAGS = -fwarn-name-shadowing  -XOverloadedStrings
CLG = $(HSFLAGS) -dynamic --make -O2 -threaded -rtsopts

PROGS=Primes.o jpd

all:$(PROGS)

jpd : jpd.hs Primes.o
	ghc $(CLG) -o jpd jpd.hs

% : %.hs
	ghc $(CLG) -o $@ $<

Primes.o:Primes.hs
	ghc $(CLG) -c Primes.hs

clean:
		-rm *.hi *.o $(PROGS)
