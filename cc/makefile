CPPFLAGS= -Wall -g -O
LDFLAGS=-lm

LIBS=primes.o

PROGS=ccinc $(LIBS) t jpd

all:$(PROGS)

jpd:jpd.cc $(LIBS)
	g++ $(CPPFLAGS) $(LDFLAGS) jpd.cc -ojpd

t:t.cc $(LIBS)
	g++ $(CPPFLAGS) $(LDFLAGS) t.cc $(LIBS) -ot

primes.o:primes.cc
	ccinc <primes.cc >primes.h
	g++ $(CPPFLAGS) -c primes.cc
