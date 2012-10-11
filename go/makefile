all:jpd

jpd:jpd.go primes.go
	go build -o $@ jpd.go primes.go

%:%.go
	go build -o $@ $<
