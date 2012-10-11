package main

import (
    "fmt"
)

func fib(a,b int) (int, int) {
    return b,a+b
}

func main() {
    a,b,sum := 1,1,0
    for {
	a,b = fib(a,b)
	if a % 2 == 0 {
	    sum = sum + a
	}
	if b > 4000000 {
	    break
	}
    }
    fmt.Printf ("%d\n", sum)
}

