package main

import (
    "fmt"
)

func main() {
    var d,n int64
    n = 600851475143
    d = 2
    for {
	if n%d == 0 {
	    n = n / d
	    if n == 1 {
		break
	    }
	} else {
	    if d == 2 {
		d += 1
	    } else {
		d+=2
	    }
	}
    }
    fmt.Println(d)
}

