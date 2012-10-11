package main

import (
    "fmt"
    "math"
)

func p01 () (sum int) {
    sum = 0
    for i:=1; i<1000; i++ {
	if i%3 == 0 || i%5==0 {
	    sum += i
	}
    }
    return
}

func slicer() {
    a:=make([]int,5,10)
    fmt.Println (a)
    a=a[:10]
    fmt.Println (a)
}

func mapper () {
    m := make(map[string]int)
    m["one"]=1
    fmt.Println(m["one"])
    fmt.Println(m["two"])
}

func newt (x float64) float64 {
    z:=x/10.0
    for i:=0; i<10; i++ {
	z0 := z-(z*z-x)/(2*z)
	if tol:= math.Abs(z-z0); tol < .000001 {
	    fmt.Println (tol)
	    break
	} else {
	    fmt.Println (tol)
	}
	z=z0
    }
    fmt.Println (x,z)
    return z
}

func main() {
    fmt.Printf("%d\n", p01())
    newt(36)
}

