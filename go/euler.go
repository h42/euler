package euler

import (
    "fmt"
    "math"
)

func Newt (x float64) float64 {
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


