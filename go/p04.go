package main

import (
    "fmt"
)

func reverse (a []int, l int) {
    var t int
    for i:=0; i<l/2; i++ {
	t = a[i]
	a[i] = a[l-1-i]
	a[l-1-i] = t
    }
    return
}

func num2arr(a []int,x int) int {
    var l int
    for l=0; x>0; l++ {
	d:=x%10
	x=x/10
	a[l]=d
    }
    reverse (a,l)
    return l
}

func mini(x,y int) int {
    if x<y {
	return x
    }
    return y
}

func comparr(x,y []int) int {
    lx:=len(x)
    ly:=len(y)
    lm:=mini(lx,ly)
    for i:=0;i<lm;i++ {
	if x[i]<y[i] {
	    return -1
	} else if x[i]>y[i] {
	    return 1
	}
    }
    if lx<ly {
	return -1
    } else if lx>ly {
	return 1
    }
    return 0
}

func main() {
    ans:=0
    a := make([]int, 10)
    lim:=1000
    for i:=100;i<lim;i++ {
	for j:=100;j<lim;j++ {
	    x:=i*j
	    l:=num2arr(a,x)
	    //fmt.Println(a[:l])
	    good:=true
	    for k:=0; k<l/2; k++ {
		if a[k] != a[l-1-k] {
		    good=false
		    break
		}
	    }
	    if good && x>ans {
		ans=x
	    }
	}
    }
    fmt.Println(ans)
}

