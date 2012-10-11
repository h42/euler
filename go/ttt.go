package main

import (
    "fmt"
    rand "math/rand"
    "time"
    "strings"
    "os"
    "bufio"
)


func main () {
    slicer()
}

func slicer() {
    a:=make([]int,0,5)
    for i:=0;i<21;i++ {
	a=append(a,i+1)
	fmt.Println(a,len(a),cap(a))
    }
    fmt.Println(a)
}

func  stringy () {
    fd,err := os.Open("/etc/passwd")
    if err != nil {
	return
    }
    defer fd.Close()
    rdr:=bufio.NewReader(fd)
    for {
	s,err:=rdr.ReadString('\n')
	if err!=nil {
	    break
	}
	s = strings.TrimRight(s,"\r\n")
	if strings.HasPrefix(s,"jerry") {
	    sx:=strings.Split(s,":")
	    fmt.Println(sx)
	    for _,val:=range sx {
		fmt.Println(val)
	    }
	}
    }
}

var ga = 12

func randy() {
    fmt.Printf("hey\n")
    fmt.Println(ga)
    for i:=0;i<10;i++ {
	ga:=rand.Intn(10)
	fmt.Println(ga)
    }
    fmt.Println(ga)
    var t time.Time = time.Now()
    fmt.Println(t.Format(time.Kitchen))
    fmt.Println(t.Format("15:04:05"))
}

