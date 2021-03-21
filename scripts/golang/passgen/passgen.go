/// 2>/dev/null ; GO111MODULE=off exec gorun "$0" "$@"

package main

import (
	"log"
	"fmt"
	"github.com/sethvargo/go-password/password"
	"os"
	"strconv"
)

func main() {
	leng := 32
	if len(os.Args) >= 2 {
		dummy, err := strconv.Atoi(os.Args[1])
		if err != nil {
			panic(err)
		}
		leng = dummy
	}
	numSymbols := leng/3
	if len(os.Args) >= 3 {
		dummy, err := strconv.Atoi(os.Args[2])
		if err != nil {
			panic(err)
		}
		numSymbols = dummy
	}

	// func Generate(length, numDigits, numSymbols int, noUpper, allowRepeat bool) (string, error)
	res, err := password.Generate(leng, leng/5, numSymbols, false, true)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Print(res)
}
