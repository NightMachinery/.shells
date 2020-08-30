/// 2>/dev/null ; gorun "$0" "$@" ; exit $?

package main

import (
	. "fmt"
	"github.com/tj/go-naturaldate"
	"log"
	"os"
	"strings"
	"time"
)

func main() {
	res, err := naturaldate.Parse(strings.Join(os.Args[1:], " "), time.Now(), naturaldate.WithDirection(naturaldate.Future))
	if err != nil {
		log.Fatalln(err.Error())
	}
	Println(res.Format("2006/01/02"))
	//resj, _ := jalaali.From(res).JFormat("2006/01/02")
	//Println(resj) // ۱۳۹۹/۰۶/۱۱
}
