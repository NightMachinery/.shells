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
	dbg := os.Getenv("DEBUGME") != ""
	l := log.New(os.Stderr, "", 0)
	res, err := naturaldate.Parse(strings.Join(os.Args[1:], " "), time.Now(), naturaldate.WithDirection(naturaldate.Future))
	if err != nil {
		l.Fatalln(err.Error())
	}
	resMidnight := time.Date(res.Year(), res.Month(), res.Day(), 0, 0, 0, 0, time.Local)
	if ! resMidnight.After(time.Now()) {
		l.Fatalln("The requested date is in the past.")
	}
	if dbg {
		l.Println(res)
	}
	Println(res.Format("2006/01/02"))
	//resj, _ := jalaali.From(res).JFormat("2006/01/02")
	//Println(resj) // ۱۳۹۹/۰۶/۱۱
}
