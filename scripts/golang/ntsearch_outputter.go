/// 2>/dev/null ; GO111MODULE=off exec gorun "$0" "$@"

package main

import (
	//"bytes"
	. "fmt"
	"github.com/gookit/color"
	"io/ioutil"
	"log"
	"strings"
	"os"
)

func main() {
	relDir := ""
	if len(os.Args) >= 2 {
		relDir = os.Args[1] + "/"
	}

	l := log.New(os.Stderr, "", 0)
	topS := color.NewRGBStyle(color.RGB(0, 0, 0), color.RGB(200, 255, 200))
	// matchS := color.NewRGBStyle(color.RGB(255, 120, 0), color.RGB(255, 255, 255))

	inBytes, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		log.Fatalln(err.Error())
	}
	input := string(inBytes)
	isep := "\x00"
	osep := "\x00"
	records := strings.Split(input, isep)

	for i, filePath := range records {
		if i != 0 {
			Print(osep)
		}
		topS.Println(filePath)
		Println()

		if fs, err := os.Stat(filePath); err != nil || fs.IsDir() {
			if fs, err := os.Stat(relDir + filePath); err == nil && fs.IsDir() == false {
				filePath = relDir + filePath
			} else {
				// Println(match)
				l.Fatalln("File supplied did not exist or is a directory.")
			}
		}

		fileBytes, err := ioutil.ReadFile(filePath)
		if err != nil {
			l.Fatal(err)
		}
		Println(string(fileBytes))
	}

	os.Exit(0)
}
