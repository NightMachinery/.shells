package main

import (
	"fmt"
	"io/ioutil"
	"unicode"
	"os"
	"log"
)

func main() {
	inBytes, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		log.Fatalln(err.Error())
	}
	input := string(inBytes)

	// fmt.Printf("%x\n", input)

	output := make([]rune, 0)
	for _, rune := range input  {
		if ! unicode.In(rune, unicode.Variation_Selector) {
			output = append(output, rune)
		}
	}

	// fmt.Printf("%x\n", output)

	fmt.Printf("%s", string(output))

}
