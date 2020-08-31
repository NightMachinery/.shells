package main

import (
	"log"
	. "github.com/bitfield/script"
)

func main() {
	test2 := "Alice's pet"
	_, err := Exec("brishzq.zsh arger hi 'test 1' " + test2 ).Stdout()
	if err != nil {
		log.Fatalln(err.Error())
	}
}