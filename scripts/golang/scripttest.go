/// 2>/dev/null ; exec gorun "$0" "$@"

package main

import (
	"log"
	. "github.com/bitfield/script"
	"os"
)

func main() {
	url := os.Args[1]
	out := os.Args[2]

	_, err := Exec("youtube-dl --no-playlist --prefer-ffmpeg --extract-audio --audio-format mp3 " + url + " --output " + out ).Stdout()
	if err != nil {
		log.Fatalln(err.Error())
	}
}