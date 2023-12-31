package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day13"
)

func main() {
	f, err := os.Open("./inputs/day13.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day13.Run(f, true)
}
