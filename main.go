package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day14"
)

func main() {
	f, err := os.Open("./inputs/day14.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day14.Run(f, true)
}
