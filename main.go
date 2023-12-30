package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day12"
)

func main() {
	f, err := os.Open("./inputs/day12.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day12.Run(f, true)
}
