package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day5"
)

func main() {
	f, err := os.Open("./inputs/day5.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day5.Run(f, false)
}
