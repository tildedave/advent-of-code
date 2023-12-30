package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day11"
)

func main() {
	f, err := os.Open("./inputs/day11.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day11.Run(f, false)
}
