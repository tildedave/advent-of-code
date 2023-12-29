package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day10"
)

func main() {
	f, err := os.Open("./inputs/day10-example5.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day10.Run(f, false)
}
