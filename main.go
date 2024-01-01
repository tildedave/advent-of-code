package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day15"
)

func main() {
	f, err := os.Open("./inputs/day15.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day15.Run(f, true)
}
