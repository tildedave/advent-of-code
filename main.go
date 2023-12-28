package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day9"
)

func main() {
	f, err := os.Open("./inputs/day9.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day9.Run(f, true)
}
