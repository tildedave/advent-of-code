package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day3"
)

func main() {
	f, err := os.Open("./inputs/day3.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day3.Run(f)
}
