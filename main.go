package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day1"
)

func main() {
	f, err := os.Open("./inputs/day1.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day1.Run(f)
}
