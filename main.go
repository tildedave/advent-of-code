package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day4"
)

func main() {
	f, err := os.Open("./inputs/day4.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day4.Run(f)
}
