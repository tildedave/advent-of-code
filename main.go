package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day6"
)

func main() {
	f, err := os.Open("./inputs/day6.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day6.Run(f, false)
}
