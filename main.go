package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day8"
)

func main() {
	f, err := os.Open("./inputs/day8.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day8.Run(f, true)
}
