package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day17"
)

func main() {
	f, err := os.Open("./inputs/day17.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day17.Run(f, true)
}
