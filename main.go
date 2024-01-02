package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day16"
)

func main() {
	f, err := os.Open("./inputs/day16.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day16.Run(f, true)
}
