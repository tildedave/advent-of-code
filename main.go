package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day2"
)

func main() {
	f, err := os.Open("./inputs/day2.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day2.Run(f)
}
