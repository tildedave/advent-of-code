package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day7"
)

func main() {
	f, err := os.Open("./inputs/day7.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day7.Run(f, true)
}
