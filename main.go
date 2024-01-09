package main

import (
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/day18"
)

func main() {
	f, err := os.Open("./inputs/day18-example2.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day18.Run(f, true)
}
