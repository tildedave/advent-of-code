package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day11-test.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day11(f)
}
