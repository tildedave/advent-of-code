package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day23-test.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day23(f)
}
