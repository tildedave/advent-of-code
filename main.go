package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day11.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day11(f)
}
