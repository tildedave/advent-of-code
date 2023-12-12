package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day12.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day12(f)
}
