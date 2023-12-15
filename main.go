package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day15.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day15(f)
}
