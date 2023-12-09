package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day9.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day9(f)
}
