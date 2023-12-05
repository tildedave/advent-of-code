package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day5.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day5(f)
}
