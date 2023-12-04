package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day4.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day4(f)
}
