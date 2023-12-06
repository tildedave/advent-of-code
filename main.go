package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day6.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day6(f)
}
