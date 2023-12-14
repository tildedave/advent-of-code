package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day14.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day14(f)
}
