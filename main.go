package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day17.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day17(f)
}
