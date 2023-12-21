package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./input-day21.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day21(f)
}
