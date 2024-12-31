package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.Open("./day19-example.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day19(f)
}
