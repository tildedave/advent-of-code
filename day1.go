package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func isDigit(b byte) bool {
	return 48 <= b && b <= 57
}

func main() {
	f, err := os.Open("./input-day1.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	sum := 0
	for scanner.Scan() {
		var firstDigit int
		var secondDigit int

		line := scanner.Text()
		for i := 0; i < len(line); i++ {
			b := line[i]
			if isDigit(b) {
				firstDigit = int(b) - 48
				break
			}
		}
		for i := len(line) - 1; i >= 0; i-- {
			b := line[i]
			if isDigit(b) {
				secondDigit = int(b) - 48
				break
			}
		}
		sum += firstDigit*10 + secondDigit
	}
	fmt.Println(sum)
}
