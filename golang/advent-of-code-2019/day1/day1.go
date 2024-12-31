package day1

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func TotalFuel(n int) int {
	total := 0
	for {
		fuelAmount := n/3 - 2
		if fuelAmount <= 0 {
			break
		}
		total += fuelAmount
		n = fuelAmount
	}
	return total
}

func Run(f *os.File) {
	scanner := bufio.NewScanner(f)
	total := 0
	for scanner.Scan() {
		line := scanner.Text()
		n, err := strconv.ParseInt(line, 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		total += TotalFuel(int(n))
	}
	fmt.Println(total)
}
