package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func parseNumberList(str string) map[int]bool {
	var nums map[int]bool = make(map[int]bool)
	for _, numStr := range strings.Fields(str) {
		parsedNum, err := strconv.ParseInt(numStr, 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		nums[int(parsedNum)] = true
	}
	return nums
}

func day4(f *os.File) {
	scanner := bufio.NewScanner(f)
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(strings.Split(line, ": ")[1], " | ")
		winningNumbers := parseNumberList(res[0])
		myNumbers := parseNumberList(res[1])
		var score = 0
		for num := range myNumbers {
			if winningNumbers[num] {
				if score == 0 {
					score = 1
				} else {
					score *= 2
				}
			}
		}
		sum += score
	}
	fmt.Println(sum)
}
