package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type gameResult struct {
	winningNumbers map[int]bool
	gameNumbers    map[int]bool
}

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
	cardNum := 0
	cardData := make(map[int]gameResult)
	cardCount := make(map[int]int)

	for scanner.Scan() {
		line := scanner.Text()
		cardNum += 1
		splitLine := strings.Split(line, ": ")
		res := strings.Split(splitLine[1], " | ")
		winningNumbers := parseNumberList(res[0])
		myNumbers := parseNumberList(res[1])
		result := gameResult{winningNumbers: winningNumbers, gameNumbers: myNumbers}
		cardData[cardNum] = result
	}
	maxCard := cardNum

	for cardNum = 1; cardNum <= maxCard; cardNum++ {
		result := cardData[cardNum]
		cardCount[cardNum] += 1
		for j := 0; j < cardCount[cardNum]; j++ {
			var score = 0
			for num := range result.gameNumbers {
				if result.winningNumbers[num] {
					score += 1
				}
			}
			for i := cardNum + 1; i < cardNum+1+score; i++ {
				if len(cardData[i].gameNumbers) > 0 { // stupid nullity check
					cardCount[i] += 1
				}
			}
		}
	}
	sum := 0
	for _, v := range cardCount {
		sum += v
	}
	fmt.Println(sum)
}
