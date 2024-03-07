package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func isSymbol(b byte) bool {
	return b != '.' && !isDigit(b)
}

const UP = 1
const UP_RIGHT = 2
const RIGHT = 3
const DOWN_RIGHT = 4
const DOWN = 5
const DOWN_LEFT = 6
const LEFT = 7
const UP_LEFT = 8
const START_DIR = UP
const END_DIR = UP_LEFT

func extractNumber(parsedInput []string, row int, col int) (int64, int, int) {
	var startCol int
	var endCol int
	for startCol = col - 1; startCol >= 0 && isDigit(parsedInput[row][startCol]); startCol-- {
	}
	for endCol = col + 1; endCol < len(parsedInput[row]) && isDigit(parsedInput[row][endCol]); endCol++ {
	}
	num, err := strconv.ParseInt(parsedInput[row][startCol+1:endCol], 10, 64)
	if err != nil {
		log.Fatal(err)
	}
	return num, startCol + 1, endCol
}

func day3(f *os.File) {
	scanner := bufio.NewScanner(f)
	var parsedInput []string
	for scanner.Scan() {
		line := scanner.Text()
		parsedInput = append(parsedInput, line)
	}
	totalGearRatio := 0
	for row, line := range parsedInput {
		for col := 0; col < len(line); col++ {
			chr := line[col]
			if chr != byte('*') {
				continue
			}

			// found gear, test all adjacent areas.
			// we probably need to worry about duplicates, so we should
			// insert this into some array that we can de-dupe later.
			canLookUp := row > 0
			canLookDown := row < len(line)-1
			canLookLeft := col > 0
			canLookRight := col < len(line)-1
			adjacent := make(map[int64]map[string]bool, 0)
			for dir := START_DIR; dir <= END_DIR; dir++ {
				candidateRow := row
				candidateCol := col
				if dir == UP || dir == UP_LEFT || dir == UP_RIGHT {
					if !canLookUp {
						continue
					}
					candidateRow -= 1
				}
				if dir == DOWN || dir == DOWN_LEFT || dir == DOWN_RIGHT {
					if !canLookDown {
						continue
					}
					candidateRow += 1
				}
				if dir == LEFT || dir == UP_LEFT || dir == DOWN_LEFT {
					if !canLookLeft {
						continue
					}
					candidateCol -= 1
				}
				if dir == RIGHT || dir == UP_RIGHT || dir == DOWN_RIGHT {
					if !canLookRight {
						continue
					}
					candidateCol += 1
				}

				if isDigit(parsedInput[candidateRow][candidateCol]) {
					num, start, finish := extractNumber(parsedInput, candidateRow, candidateCol)
					if adjacent[num] == nil {
						adjacent[num] = make(map[string]bool, 0)
					}
					adjacent[num][fmt.Sprintf("%d-%d-%d", candidateRow, start, finish)] = true
				}
			}

			if len(adjacent) == 2 {
				// only 2 counts
				gearRatio := 1
				for k := range adjacent {
					gearRatio *= int(k)
				}
				totalGearRatio += gearRatio
			}
		}
	}
	fmt.Println(totalGearRatio)
}
