package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func parseResult(result string) map[string]int {
	m := make(map[string]int)
	cubes := strings.Split(result, ", ")
	for _, cube := range cubes {
		data := strings.Split(cube, " ")
		i, err := strconv.ParseInt(data[0], 10, 32)
		if err != nil {
			log.Fatal("Invalid number", err)
		}
		m[data[1]] = int(i)
	}
	return m
}

func isPossible(bag map[string]int, results []map[string]int) bool {
	for _, m := range results {
		for color, num := range m {
			if bag[color] < num {
				return false
			}
		}
	}
	return true
}

func minimumBag(results []map[string]int) map[string]int {
	minBag := make(map[string]int, 0)
	for _, m := range results {
		for color, num := range m {
			minBag[color] = max(minBag[color], num)
		}
	}
	return minBag
}

func bagPower(bag map[string]int) int {
	total := 1
	for _, v := range bag {
		total *= v
	}
	return total
}

func day2(f *os.File) {
	scanner := bufio.NewScanner(f)
	var sum int = 0
	for scanner.Scan() {
		text := scanner.Text()
		game := strings.Split(text, ": ")
		results := strings.Split(game[1], "; ")
		parsedResults := make([]map[string]int, 0)
		for _, result := range results {
			m := parseResult(result)
			parsedResults = append(parsedResults, m)
		}
		minBag := minimumBag(parsedResults)
		sum += bagPower(minBag)
	}
	fmt.Println(sum)
}
