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

func day2(f *os.File) {
	scanner := bufio.NewScanner(f)
	var bag = map[string]int{
		"red":   12,
		"green": 13,
		"blue":  14,
	}

	var sum int = 0
	for scanner.Scan() {
		text := scanner.Text()
		game := strings.Split(text, ": ")
		gameNum, err := strconv.ParseInt(strings.Split(game[0], " ")[1], 10, 32)
		if err != nil {
			log.Fatal(err)
		}
		results := strings.Split(game[1], "; ")
		var possible = true
		for _, result := range results {
			m := parseResult(result)
			for color, num := range m {
				if bag[color] < num {
					possible = false
					break
				}
			}
		}
		if possible {
			sum += int(gameNum)
		}
	}
	fmt.Println(sum)
}
