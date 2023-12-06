package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func day6(f *os.File) {
	scanner := bufio.NewScanner(f)
	var times []int
	var distances []int
	for scanner.Scan() {
		text := scanner.Text()
		if strings.HasPrefix(text, "Time:") {
			times = extractFields(strings.TrimSpace(strings.Split(text, "Time:")[1]))
		}
		if strings.HasPrefix(text, "Distance:") {
			distances = extractFields(strings.TrimSpace(strings.Split(text, "Distance:")[1]))
		}
	}
	totalTimes := 1
	for i := 0; i < len(times); i++ {
		time, distance := times[i], distances[i]
		waysToWin := 0
		for j := 0; j < time; j++ {
			// simulate hold down for j seconds
			// hold down for j seconds means we travel j * (time - j) distance
			traveledDistance := j * (time - j)
			if traveledDistance > distance {
				waysToWin++
			}
		}
		totalTimes *= waysToWin
	}
	fmt.Println(totalTimes)
}
