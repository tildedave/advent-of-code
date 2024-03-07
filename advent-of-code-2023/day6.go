package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func day6(f *os.File) {
	scanner := bufio.NewScanner(f)
	var time int
	var distance int
	for scanner.Scan() {
		text := scanner.Text()
		if strings.HasPrefix(text, "Time:") {
			time = extractFields(strings.ReplaceAll(strings.TrimSpace(strings.Split(text, "Time:")[1]), " ", ""))[0]
		}
		if strings.HasPrefix(text, "Distance:") {
			distance = extractFields(strings.ReplaceAll(strings.TrimSpace(strings.Split(text, "Distance:")[1]), " ", ""))[0]
		}
	}
	waysToWin := 0
	for j := 0; j < time; j++ {
		// simulate hold down for j seconds
		// hold down for j seconds means we travel j * (time - j) distance
		traveledDistance := j * (time - j)
		if traveledDistance > distance {
			waysToWin++
		}
	}
	fmt.Println(waysToWin)
}
