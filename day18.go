package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type edge = struct {
	direction int
	length    int
	color     int
}

func gridKey(x int, y int) string {
	return fmt.Sprintf("%d-%d", x, y)
}

func day18(f *os.File) {
	scanner := bufio.NewScanner(f)
	instructions := make([]edge, 0)
	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Fields(line)
		n, err := strconv.ParseInt(res[1], 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		if len(res[0]) != 1 {
			panic("Direction had more than one char")
		}
		var dir int
		switch res[0][0] {
		case 'R':
			dir = DIR_RIGHT
		case 'D':
			dir = DIR_DOWN
		case 'U':
			dir = DIR_UP
		case 'L':
			dir = DIR_LEFT
		default:
			panic("Invalid direction")
		}
		hexCode, err := strconv.ParseInt(strings.ReplaceAll(res[2][2:], ")", ""), 16, 64)
		if err != nil {
			log.Fatal(err)
		}
		e := edge{dir, int(n), int(hexCode)}
		instructions = append(instructions, e)
	}

	currentX := 0
	currentY := 0
	vertices := make([][]int, 0)
	perimLength := 0

	for _, e := range instructions {
		// fmt.Println(currentX, currentY)
		vertices = append(vertices, []int{currentX, currentY})
		switch e.direction {
		case DIR_LEFT:
			currentY -= e.length
		case DIR_RIGHT:
			currentY += e.length
		case DIR_UP:
			currentX -= e.length
		case DIR_DOWN:
			currentX += e.length
		}
		perimLength += e.length
	}
	if currentX != 0 || currentY != 0 {
		fmt.Println(currentX, currentY)
		panic("Did not return to start")
	}
	// Shoelace formula, 2A = |x1 x2| + |x2 x3| + ...
	//                        |y1 y2| + |y2 y3|
	sum := 0
	for i, curr := range vertices {
		nextIndex := (i + 1) % len(vertices)
		next := vertices[nextIndex]
		sum += (curr[1] + next[1]) * (curr[0] - next[0])
	}
	// Why is this right?
	fmt.Println(-sum/2 + perimLength/2 + 1)
}
