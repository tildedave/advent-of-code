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
		length, err := strconv.ParseInt(res[2][2:7], 16, 64)
		if err != nil {
			log.Fatal(err)
		}
		dirChar := res[2][7]
		var dir int
		switch dirChar {
		case '0':
			dir = DIR_RIGHT
		case '1':
			dir = DIR_DOWN
		case '2':
			dir = DIR_LEFT
		case '3':
			dir = DIR_UP
		default:
			panic("Invalid direction")
		}

		e := edge{dir, int(length)}
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
	// Shoelace formula + missing parts of the area + correction for the first
	// square.
	fmt.Println(-sum/2 + perimLength/2 + 1)
}
