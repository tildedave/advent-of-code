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

	// currentX := 0
	// currentY := 0
	// maxX := 0
	// maxY := 0
	// minX := 0
	// minY := 0
	// for _, e := range instructions {
	// 	switch e.direction {
	// 	case DIR_LEFT:
	// 		currentY -= e.length
	// 	case DIR_RIGHT:
	// 		currentY += e.length
	// 	case DIR_UP:
	// 		currentX -= e.length
	// 	case DIR_DOWN:
	// 		currentX += e.length
	// 	}
	// 	if currentX > maxX {
	// 		maxX = currentX
	// 	}
	// 	if currentY > maxY {
	// 		maxY = currentY
	// 	}
	// 	if currentX < minX {
	// 		minX = currentX
	// 	}
	// 	if currentY < minY {
	// 		minY = currentY
	// 	}
	// }
	// maxX += 1
	// maxY += 1
	// // Add a moat around the whole thing so that
	// // x = 0, y = 0, x = len(grid) - 1, y = len(grid[0]) - 1 are all outside.
	// maxX += 2
	// maxY += 2
	// minX -= 2
	// minY -= 2

	// Maybe I should have used a map instead of this grid thing.
	// fmt.Println(minX, minY, maxX, maxY, maxX-minX)
	// grid := make([][]edge, maxX-minX)
	// for i := 0; i < maxX-minX; i++ {
	// 	grid[i] = make([]edge, maxY-minY)
	// }

	grid := make(map[string]edge)
	currentX := 0
	currentY := 0
	maxX := 0
	maxY := 0
	minX := 0
	minY := 0

	for _, e := range instructions {
		switch e.direction {
		case DIR_LEFT:
			for i := 0; i < e.length; i++ {
				grid[gridKey(currentX, currentY-i)] = e
			}
			currentY -= e.length
		case DIR_RIGHT:
			for i := 0; i < e.length; i++ {
				grid[gridKey(currentX, currentY+i)] = e
			}
			currentY += e.length
		case DIR_UP:
			for i := 0; i < e.length; i++ {
				grid[gridKey(currentX-i, currentY)] = e
			}
			currentX -= e.length
		case DIR_DOWN:
			for i := 0; i < e.length; i++ {
				grid[gridKey(currentX+i, currentY)] = e
			}
			currentX += e.length
		}
		if currentX > maxX {
			maxX = currentX
		}
		if currentY > maxY {
			maxY = currentY
		}
		if currentX < minX {
			minX = currentX
		}
		if currentY < minY {
			minY = currentY
		}
	}

	// flood fill needs to start from the outside, so we add a moat of 1 line
	// around everything
	visited := make(map[string]bool)
	queue := make([][]int, 0)
	for x := minX - 1; x <= maxX+1; x++ {
		queue = append(queue, []int{x, minY - 1})
		queue = append(queue, []int{x, maxY + 1})
	}
	for y := minY - 1; y <= maxY+1; y++ {
		queue = append(queue, []int{minX - 1, y})
		queue = append(queue, []int{maxX + 1, y})
	}
	for len(queue) > 0 {
		r := queue[0]
		queue = queue[1:]
		x, y := r[0], r[1]
		k := gridKey(x, y)
		if grid[k].length != 0 {
			panic("Hit an edge, should not do this")
		}

		if visited[k] {
			continue
		}
		visited[k] = true
		canUp := x > minX && grid[gridKey(x-1, y)].length == 0
		canDown := x < maxX && grid[gridKey(x+1, y)].length == 0
		canLeft := y > minY && grid[gridKey(x, y-1)].length == 0
		canRight := y < maxY && grid[gridKey(x, y+1)].length == 0
		if canUp {
			queue = append(queue, []int{x - 1, y})
		}
		if canDown {
			queue = append(queue, []int{x + 1, y})
		}
		if canLeft {
			queue = append(queue, []int{x, y - 1})
		}
		if canRight {
			queue = append(queue, []int{x, y + 1})
		}
	}

	count := 0
	str := ""
	for i := minX - 1; i <= maxX+1; i++ {
		line := ""
		for j := minY - 1; j <= maxY+1; j++ {
			if visited[gridKey(i, j)] {
				line += "#"
			} else {
				line += "."
				count++
			}
		}
		str += line + "\n"
	}
	fmt.Println(str)
	fmt.Println(count)
}
