package main

import (
	"bufio"
	"fmt"
	"os"
)

const START_POSITION = 0
const ROCK = 1
const GARDEN_PLOT = 2

func day21(f *os.File) {
	scanner := bufio.NewScanner(f)
	// For this problem I'm actually going to use "x" as the column and "y" as
	// the row.  We've also done a bunch of 2x grids so I'm going to 1x it for
	// extra style points.
	grid := make([]int, 0)
	columns := 0
	rows := 0
	startX := -1
	startY := -1
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		columns = len(line)
		for i, c := range line {
			switch c {
			case 'S':
				row[i] = START_POSITION
				startX = i
				startY = rows
			case '.':
				row[i] = GARDEN_PLOT
			case '#':
				row[i] = ROCK
			}
		}
		grid = append(grid, row...)
		rows++
	}

	queue := make([][]int, 0)
	// only mark visited on maxSteps
	visited := make([]bool, len(grid))
	seen := make(map[string]bool)

	maxSteps := 64
	queue = append(queue, []int{startX, startY, 0})
	for len(queue) > 0 {
		item := queue[0]
		queue = queue[1:]
		x, y, steps := item[0], item[1], item[2]
		key := fmt.Sprintf("%d-%d-%d", x, y, steps)

		if seen[key] {
			continue
		} else {
			seen[key] = true
		}

		if steps == maxSteps {
			visited[y*columns+x] = true
			continue
		}

		canUp := y > 0 && grid[(y-1)*columns+x] != ROCK
		canDown := y < rows-1 && grid[(y+1)*columns+x] != ROCK
		canLeft := x > 0 && grid[y*columns+(x-1)] != ROCK
		canRight := x < columns-1 && grid[y*columns+(x+1)] != ROCK

		if canUp {
			queue = append(queue, []int{x, y - 1, steps + 1})
		}
		if canDown {
			queue = append(queue, []int{x, y + 1, steps + 1})
		}
		if canLeft {
			queue = append(queue, []int{x - 1, y, steps + 1})
		}
		if canRight {
			queue = append(queue, []int{x + 1, y, steps + 1})
		}
	}
	numVisited := 0
	for _, v := range visited {
		if v {
			numVisited++
		}
	}
	fmt.Println(numVisited)
}
