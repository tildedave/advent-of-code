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

	// only mark visited on maxSteps

	for _, maxSteps := range []int{65, 196, 327} {
		seen := make(map[string]bool)
		visited := make(map[string]bool)
		queue := make([][]int, 0)
		fmt.Println(maxSteps)
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
				visited[fmt.Sprintf("%d|%d", x, y)] = true
				continue
			}

			for _, d := range [][]int{{0, -1}, {-1, 0}, {0, 1}, {1, 0}} {
				dx, dy := d[0], d[1]
				nx, ny := (x+dx)%columns, (y+dy)%rows
				if nx < 0 {
					nx += columns
				}
				if ny < 0 {
					ny += rows
				}
				if grid[ny*columns+nx] != ROCK {
					queue = append(queue, []int{x + dx, y + dy, steps + 1})
				}
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
}
