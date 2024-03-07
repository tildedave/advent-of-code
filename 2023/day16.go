package main

import (
	"bufio"
	"fmt"
	"os"
)

const DIR_LEFT = 1
const DIR_UP = 2
const DIR_RIGHT = 3
const DIR_DOWN = 4

func move(queue [][]int, grid *[][]byte, x int, y int, dir int) [][]int {
	if x >= 0 && x < len(*grid) && y >= 0 && y < len((*grid)[0]) {
		return append(queue, []int{x, y, dir})
	}
	return queue
}

func formatSeenGrid(seen [][]bool) string {
	str := ""
	for i := 0; i < len(seen); i++ {
		line := ""
		for j := 0; j < len(seen[0]); j++ {
			if seen[i][j] {
				line += "#"
			} else {
				line += "."
			}
		}
		str += line + "\n"
	}
	return str
}

func key(q []int) string {
	return fmt.Sprintf("%d-%d-%d", q[0], q[1], q[2])
}

func count(grid [][]byte, x int, y int, dir int) int {
	seen := make([][]bool, 0)
	for i := 0; i < len(grid); i++ {
		seen = append(seen, make([]bool, len(grid[0])))
	}

	queue := make([][]int, 0)
	queue = append(queue, []int{x, y, dir})
	queueSeen := make(map[string]bool)
	for len(queue) > 0 {
		x, y, dir := queue[0][0], queue[0][1], queue[0][2]
		k := key(queue[0])
		queue = queue[1:]
		if queueSeen[k] {
			continue
		}
		queueSeen[k] = true
		seen[x][y] = true
		switch grid[x][y] {
		case '.':
			// continue in the same direction unless we're off the screen
			switch dir {
			case DIR_DOWN:
				queue = move(queue, &grid, x+1, y, dir)
			case DIR_UP:
				queue = move(queue, &grid, x-1, y, dir)
			case DIR_RIGHT:
				queue = move(queue, &grid, x, y+1, dir)
			case DIR_LEFT:
				queue = move(queue, &grid, x, y-1, dir)
			}
		case '/':
			switch dir {
			case DIR_DOWN:
				queue = move(queue, &grid, x, y-1, DIR_LEFT)
			case DIR_UP:
				queue = move(queue, &grid, x, y+1, DIR_RIGHT)
			case DIR_LEFT:
				queue = move(queue, &grid, x+1, y, DIR_DOWN)
			case DIR_RIGHT:
				queue = move(queue, &grid, x-1, y, DIR_UP)
			}
		case '\\':
			switch dir {
			case DIR_DOWN:
				queue = move(queue, &grid, x, y+1, DIR_RIGHT)
			case DIR_UP:
				queue = move(queue, &grid, x, y-1, DIR_LEFT)
			case DIR_LEFT:
				queue = move(queue, &grid, x-1, y, DIR_UP)
			case DIR_RIGHT:
				queue = move(queue, &grid, x+1, y, DIR_DOWN)
			}
		case '|':
			switch dir {
			case DIR_DOWN:
				queue = move(queue, &grid, x+1, y, dir)
			case DIR_UP:
				queue = move(queue, &grid, x-1, y, dir)
			case DIR_LEFT:
				fallthrough
			case DIR_RIGHT:
				queue = move(queue, &grid, x-1, y, DIR_UP)
				queue = move(queue, &grid, x+1, y, DIR_DOWN)
			}
		case '-':
			{
				switch dir {
				case DIR_LEFT:
					queue = move(queue, &grid, x, y-1, dir)
				case DIR_RIGHT:
					queue = move(queue, &grid, x, y+1, dir)
				case DIR_DOWN:
					fallthrough
				case DIR_UP:
					queue = move(queue, &grid, x, y-1, DIR_LEFT)
					queue = move(queue, &grid, x, y+1, DIR_RIGHT)
				}
			}
		}
	}
	count := 0
	for i := 0; i < len(seen); i++ {
		for j := 0; j < len(seen[0]); j++ {
			if seen[i][j] {
				count++
			}
		}
	}
	return count
}

func day16(f *os.File) {
	scanner := bufio.NewScanner(f)
	grid := make([][]byte, 0)
	i := 0
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]byte, len(line))
		copy(row, line)
		grid = append(grid, row)
		i++
	}
	maxCount := 0
	for i := 0; i < len(grid); i++ {
		c := count(grid, i, 0, DIR_RIGHT)
		if c > maxCount {
			maxCount = c
		}
		c = count(grid, i, len(grid[0])-1, DIR_LEFT)
		if c > maxCount {
			maxCount = c
		}
	}
	for j := 0; j < len(grid); j++ {
		c := count(grid, 0, j, DIR_DOWN)
		if c > maxCount {
			maxCount = c
		}
		c = count(grid, len(grid)-1, j, DIR_UP)
		if c > maxCount {
			maxCount = c
		}
	}
	fmt.Println(maxCount)
}
