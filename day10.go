package main

import (
	"bufio"
	"fmt"
	"os"
)

const VERTICAL = 1
const HORIZONTAL = 2
const NE_BEND_L = 3
const NW_BEND_J = 4
const SW_BEND_7 = 5
const SE_BEND_F = 6
const GROUND = 7
const START = 8

const NORTH = 1
const SOUTH = -1
const EAST = 2
const WEST = -2

func validDistances(maze [][]int, x int, y int) []int {
	// Test all adjacent squares to see which they allow
	valid := make([]int, 0)
	if maze[x-1][y] == VERTICAL || maze[x-1][y] == SW_BEND_7 || maze[x-1][y] == SE_BEND_F {
		valid = append(valid, NORTH)
	}
	if maze[x+1][y] == VERTICAL || maze[x+1][y] == NE_BEND_L || maze[x+1][y] == NW_BEND_J {
		valid = append(valid, SOUTH)
	}
	if maze[x][y-1] == HORIZONTAL || maze[x][y-1] == NE_BEND_L || maze[x][y-1] == SE_BEND_F {
		valid = append(valid, WEST)
	}
	if maze[x][y+1] == HORIZONTAL || maze[x][y+1] == NW_BEND_J || maze[x][y+1] == SW_BEND_7 {
		valid = append(valid, EAST)
	}

	if len(valid) != 2 {
		fmt.Println(valid, x, y, maze)
		panic("More than two valid directions")
	}

	return valid
}

func opposingDirection(direction int) int {
	return -direction
}

func parseChar(c byte) int {
	switch c {
	case '|':
		return VERTICAL
	case '-':
		return HORIZONTAL
	case 'L':
		return NE_BEND_L
	case 'J':
		return NW_BEND_J
	case '7':
		return SW_BEND_7
	case 'F':
		return SE_BEND_F
	case '.':
		return GROUND
	case 'S':
		return START
	default:
		panic("Unsupported character")
	}
}

func validDistancesFromSquare(sq int) []int {
	switch sq {
	case VERTICAL:
		return []int{NORTH, SOUTH}
	case HORIZONTAL:
		return []int{EAST, WEST}
	case NE_BEND_L:
		return []int{NORTH, EAST}
	case NW_BEND_J:
		return []int{NORTH, WEST}
	case SW_BEND_7:
		return []int{SOUTH, WEST}
	case SE_BEND_F:
		return []int{SOUTH, EAST}
	case GROUND:
		panic("Should not get here")
	case START:
		panic("Should not call this function on START")
	default:
		panic("Unsupported character")
	}
}

func traverseMaze(maze [][]int, distances [][]int, x int, y int, direction int, distance int) {
	if direction == NORTH {
		x -= 1
	} else if direction == EAST {
		y += 1
	} else if direction == WEST {
		y -= 1
	} else if direction == SOUTH {
		x += 1
	}
	if maze[x][y] == START {
		// done
		return
	}
	if distances[x][y] == 0 {
		distances[x][y] = distance + 1
	} else {
		distances[x][y] = min(distances[x][y], distance+1)
	}
	// find next direction
	directions := validDistancesFromSquare(maze[x][y])
	var nextDirection int
	for j := 0; j < len(directions); j++ {
		if directions[j] != opposingDirection(direction) {
			nextDirection = directions[j]
			break
		}
	}
	traverseMaze(maze, distances, x, y, nextDirection, distance+1)
}

func day10(f *os.File) {
	scanner := bufio.NewScanner(f)
	maze := make([][]int, 0)
	i := 0
	var l int
	var startPosition []int
	for scanner.Scan() {
		line := scanner.Text()
		l = len(line)
		row := make([]int, l)
		for j := 0; j < len(line); j++ {
			entry := parseChar(line[j])
			if entry == START {
				startPosition = []int{i, j}
			}
			row[j] = entry
		}
		maze = append(maze, row)
		i++
	}

	distances := make([][]int, 0)
	for j := 0; j < i; j++ {
		distances = append(distances, make([]int, l))
	}

	directions := validDistances(maze, startPosition[0], startPosition[1])
	// choose a direction
	traverseMaze(maze, distances, startPosition[0], startPosition[1], directions[0], 0)
	traverseMaze(maze, distances, startPosition[0], startPosition[1], directions[1], 0)
	maxDistance := 0
	for x := 0; x < i; x++ {
		for y := 0; y < l; y++ {
			if distances[x][y] > maxDistance {
				maxDistance = distances[x][y]
			}
		}
	}
	fmt.Println(maxDistance)
}
