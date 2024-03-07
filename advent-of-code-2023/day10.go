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

const UNKNOWN_LOOP_STATE = 0
const IN_LOOP = 1
const INSIDE = 2
const OUTSIDE = 3

func formattedMazeString(maze [][]int) string {
	str := ""
	for i := 0; i < len(maze); i++ {
		line := ""
		for j := 0; j < len(maze[0]); j++ {
			switch maze[i][j] {
			case VERTICAL:
				line += "|"
			case HORIZONTAL:
				line += "-"
			case NE_BEND_L:
				line += "L"
			case NW_BEND_J:
				line += "J"
			case SW_BEND_7:
				line += "7"
			case SE_BEND_F:
				line += "F"
			case GROUND:
				line += "."
			case START:
				line += "S"
			}
		}
		str += line + "\n"
	}
	return str
}

func formattedRegions(loop [][]int) string {
	str := ""
	for i := 0; i < len(loop); i++ {
		line := ""
		for j := 0; j < len(loop[0]); j++ {
			switch loop[i][j] {
			case INSIDE:
				line += "I"
			case OUTSIDE:
				line += "O"
			case IN_LOOP:
				line += "L"
			case UNKNOWN_LOOP_STATE:
				line += "?"
			}
		}
		str += line + "\n"
	}
	return str
}

func expand3x(maze [][]int) [][]int {
	l := len(maze)
	w := len(maze[0])
	expandedMaze := make([][]int, 0)
	for i := 0; i < l; i++ {
		row1 := make([]int, w*3)
		row2 := make([]int, w*3)
		row3 := make([]int, w*3)
		expandedMaze = append(expandedMaze, row1, row2, row3)
		for j := 0; j < w; j++ {
			// By default set everything to ground
			copy(row1[3*j:3*j+3], []int{GROUND, GROUND, GROUND})
			copy(row2[3*j:3*j+3], []int{GROUND, GROUND, GROUND})
			copy(row3[3*j:3*j+3], []int{GROUND, GROUND, GROUND})

			sq := maze[i][j]
			// The middle of these is always the original square
			row2[3*j+1] = sq
			switch sq {
			case START:
				// start maps to start + valid directions from start
				valid := validDistances(maze, i, j)
				if valid[0] == NORTH || valid[1] == NORTH {
					row1[3*j+1] = VERTICAL
				}
				if valid[0] == SOUTH || valid[1] == SOUTH {
					row3[3*j+1] = VERTICAL
				}
				if valid[0] == EAST || valid[1] == EAST {
					row2[3*j+2] = HORIZONTAL
				}
				if valid[0] == WEST || valid[1] == WEST {
					row2[3*j] = HORIZONTAL
				}
			case GROUND:
				// nothing
				break
			case VERTICAL:
				row1[3*j+1] = VERTICAL
				row2[3*j+1] = VERTICAL
				row3[3*j+1] = VERTICAL
			case HORIZONTAL:
				row2[3*j] = HORIZONTAL
				row2[3*j+1] = HORIZONTAL
				row2[3*j+2] = HORIZONTAL
			case NE_BEND_L:
				row1[3*j+1] = VERTICAL
				row2[3*j+2] = HORIZONTAL
			case NW_BEND_J:
				row1[3*j+1] = VERTICAL
				row2[3*j] = HORIZONTAL
			case SE_BEND_F:
				row2[3*j+2] = HORIZONTAL
				row3[3*j+1] = VERTICAL
			case SW_BEND_7:
				row2[3*j] = HORIZONTAL
				row3[3*j+1] = VERTICAL
			default:
				fmt.Println(sq)
				panic("Should not get here - expand3x")
			}
		}
	}

	return expandedMaze
}

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
		panic("More than two valid directions")
	}

	return valid
}

func floodFillDay10(maze [][]int, loop [][]int, i int, j int) {
	// Constraints: starts from an "outside" square
	queue := make([][]int, 0)
	queue = append(queue, []int{i, j})

	visited := make([][]bool, 0)
	for i := 0; i < len(maze); i++ {
		visited = append(visited, make([]bool, len(maze[0])))
	}

	for len(queue) > 0 {
		pos := queue[0]
		queue = queue[1:]
		i := pos[0]
		j := pos[1]

		if visited[i][j] == true {
			continue
		}

		loop[i][j] = OUTSIDE
		visited[i][j] = true

		canNorth := i > 0
		canSouth := i < len(maze)-1
		canEast := j < len(maze[0])-1
		canWest := j > 0

		if canNorth {
			// Is it valid that we could flow north from this square?
			sq := maze[i-1][j]
			if sq == GROUND || sq == VERTICAL || sq == NE_BEND_L || sq == NW_BEND_J {
				if loop[i-1][j] == UNKNOWN_LOOP_STATE {
					queue = append(queue, []int{i - 1, j})
				}
			}
		}
		if canSouth {
			sq := maze[i+1][j]
			if sq == GROUND || sq == VERTICAL || sq == SE_BEND_F || sq == SW_BEND_7 {
				if loop[i+1][j] == UNKNOWN_LOOP_STATE {
					queue = append(queue, []int{i + 1, j})
				}
			}
		}
		if canEast {
			sq := maze[i][j+1]
			if sq == GROUND || sq == HORIZONTAL || sq == SW_BEND_7 || sq == NW_BEND_J {
				if loop[i][j+1] == UNKNOWN_LOOP_STATE {
					queue = append(queue, []int{i, j + 1})
				}
			}
		}
		if canWest {
			sq := maze[i][j-1]
			if sq == GROUND || sq == HORIZONTAL || sq == SW_BEND_7 || sq == NW_BEND_J {
				if loop[i][j-1] == UNKNOWN_LOOP_STATE {
					queue = append(queue, []int{i, j - 1})
				}
			}
		}
	}
	// That should do it
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
		panic("Should not get here - valid distances from ground")
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
	var w int
	var startPosition []int
	for scanner.Scan() {
		line := scanner.Text()
		w = len(line)
		row := make([]int, w)
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

	maze = expand3x(maze)
	distances := make([][]int, 0)
	for j := 0; j < i; j++ {
		distances = append(distances, make([]int, w*3))
		distances = append(distances, make([]int, w*3))
		distances = append(distances, make([]int, w*3))
	}

	startPosition[0] = 3*startPosition[0] + 1
	startPosition[1] = 3*startPosition[1] + 1

	if maze[startPosition[0]][startPosition[1]] != START {
		panic("Expanded maze did not start at start")
	}

	directions := validDistances(maze, startPosition[0], startPosition[1])
	fmt.Println("valid directions", directions)
	// choose a direction
	traverseMaze(maze, distances, startPosition[0], startPosition[1], directions[0], 0)
	traverseMaze(maze, distances, startPosition[0], startPosition[1], directions[1], 0)

	loop := distances
	for i := 0; i < len(maze); i++ {
		for j := 0; j < len(maze[0]); j++ {
			if loop[i][j] > 0 || maze[i][j] == START {
				loop[i][j] = IN_LOOP
			} else if i == 0 || i == len(maze)-1 || j == 0 || j == len(maze[0])-1 {
				loop[i][j] = OUTSIDE
			}
		}
	}

	fmt.Println(formattedMazeString(maze))

	// So now we can flood fill without worrying about "squeezing through pipes".
	for i := 0; i < len(maze); i++ {
		for j := 0; j < len(maze[0]); j++ {
			if loop[i][j] == OUTSIDE {
				floodFillDay10(maze, loop, i, j)
			}
		}
	}
	for i := 0; i < len(maze); i++ {
		for j := 0; j < len(maze[0]); j++ {
			if loop[i][j] == UNKNOWN_LOOP_STATE {
				loop[i][j] = INSIDE
			}
		}
	}

	insideCount := 0
	for i := 0; i < len(maze); i++ {
		for j := 0; j < len(maze[0]); j++ {
			if i%3 == 1 && j%3 == 1 && loop[i][j] == INSIDE {
				insideCount += 1
			}
		}
	}
	fmt.Println(formattedRegions(loop))
	fmt.Println(insideCount)
}
