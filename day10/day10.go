package day10

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

func PrintSeenMap(grid []int, rows int, columns int, seen map[int]bool) {
	str := ""
	for i := range grid {
		if i > 0 && i%columns == 0 {
			str += "\n"
		}
		if grid[i] == 1 {
			if seen[i] {
				str += "X"
			} else {
				str += "#"
			}
		} else {
			str += "."
		}
	}
	fmt.Println(str)
}

func Run(f *os.File, partTwo bool) {
	// we're going to use discrete trigonometry + ray tracing.  kind of exciting.
	grid := make([]int, 0)
	var rows int
	var columns int

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		columns = len(line)
		for n, c := range line {
			switch c {
			case '.':
				row[n] = 0
			case '#':
				row[n] = 1
			default:
				panic("Invalid input")
			}
		}
		grid = append(grid, row...)
		rows++
	}

	// so for each asteroid in the grid, we ray trace from it at all angles.
	// I guess the angle delta can be kind of coarse, 5 degrees ?
	// Correctness really depends on the size of the map.
	var maxSeenSoFar int
	for x := 0; x < columns; x++ {
		for y := 0; y < rows; y++ {
			seen := make(map[int]bool)
			for radian := float64(0); radian < 2*math.Pi; radian += (2 * math.Pi / 60) {
				length := float64(1)
				for {
					dx := int(length * math.Cos(radian))
					dy := int(length * math.Sin(radian))
					length += float64(1)
					if dx == 0 && dy == 0 {
						continue
					}

					// Possibilities: ray goes off screen (violates 0 <= x + dx < columns
					// or  0 <= y + dy < rows), ray hits asteroid, in which case we mark
					// as seen and continue.
					if !(0 <= x+dx && x+dx < columns && 0 <= y+dy && y+dy < rows) {
						break
					}

					location := (y+dy)*columns + (x + dx)
					if grid[location] == 1 {
						// we see the asteroid.  done.
						seen[location] = true
						break
					}
				}
			}
			seenCount := 0
			for range seen {
				seenCount += 1
			}
			if seenCount > maxSeenSoFar {
				maxSeenSoFar = seenCount
			}
		}
	}
	fmt.Println(maxSeenSoFar)
}
