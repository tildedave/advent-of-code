package day10

import (
	"bufio"
	"fmt"
	"math"
	"os"

	"gonum.org/v1/gonum/floats/scalar"
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
	numAsteroids := 0
	asteroidLocations := make(map[int]bool)

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
				numAsteroids++
				asteroidLocations[rows*columns+n] = true
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
			// for x := 5; x == 5; x++ {
			// 	for y := 8; y == 8; y++ {
			if !asteroidLocations[y*columns+x] {
				continue
			}

			seen := make(map[int]bool)
			blocked := make(map[int]bool)
			for radian := float64(0); radian < 2*math.Pi; radian += (2 * math.Pi / 256.0) {
				length := float64(1)
				blockLocation := -1
				for {
					// I feel like the ways I'm doing dx/dy are not correct
					// relative to the puzzle expectation.
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
						// we see the asteroid. this asteroid blocks other
						// asteroids that would be seen in this direction.
						// so we remember that we have a blocking asteroid.
						if blockLocation == -1 {
							seen[location] = true
							blockLocation = location
						} else if blockLocation != location {
							// Sometimes the integer rounding for the asteroid gets us
							// to a different angle.
							blockX := blockLocation % columns
							blockY := blockLocation / columns
							blockAngle := math.Atan2(float64(blockY-y), float64(blockX-x))
							currentAngle := math.Atan2(float64(dy), float64(dx))
							if scalar.EqualWithinAbs(currentAngle, blockAngle, 1e-6) {
								blocked[location] = true
							} else if x == 0 && y == 1 {
								fmt.Println("angles did not match", currentAngle, blockAngle, blockX, blockY, dx, dy)
							}
						}
					}
				}
			}
			// This is kind of weird, we count the total number of asteroids
			// minus the asteroids that were explicitly blocked.
			seenCount := numAsteroids - 1
			for range blocked {
				seenCount--
			}
			// fmt.Println("blocked map")
			// PrintSeenMap(grid, rows, columns, blocked)
			// fmt.Println("blocked map over")
			// seenCount := 0
			// for range seen {
			// 	seenCount += 1
			// }
			fmt.Println("seen at", x, y, "is", seenCount)
			if seenCount >= maxSeenSoFar {
				maxSeenSoFar = seenCount
				fmt.Println("*********")
				PrintSeenMap(grid, rows, columns, blocked)
				fmt.Println("*********")
				PrintSeenMap(grid, rows, columns, seen)
				fmt.Println("*********")
			}
		}
	}
	fmt.Println(maxSeenSoFar)
}
