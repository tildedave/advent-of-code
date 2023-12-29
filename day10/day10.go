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

	// For each asteroid, we find, for all the other asteroids, what the angles is to it.
	// An asteroid is invisible if it has the same angle as another asteroid, but is farther away.
	var maxSeenSoFar int
	for x := 0; x < columns; x++ {
		for y := 0; y < rows; y++ {
			if !asteroidLocations[y*columns+x] {
				continue
			}

			// this is essentially a hash set.
			uniqueAngles := make(map[string]bool)

			for location := range asteroidLocations {
				locationX := location % columns
				locationY := location / columns
				if x == locationX && y == locationY {
					continue
				}
				dx := x - locationX
				dy := y - locationY
				angle := math.Atan2(float64(dy), float64(dx))
				uniqueAngles[fmt.Sprintf("%.4f", angle)] = true
			}
			seen := 0
			for range uniqueAngles {
				seen++
			}
			if seen > maxSeenSoFar {
				maxSeenSoFar = seen
			}
		}
	}
	fmt.Println(maxSeenSoFar)
}
