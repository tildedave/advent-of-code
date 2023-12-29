package day10

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"

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

func ManhattanDistance(dx, dy int) int {
	distance := 0
	if dx > 0 {
		distance += dx
	} else {
		distance -= dx
	}
	if dy > 0 {
		distance += dy
	} else {
		distance -= dy
	}
	return distance
}

func Run(f *os.File, partTwo bool) {
	// we're going to use discrete trigonometry + ray tracing.  kind of exciting.
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
		rows++
	}

	// For each asteroid, we find, for all the other asteroids, what the angles is to it.
	// An asteroid is invisible if it has the same angle as another asteroid, but is farther away.
	var maxSeenSoFar int
	var maxAngleDxDy map[float64][][2]int
	var maxAngles []float64
	var maxAsteroidLocation [2]int
	for x := 0; x < columns; x++ {
		for y := 0; y < rows; y++ {
			if !asteroidLocations[y*columns+x] {
				continue
			}

			// this is essentially a hash set.
			angleDxDy := make(map[float64][][2]int)
			angles := make([]float64, 0)

			for location := range asteroidLocations {
				locationX := location % columns
				locationY := location / columns
				if x == locationX && y == locationY {
					continue
				}
				dx := locationX - x
				dy := locationY - y
				angle := math.Atan2(float64(-dy), float64(-dx))
				key := fmt.Sprintf("%.4f", angle)
				truncatedAngle, err := strconv.ParseFloat(key, 64)
				if err != nil {
					// this should never happen
					panic("Could not parse float")
				}
				val, ok := angleDxDy[truncatedAngle]
				if !ok {
					val = make([][2]int, 0)
				}
				val = append(val, [2]int{dx, dy})
				angleDxDy[truncatedAngle] = val
				if !ok {
					angles = append(angles, truncatedAngle)
				}
				// sorting is kind of overkill because we can just selection
				// sort to find the next nearest one.  very fast in practice.
			}
			seen := 0
			for range angleDxDy {
				seen++
			}
			if seen > maxSeenSoFar {
				maxSeenSoFar = seen
				maxAngleDxDy = angleDxDy
				maxAngles = angles
				maxAsteroidLocation = [2]int{x, y}
			}
		}
	}
	if !partTwo {
		fmt.Println(maxSeenSoFar)
	} else {
		// rename these since the max terminology isn't the best
		angleDxDy := maxAngleDxDy
		angles := maxAngles
		slices.Sort(angles)
		// now we find which key is closest to UP, which = pi/2.
		// then we iterate through and vaporize!
		var startIndex int = -1
		for n, angle := range angles {
			if scalar.EqualWithinAbs(angle, math.Pi/2, 1e-3) {
				startIndex = n
				break
			}
		}
		if startIndex == -1 {
			panic("Did not find starting index")
		}
		idx := startIndex
		numVaporized := 0
		for numVaporized < numAsteroids-1 {
			// so now we vaporize the item with the min distance at this angle.
			angle := angles[idx]
			if len(angleDxDy[angle]) != 0 {
				// guard since we might have vaporized all the asteroids at
				// this angle

				// we need to know this because if we vaporize #200, this is
				// the puzzle answer.
				var closestDxDy [2]int
				closestDistanceSoFar := math.MaxInt
				// we'll have to remember this because we want to remove it.
				closestIndex := -1
				for n, a := range angleDxDy[angle] {
					distance := ManhattanDistance(a[0], a[1])
					if distance < closestDistanceSoFar {
						closestDistanceSoFar = distance
						closestDxDy = a
						closestIndex = n
					}
				}

				angleDxDy[angle] = append(angleDxDy[angle][0:closestIndex], angleDxDy[angle][closestIndex+1:]...)
				numVaporized++

				if numVaporized == 200 {
					x, y := maxAsteroidLocation[0]+closestDxDy[0], maxAsteroidLocation[1]+closestDxDy[1]
					fmt.Println(x*100 + y)
				}
			}

			idx = (idx + 1) % len(angles)
		}
	}
}
