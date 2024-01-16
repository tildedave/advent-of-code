package day24

import (
	"bufio"
	"fmt"
	"math/bits"
	"os"
)

func gridToString(grid uint) string {
	ret := ""
	for y := 0; y < 5; y++ {
		if y > 0 {
			ret += "\n"
		}

		for x := 0; x < 5; x++ {
			if grid&(1<<(x+(y*5))) != 0 {
				ret += "#"
			} else {
				ret += "."
			}
		}
	}
	return ret
}

func createAdjacencyMasks() []uint {
	masks := make([]uint, 25)
	for y := 0; y < 5; y++ {
		for x := 0; x < 5; x++ {
			var mask uint
			if y != 0 {
				mask |= 1 << ((y-1)*5 + x)
			}
			if y != 4 {
				mask |= 1 << ((y+1)*5 + x)
			}
			if x != 0 {
				mask |= 1 << (5*y + (x - 1))
			}
			if x != 4 {
				mask |= 1 << (5*y + (x + 1))
			}
			masks[5*y+x] = mask
		}
	}
	return masks
}

func tick(grid uint, adjacencyMasks []uint) uint {
	nextGrid := uint(0)
	for y := 0; y < 5; y++ {
		for x := 0; x < 5; x++ {
			var mask uint = 1 << (5*y + x)
			count := bits.OnesCount(adjacencyMasks[5*y+x] & grid)
			hasBug := grid&mask != 0
			if hasBug {
				if count != 1 {
					// dead bug, we do nothing.
				} else {
					// bug lives
					nextGrid |= mask
				}
			} else {
				if count > 0 && count <= 2 {
					nextGrid |= mask
				}
			}
		}
	}
	return nextGrid
}

func biodiversityRating(grid uint) int {
	rating := 0
	for grid != 0 {
		idx := bits.TrailingZeros(grid)
		rating += 1 << idx
		grid ^= 1 << idx
	}
	return rating
}

func Run(f *os.File, partTwo bool) {
	// 0th most significant = 0, etc.
	// so it runs backwards sort of.
	grid := uint(0)
	adjacent := createAdjacencyMasks()

	scanner := bufio.NewScanner(f)
	row := 0
	for scanner.Scan() {
		line := scanner.Text()
		for n, ch := range line {
			if ch == '#' {
				grid |= 1 << (n + (row * 5))
			}
		}
		row++
	}

	seen := make(map[uint]bool)
	for {
		if seen[grid] {
			fmt.Println(gridToString(grid))
			fmt.Println(biodiversityRating(grid))
			break
		}
		seen[grid] = true
		grid = tick(grid, adjacent)
	}
}
