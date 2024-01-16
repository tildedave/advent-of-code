package day24

import (
	"bufio"
	"fmt"
	"math"
	"math/bits"
	"os"
)

func gridToString(grid uint, partTwo bool) string {
	ret := ""
	for y := 0; y < 5; y++ {
		if y > 0 {
			ret += "\n"
		}

		for x := 0; x < 5; x++ {
			if partTwo && y == 2 && x == 2 {
				ret += "?"
				continue
			}
			if grid&(1<<(x+(y*5))) != 0 {
				ret += "#"
			} else {
				ret += "."
			}
		}
	}
	return ret
}

func createSameLevelAdjacencyMasks(partTwo bool) []uint {
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
			if partTwo && mask&(1<<(5*2+2)) != 0 {
				// we don't want to consider this square.
				mask ^= 1 << (5*2 + 2)
			}
			masks[5*y+x] = mask
		}
	}
	return masks
}

func createOuterAdjacency() []uint {
	masks := make([]uint, 25)
	for y := 0; y < 5; y++ {
		for x := 0; x < 5; x++ {
			var mask uint
			// nicely enough these numbers are in the example :-)
			// un-nicely enough I need to subtract 1 from all of them.
			if y == 0 {
				mask |= 1 << 7
			}
			if y == 4 {
				mask |= 1 << 17
			}
			if x == 0 {
				mask |= 1 << 11
			}
			if x == 4 {
				mask |= 1 << 13
			}
			masks[5*y+x] = mask
		}
	}
	return masks
}

func createInnerAdjacency() []uint {
	masks := make([]uint, 25)
	masks[7] = 1 | 1<<1 | 1<<2 | 1<<3 | 1<<4
	masks[11] = 1 | 1<<5 | 1<<10 | 1<<15 | 1<<20
	masks[13] = 1<<4 | 1<<9 | 1<<14 | 1<<19 | 1<<24
	masks[17] = 1<<20 | 1<<21 | 1<<22 | 1<<23 | 1<<24

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

func getGridLevelBounds(grids map[int]uint) (int, int) {
	maxLevel := math.MinInt
	minLevel := math.MaxInt

	for l, grid := range grids {
		if l > maxLevel && grid != 0 {
			maxLevel = l
		}
		if l < minLevel && grid != 0 {
			minLevel = l
		}
	}
	return minLevel, maxLevel
}

func tickRecursive(
	grids map[int]uint,
	sameLevelMasks []uint,
	outerMasks []uint,
	innerMasks []uint,
) map[int]uint {
	nextGrids := make(map[int]uint)
	minLevel, maxLevel := getGridLevelBounds(grids)

	for l := minLevel - 1; l <= maxLevel+1; l++ {
		nextGrids[l] = 0

		for y := 0; y < 5; y++ {
			for x := 0; x < 5; x++ {
				if x == 2 && y == 2 {
					// don't process this one.
					continue
				}

				idx := 5*y + x
				var mask uint = 1 << idx

				sameAdjacency := sameLevelMasks[idx] & grids[l]
				outerAdjacency := outerMasks[idx] & grids[l-1]
				innerAdjacency := innerMasks[idx] & grids[l+1]
				count := 0
				count += bits.OnesCount(sameAdjacency)
				count += bits.OnesCount(outerAdjacency)
				count += bits.OnesCount(innerAdjacency)

				hasBug := grids[l]&mask != 0
				if hasBug {
					if count != 1 {
						// dead bug, we do nothing.
					} else {
						// bug lives
						nextGrids[l] |= mask
					}
				} else {
					if count > 0 && count <= 2 {
						nextGrids[l] |= mask
					}
				}
			}
		}
	}
	// do above + below levels as they start growing now.

	return nextGrids
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

func countBugs(grids map[int]uint) int {
	bugs := 0
	for _, grid := range grids {
		bugs += bits.OnesCount(grid)
	}
	return bugs
}

func Run(f *os.File, partTwo bool) {
	// 0th most significant = 0, etc.
	// so it runs backwards sort of.
	grid := uint(0)
	sameLevelMasks := createSameLevelAdjacencyMasks(partTwo)

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

	if !partTwo {
		seen := make(map[uint]bool)
		minutes := 0
		for {
			if seen[grid] {
				fmt.Println(gridToString(grid, partTwo))
				fmt.Println(biodiversityRating(grid))
				fmt.Println("cycle after", minutes)
				return
			}
			seen[grid] = true
			grid = tick(grid, sameLevelMasks)
			minutes++
		}
	}

	grids := make(map[int]uint)
	grids[0] = grid

	outerMasks := createOuterAdjacency()
	innerMasks := createInnerAdjacency()

	minutes := 0
	for minutes < 200 {
		grids = tickRecursive(grids, sameLevelMasks, outerMasks, innerMasks)
		minutes++
	}
	numBugs := countBugs(grids)
	fmt.Println(numBugs)
	// fmt.Println(grids)
	// minLevel, maxLevel := getGridLevelBounds(grids)
	// for l := minLevel; l <= maxLevel; l++ {
	// 	fmt.Println(l)
	// 	fmt.Println(gridToString(grids[l], partTwo))
	// }
	// fmt.Println(gridToString(nextGrids[0], partTwo))
}
