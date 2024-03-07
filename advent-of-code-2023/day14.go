package main

import (
	"bufio"
	"fmt"
	"hash/fnv"
	"math/bits"
	"os"
)

type Summary = struct {
	numOnes     int
	idx         int
	endIdx      int
	rowOrColumn int
	isSentinel  bool
}

func verticalSummaryDay14(grid [][]byte) []Summary {
	results := []Summary{}
	for j := 0; j < len(grid[0]); j++ {
		curr := uint(0)
		endIdx := len(grid)
		for i := len(grid) - 1; i >= 0; i-- {
			c := grid[i][j]
			// fmt.Println(string(c))
			if c == '#' {
				// break, push to next
				if curr != 0 {
					results = append(results, Summary{bits.OnesCount(curr), i + 1, endIdx, j, false})
				}
				results = append(results, Summary{0, i, i + 1, j, true})
				endIdx = i
				curr = 0
			} else if c == 'O' {
				curr = (curr << 1) | 1
			} else if c == '.' {
				curr = curr << 1
			}
		}
		if curr != 0 {
			results = append(results, Summary{bits.OnesCount(curr), 0, endIdx, j, false})
		}
	}
	return results
}

func horizontalSummaryDay14(grid [][]byte) []Summary {
	results := []Summary{}
	for i := 0; i < len(grid); i++ {
		curr := uint(0)
		startIdx := 0
		for j := 0; j < len(grid[0]); j++ {
			c := grid[i][j]
			// fmt.Println(string(c))
			if c == '#' {
				// break, push to next
				if curr != 0 {
					results = append(results, Summary{bits.OnesCount(curr), startIdx, j, i, false})
				}
				results = append(results, Summary{0, j, j + 1, i, true})
				startIdx = j + 1
				curr = 0
			} else if c == 'O' {
				curr = (curr << 1) | 1
			} else if c == '.' {
				curr = curr << 1
			}
		}
		if curr != 0 {
			results = append(results, Summary{bits.OnesCount(curr), startIdx, len(grid), i, false})
		}
	}
	return results
}

func formatGrid(grid [][]byte) string {
	ret := ""
	for i := 0; i < len(grid); i++ {
		row := ""
		for j := 0; j < len(grid[0]); j++ {
			row += string(grid[i][j])
		}
		ret += row + "\n"
	}
	return ret
}

const W_TOP = 0
const W_BOTTOM = 1
const W_LEFT = 2
const W_RIGHT = 3

func summaryToGrid(summary []Summary, numRows int, numCols int, direction int) [][]byte {
	grid := [][]byte{}
	for i := 0; i < numRows; i++ {
		row := make([]byte, numCols)
		for j := 0; j < numCols; j++ {
			row[j] = '.'
		}
		grid = append(grid, row)
	}
	for _, s := range summary {
		numOnes := s.numOnes
		if s.isSentinel {
			if direction == W_TOP || direction == W_BOTTOM {
				// in this case it's a column.
				grid[s.idx][s.rowOrColumn] = '#'
				continue
			} else if direction == W_LEFT || direction == W_RIGHT {
				// in this case it's a row.
				grid[s.rowOrColumn][s.idx] = '#'
				continue
			}
		}
		if direction == W_TOP {
			for i := s.idx; i < len(grid) && numOnes > 0; i++ {
				grid[i][s.rowOrColumn] = 'O'
				numOnes--
			}
		} else if direction == W_BOTTOM {
			for i := s.endIdx - 1; i >= 0 && numOnes > 0; i-- {
				grid[i][s.rowOrColumn] = 'O'
				numOnes--
			}
		} else if direction == W_LEFT {
			for j := s.idx; j < len(grid[0]) && numOnes > 0; j++ {
				grid[s.rowOrColumn][j] = 'O'
				numOnes--
			}
		} else if direction == W_RIGHT {
			for j := s.endIdx - 1; j >= 0 && numOnes > 0; j-- {
				grid[s.rowOrColumn][j] = 'O'
				numOnes--
			}
		} else {
			panic("Invalid direction")
		}
	}
	return grid
}

func hash(s string) uint64 {
	h := fnv.New64a()
	h.Write([]byte(s))
	return h.Sum64()
}

func load(grid [][]byte) int {
	score := 0
	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[0]); j++ {
			if grid[i][j] == 'O' {
				score += len(grid) - i
			}
		}
	}
	return score
	// results := verticalSummaryDay14(grid)
	// for _, r := range results {
	// 	if r.isSentinel {
	// 		continue
	// 	}
	// 	adder := len(grid) - r.idx
	// 	for s := 0; s < r.numOnes; s++ {
	// 		score += adder
	// 		adder -= 1
	// 	}
	// }
	// return score
}

func day14(f *os.File) {
	scanner := bufio.NewScanner(f)
	grid := [][]byte{}
	i := 0
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]byte, len(line))
		for j, c := range line {
			row[j] = byte(c)
		}
		grid = append(grid, row)
		i++
	}
	grid = summaryToGrid(verticalSummaryDay14(grid), len(grid), len(grid[0]), W_TOP)
	fmt.Println(load(grid))

	// fmt.Println(formatGrid(grid))
	// go upwards and split by '#'s
	// 0 = empty, 1 = O
	// results := verticalSummaryDay14(grid)
	cycles := 0
	var loadMap map[uint64][]int = make(map[uint64][]int)
	loads := make([]int, 0)
	total := 1_000_000_000
	for cycles < 1000 { // detect the cycle
		grid = summaryToGrid(verticalSummaryDay14(grid), len(grid), len(grid[0]), W_TOP)
		grid = summaryToGrid(horizontalSummaryDay14(grid), len(grid), len(grid[0]), W_LEFT)
		grid = summaryToGrid(verticalSummaryDay14(grid), len(grid), len(grid[0]), W_BOTTOM)
		grid = summaryToGrid(horizontalSummaryDay14(grid), len(grid), len(grid[0]), W_RIGHT)
		h := hash(formatGrid(grid))
		if loadMap[h] != nil {
			// detected cycle so we're done with the computation, for now.
			startLoop := loadMap[h][0]
			loopLength := cycles - startLoop
			// Make it zero-offset
			idx := (total - startLoop) % loopLength
			// Not sure why I have to subtract 1 from the index but it works.
			fmt.Println(loads[idx+startLoop-1])
			return
		}
		l := load(grid)
		loadMap[h] = []int{cycles, l}
		loads = append(loads, l)
		fmt.Println(h, cycles)
		cycles++
	}
}
