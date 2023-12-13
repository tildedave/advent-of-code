package main

import (
	"bufio"
	"fmt"
	"math/bits"
	"os"
	"strings"
)

func horizontalSummary(grid [][]int) []int {
	ret := make([]int, 0)
	for i := 0; i < len(grid); i++ {
		val := 0
		for j := 0; j < len(grid[0]); j++ {
			val = (val << 1) | grid[i][j]
		}
		ret = append(ret, val)
	}
	return ret
}

func verticalSummary(grid [][]int) []int {
	ret := make([]int, 0)
	for j := 0; j < len(grid[0]); j++ {
		val := 0
		for i := 0; i < len(grid); i++ {
			val = (val << 1) | grid[i][j]
		}
		ret = append(ret, val)
	}
	return ret
}

func reverse(l []int) {
	for i, j := 0, len(l)-1; i < j; i, j = i+1, j-1 {
		l[i], l[j] = l[j], l[i]
	}
}

func isSmudged(x int, y int) bool {
	return bits.OnesCount(uint(x^y)) == 1
}

func hasSmudgedReflection(summary []int) int {
	// We have to find any palindromic subsequence such that one of the ends
	// of it is the end of the array.
	for i := 0; i < len(summary); i++ {
		numSmudges := 0
		if i+1 < len(summary) && (summary[i] == summary[i+1] || isSmudged(summary[i], summary[i+1])) {
			// starting point
			h1, h2 := i, i+1
			foundInvalid := false
			// i, j is a potential reflection.  let's now check outwards
			// from it.
			for h1 >= 0 && h2 < len(summary) {
				smudgedEquals := isSmudged(summary[h1], summary[h2])
				if summary[h1] != summary[h2] && !smudgedEquals {
					foundInvalid = true
					break
				}
				if smudgedEquals {
					numSmudges += 1
					if numSmudges > 1 {
						foundInvalid = true
						break
					}
				}
				h1 -= 1
				h2 += 1
			}
			if !foundInvalid && numSmudges == 1 {
				return i + 1
			}
		}
	}
	return -1
}

func day13(f *os.File) {
	scanner := bufio.NewScanner(f)
	grids := make([][][]int, 0)
	currentGrid := make([][]int, 0)

	for scanner.Scan() {
		line := scanner.Text()
		if len(strings.TrimSpace(line)) == 0 {
			grids = append(grids, currentGrid)
			currentGrid = make([][]int, 0)
			continue
		}
		row := make([]int, 0)
		for _, c := range line {
			if c == '#' {
				row = append(row, 1)
			} else if c == '.' {
				row = append(row, 0)
			} else {
				panic("Invalid character")
			}
		}
		currentGrid = append(currentGrid, row)
	}
	grids = append(grids, currentGrid)

	total := 0
	for _, grid := range grids {
		h := hasSmudgedReflection(horizontalSummary(grid))
		v := hasSmudgedReflection(verticalSummary(grid))
		if h != -1 {
			total += h * 100
		}
		if v != -1 {
			total += v
		}
	}
	fmt.Println(total)
}
