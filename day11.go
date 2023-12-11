package main

import (
	"bufio"
	"fmt"
	"os"
)

type galaxyLocation = struct {
	x int
	y int
}

func formattedSpace(space [][]int) string {
	str := ""
	for i := 0; i < len(space); i++ {
		line := ""
		for j := 0; j < len(space[0]); j++ {
			if space[i][j] == 0 {
				line += "."
			} else {
				line += "#"
			}
		}
		str += line + "\n"
	}
	return str
}

func cosmicExpansion(space [][]int) [][]int {
	// expand columns first, then expand rows
	expandRows := make(map[int]bool)
	expandColumns := make(map[int]bool, 0)

	for i := 0; i < len(space); i++ {
		row := space[i]
		isEmpty := true
		for j := 0; j < len(row); j++ {
			if row[j] != 0 {
				isEmpty = false
				break
			}
		}
		if isEmpty {
			expandRows[i] = true
		}
	}
	// column expansion is a pain
	for j := 0; j < len(space[0]); j++ {
		isEmpty := true
		for i := 0; i < len(space); i++ {
			if space[i][j] != 0 {
				isEmpty = false
				break
			}
		}
		if isEmpty {
			expandColumns[j] = true
		}
	}

	newSpace := make([][]int, 0)
	newWidth := len(space[0]) + len(expandColumns)
	for i := 0; i < len(space); i++ {
		newRow := make([]int, newWidth)
		// j indexes into old space
		newJ := 0
		for j := 0; j < len(space[0]); j++ {
			newRow[newJ] = space[i][j]
			if expandColumns[j] {
				newJ++
			}
			newJ++
		}
		newSpace = append(newSpace, newRow)
		if expandRows[i] {
			newRowCopy := make([]int, newWidth)
			copy(newRowCopy, newRow)
			newSpace = append(newSpace, newRow)
		}
	}
	return newSpace
}

func getGalaxyLocations(space [][]int) []galaxyLocation {
	ret := make([]galaxyLocation, 0)
	for i := 0; i < len(space); i++ {
		for j := 0; j < len(space[0]); j++ {
			if space[i][j] != 0 {
				ret = append(ret, galaxyLocation{i, j})
			}
		}
	}
	return ret
}

func absInt(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

func taxiCab(l1 galaxyLocation, l2 galaxyLocation) int {
	return absInt(l1.x-l2.x) + absInt(l1.y-l2.y)
}

func day11(f *os.File) {
	scanner := bufio.NewScanner(f)
	space := make([][]int, 0)
	galaxyNum := 0
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		space = append(space, row)
		for i := 0; i < len(line); i++ {
			if line[i] == '#' {
				galaxyNum++
				row[i] = galaxyNum
			} else {
				row[i] = 0
			}
		}
	}
	expandedSpace := cosmicExpansion(space)
	locations := getGalaxyLocations(expandedSpace)
	// Galaxy n + 1 is at location n.
	// Since we're on a 2d grid, distances between two galaxies is just the
	// taxicab metric.
	totalDistance := 0
	for _, l1 := range locations {
		for _, l2 := range locations {
			totalDistance += taxiCab(l1, l2)
		}
	}
	fmt.Println(totalDistance / 2)
}
