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

func cosmicExpansion(space [][]int) (map[int]bool, map[int]bool) {
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

	return expandRows, expandColumns
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

func galaxyDistance(l1 galaxyLocation, l2 galaxyLocation, expandedRows map[int]bool, expandedColumns map[int]bool, expansionFactor int) int {
	distance := taxicab(l1, l2)
	if l1.x != l2.x {
		var startX int
		var endX int
		if l1.x < l2.x {
			startX = l1.x
			endX = l2.x
		} else {
			startX = l2.x
			endX = l1.x
		}
		for x := startX; x < endX; x++ {
			if expandedRows[x] {
				distance += expansionFactor
			}
		}
	}
	if l1.y != l2.y {
		var startY int
		var endY int
		if l1.y < l2.y {
			startY = l1.y
			endY = l2.y
		} else {
			startY = l2.y
			endY = l1.y
		}
		for y := startY; y < endY; y++ {
			if expandedColumns[y] {
				distance += expansionFactor
			}
		}
	}
	return distance
}

func taxicab(l1 galaxyLocation, l2 galaxyLocation) int {
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
	expandedRows, expandedColumns := cosmicExpansion(space)
	locations := getGalaxyLocations(space)
	expansionFactor := 1_000_000 - 1
	// Galaxy n + 1 is at location n.
	// Since we're on a 2d grid, distances between two galaxies is just the
	// taxicab metric.
	totalDistance := 0
	for _, l1 := range locations {
		for _, l2 := range locations {
			dist := galaxyDistance(l1, l2, expandedRows, expandedColumns, expansionFactor)
			totalDistance += dist
		}
	}
	fmt.Println(totalDistance / 2)
}
