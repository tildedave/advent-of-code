package day20

import (
	"bufio"
	"fmt"
	"os"

	"github.com/tildedave/advent-of-code-2019/utils"
)

type coord struct {
	x int
	y int
}

func isValid(grid [][]byte, x, y int) bool {
	return y >= 0 && y < len(grid) && x >= 0 && x < len(grid[y])
}

func isOuterPortal(rows, columns int, location coord) bool {
	return location.x == 2 || location.y == 2 || location.x == columns-3 || location.y == rows-3
}

func Run(f *os.File, partTwo bool) {
	scanner := bufio.NewScanner(f)
	rows := 0
	grid := make([][]byte, 0)
	for scanner.Scan() {
		line := scanner.Text()
		strLine := make([]byte, len(line))
		copy(strLine, line)
		grid = append(grid, strLine)
		rows++
	}

	portals := make(map[string][]coord)
	portalCoords := make(map[coord]string)

	for y := range grid {
		for x := range grid[y] {
			if utils.IsUppercase(grid[y][x]) {
				// find the other portal piece
				label := string(grid[y][x])
				foundSecondPiece := false
				var dest coord

				for _, dx := range []int{1} {
					if isValid(grid, x+dx, y) && utils.IsUppercase(grid[y][x+dx]) {
						// this is the other piece of the coord
						label += string(grid[y][x+dx])
						if isValid(grid, x+dx+dx, y) && grid[y][x+dx+dx] == '.' {
							dest = coord{x + dx + dx, y}
							foundSecondPiece = true
						} else if isValid(grid, x-dx, y) && grid[y][x-dx] == '.' {
							dest = coord{x - dx, y}
							foundSecondPiece = true
						}
					}
				}
				for _, dy := range []int{1} {
					if isValid(grid, x, y+dy) && utils.IsUppercase(grid[y+dy][x]) {
						// this is the other piece of the coord
						label += string(grid[y+dy][x])
						if isValid(grid, x, y+dy+dy) && grid[y+dy+dy][x] == '.' {
							dest = coord{x, y + dy + dy}
							foundSecondPiece = true
						} else if isValid(grid, x, y-dy) && grid[y-dy][x] == '.' {
							dest = coord{x, y - dy}
							foundSecondPiece = true
						}
					}
				}
				if foundSecondPiece {
					if portals[label] == nil {
						portals[label] = []coord{dest}
					} else {
						portals[label] = append(portals[label], dest)
					}
					portalCoords[dest] = label
				}
			}
		}
	}
	columns := 0
	for _, line := range grid {
		columns = utils.MaxInt(columns, len(line))
	}

	// now DFS

	maxDepth := 50
	type queueItem struct {
		location coord
		steps    int
		level    int
	}
	queue := make([]queueItem, 0)
	minDistance := make(map[int]map[coord]int)
	queue = append(queue, queueItem{portals["AA"][0], 0, 0})
	for len(queue) > 0 {
		curr := queue[0]
		queue = queue[1:]
		if minDistance[curr.level] == nil {
			minDistance[curr.level] = make(map[coord]int)
		}
		dist, ok := minDistance[curr.level][curr.location]
		if !ok || curr.steps < dist {
			minDistance[curr.level][curr.location] = curr.steps
		} else {
			continue
		}

		if curr.location == portals["ZZ"][0] && curr.level == 0 {
			// we're done
			fmt.Println("found minimum distance to ZZ", curr.steps)
			break
		}

		// now look for adjacent nodes.
		x, y := curr.location.x, curr.location.y
		for _, walk := range [][2]int{{-1, 0}, {1, 0}, {0, -1}, {0, 1}} {
			dx, dy := walk[0], walk[1]
			if isValid(grid, x+dx, y+dy) {
				ch := grid[y+dy][x+dx]

				var next coord = coord{x + dx, y + dy}
				isPortalTransit := false
				if ch == '#' {
					// can't step here
					continue
				} else if utils.IsUppercase(ch) {
					// portal logic
					portal := portalCoords[curr.location]
					if portal == "AA" {
						// nothing
						continue
					} else if portal == "ZZ" {
						if !partTwo {
							// would have been caught in the main loop
							panic("Should have bailed out before this")
						} else {
							// can't leave on lower depths
							continue
						}
					} else {
						if len(portals[portal]) != 2 {
							panic("Did not have matching location")
						}
						foundOtherLocation := false
						for _, coord := range portals[portal] {
							if coord != curr.location {
								foundOtherLocation = true
								next = coord
								isPortalTransit = true
								break
							}
						}
						if !foundOtherLocation {
							panic("Did not find other location")
						}
					}
				} else if ch != '.' {
					panic("Some other case I didn't handle")
				}

				var level int
				if isPortalTransit && partTwo {
					if isOuterPortal(rows, columns, curr.location) {
						if curr.level == 0 {
							continue
						} else {
							level = curr.level - 1
						}
					} else {
						// we go deeper.
						level = curr.level + 1
					}
				} else {
					level = curr.level
				}
				// determine if we should visit
				if minDistance[level] == nil {
					minDistance[level] = make(map[coord]int)
				}
				dist, ok := minDistance[level][next]
				if ok && dist < curr.steps+1 {
					// cutoff, don't do anything.
					// TBD if we need this both in the main loop and in the
					// branch logic.
					continue
				}

				if level > maxDepth {
					continue
				}

				nextItem := queueItem{next, curr.steps + 1, level}
				queue = append(queue, nextItem)
			}
		}
	}
}
