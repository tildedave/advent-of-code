package day3

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

type edge = struct {
	startX int
	startY int

	endX int
	endY int
}

func isHorizontal(e edge) bool {
	return e.startY == e.endY
}

func isVertical(e edge) bool {
	return e.startX == e.endX
}

func Crosses(e1 edge, e2 edge) (int, int, bool) {
	// wires intersect if they're a common point between them.
	// a wire is either horizontal or vertical.
	// for now we'll assume that if they're both horizontal/vertical that they
	// don't cross.  probably wrong in the grand scheme of things
	if isHorizontal(e1) && isHorizontal(e2) {
		return 0, 0, false
	}
	if isVertical(e1) && isVertical(e2) {
		return 0, 0, false
	}

	if isVertical(e1) && isHorizontal(e2) {
		e1, e2 = e2, e1
	}

	// e1 is horizontal and e2 is vertical.
	if e1.startY != e1.endY {
		panic("invalid")
	}
	if e2.startX != e2.endX {
		panic("invalid")
	}

	// if there is a cross, it will be at e2.startX, e1.startY.  we need this
	// to be on both wires.  since we don't store startX and endX sorted this
	// gets annoying.

	var crosses bool = true
	if e2.startY < e2.endY {
		crosses = crosses && (e2.startY < e1.startY && e1.startY < e2.endY)
	} else {
		crosses = crosses && (e2.endY < e1.startY && e1.startY < e2.startY)
	}
	if e1.startX < e1.endX {
		crosses = crosses && (e1.startX < e2.startX && e2.startX < e1.endX)
	} else {
		crosses = crosses && (e1.endX < e2.startX && e2.startX < e1.startX)
	}

	if !crosses {
		return 0, 0, false
	}

	return e2.startX, e1.startY, true
}

func Run(f *os.File) {
	scanner := bufio.NewScanner(f)
	// two lines.  we start from 0, 0 for both.
	rows := 0
	var edges1 []edge
	var edges2 []edge

	for scanner.Scan() {
		if rows > 1 {
			panic("Only should have had two lines of input")
		}
		e := make([]edge, 0)

		line := scanner.Text()
		res := strings.Split(line, ",")
		var startX int = 0
		var endX int = 0
		var startY int = 0
		var endY int = 0
		for _, r := range res {
			n, err := strconv.ParseInt(r[1:], 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			switch r[0] {
			case 'D':
				endX = startX
				endY = startY + int(n)
			case 'U':
				endX = startX
				endY = startY - int(n)
			case 'R':
				endX = startX + int(n)
				endY = startY
			case 'L':
				endX = startX - int(n)
				endY = startY
			default:
				panic("Invalid direction")
			}
			e = append(e, edge{startX, startY, endX, endY})
			startX = endX
			startY = endY
		}
		if rows == 0 {
			edges1 = e
		} else if rows == 1 {
			edges2 = e
		}
		rows++
	}

	minDistance := math.MaxInt
	for _, e1 := range edges1 {
		for _, e2 := range edges2 {
			crossX, crossY, c := Crosses(e1, e2)
			if c {
				if crossX < 0 {
					crossX = -crossX
				}
				if crossY < 0 {
					crossY = -crossY
				}
				if crossX+crossY < minDistance {
					minDistance = crossX + crossY
				}
			}
		}
	}
	fmt.Println(minDistance)
}
