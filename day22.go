package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type brick = struct {
	ep1   []int
	ep2   []int
	axis  int
	label string
}

func isVertical(b brick) bool {
	return b.axis == 2
}

func isContained(b brick, x int, y int, z int) bool {
	// x,y,z is "inside" the brick
	// we want to iterate through "the coordinate that changes" in the brick,
	// and see if x/y/z are in there.
	start := b.ep1[b.axis]
	end := b.ep2[b.axis]
	for i := start; i <= end; i++ {
		coords := make([]int, 0)
		for j := 0; j < 3; j++ {
			if j == b.axis {
				coords = append(coords, i)
			} else {
				// Otherwise ep1[j] == ep2[j]
				if b.ep1[j] != b.ep2[j] {
					panic("Brick varied on non-axis coord")
				}
				coords = append(coords, b.ep1[j])
			}
		}
		if coords[0] == x && coords[1] == y && coords[2] == z {
			return true
		}
	}

	return false
}

func consistencyCheck(zMap map[int][]brick) {
	for k, bricks := range zMap {
		for _, b := range bricks {
			if !isContained(b, b.ep1[0], b.ep1[1], k) {
				panic("bad")
			}
			if !isContained(b, b.ep2[0], b.ep2[1], k) {
				panic("bad")
			}
		}
	}
}

func intersects(b1 brick, b2 brick) bool {
	start := b1.ep1[b1.axis]
	end := b1.ep2[b1.axis]
	for i := start; i <= end; i++ {
		coords := make([]int, 0)
		for j := 0; j < 3; j++ {
			if j == b1.axis {
				coords = append(coords, i)
			} else {
				// Otherwise ep1[j] == ep2[j]
				if b1.ep1[j] != b1.ep2[j] {
					panic("Brick varied on non-axis coord")
				}
				coords = append(coords, b1.ep1[j])
			}
		}
		if isContained(b2, coords[0], coords[1], coords[2]) {
			return true
		}
	}
	return false
}

func isHorizontal(b brick) bool {
	return !isVertical(b)
}

func shiftDownwards(b brick) brick {
	shiftedBrick := brick{}
	shiftedBrick.axis = b.axis
	shiftedBrick.ep1 = make([]int, 3)
	shiftedBrick.ep2 = make([]int, 3)
	shiftedBrick.label = fmt.Sprintf("%s-down", b.label)
	copy(shiftedBrick.ep1, b.ep1)
	copy(shiftedBrick.ep2, b.ep2)
	shiftedBrick.ep1[2] -= 1
	shiftedBrick.ep2[2] -= 1
	return shiftedBrick
}

func day22(f *os.File) {
	scanner := bufio.NewScanner(f)
	bricks := make([]brick, 0)
	zMap := make(map[int][]brick)
	brickNum := 0

	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, "~")
		ep1 := parseNumbers(strings.Split(res[0], ","))
		ep2 := parseNumbers(strings.Split(res[1], ","))
		// we want to put the bricks in the structure so that ep1 < ep2,
		// whichever coord is different.
		axis := -1
		for i := 0; i < 3; i++ {
			if ep1[i] != ep2[i] {
				axis = i
				if ep1[i] > ep2[i] {
					ep1, ep2 = ep2, ep1
				}
			}
		}
		if axis == -1 {
			axis = 2
		}

		// also let's remember which coord is different as that's less annoying
		// in the future.
		b := brick{ep1, ep2, axis, fmt.Sprintf("Brick%d", brickNum)}
		bricks = append(bricks, b)
		z1 := b.ep1[2]
		z2 := b.ep2[2]
		for i := z1; i <= z2; i++ {
			val, ok := zMap[i]
			if !ok {
				val = make([]brick, 0)
			}
			val = append(val, b)
			zMap[i] = val
		}
		brickNum++
	}

	// Basic idea is that when the bricks are suspended at a given point, we
	// can look at each one to see if it can fall, then continue until no
	// bricks have moved.
	// The "would fall" logic is the same check, but we need to understand for
	// each bricks which bricks are "connected" to it.  We only need to look
	// above the disintegrated brick.
	// So it seems like the main thing that's useful is a z map to which
	// bricks are at that location.  We of course also need x / y maps?

	for {
		consistencyCheck(zMap)

		hasSettled := true
		// for each brick, see if we can nudge it downward
		for _, b := range bricks {
			if b.ep1[2] == 1 {
				// Don't shift downwards from zero
				continue
			}
			shiftedBrick := shiftDownwards(b)

			z := shiftedBrick.ep1[2]
			bricksBelow := zMap[z]
			hasLowerBrick := false
			for _, belowBrick := range bricksBelow {
				if intersects(belowBrick, shiftedBrick) {
					hasLowerBrick = true
					break
				}
			}
			if !hasLowerBrick {
				// update our zMap
				// we should actually add from -1, remove from old plus.
				// other ones stay as-is.
				b.ep1[2] -= 1
				b.ep2[2] -= 1

				oldTopList := zMap[b.ep2[2]+1]
				newBottomMap := zMap[b.ep1[2]]

				for idx, topBrick := range oldTopList {
					if topBrick.label == b.label {
						oldTopList[idx] = oldTopList[len(oldTopList)-1]
						oldTopList = oldTopList[:len(oldTopList)-1]
						zMap[b.ep2[2]+1] = oldTopList
						break
					}
				}
				newBottomMap = append(newBottomMap, b)
				zMap[b.ep1[2]] = newBottomMap
				hasSettled = false
			}
		}
		if hasSettled {
			break
		}
	}

	// we've settled.  now we need to see, for each brick if it's
	// supporting the bricks above it.
	// I suppose the way to do it is to see, for each brick, if you shift it
	// downwards, which bricks it intersects.
	supportMap := make(map[string][]string)
	reverseSupportMap := make(map[string][]string)
	dotString := "digraph G {"

	for _, b := range bricks {
		downBrick := shiftDownwards(b)
		z := downBrick.ep1[2]
		bricksBelow := zMap[z]
		for _, belowBrick := range bricksBelow {
			if intersects(belowBrick, downBrick) {
				val, ok := supportMap[belowBrick.label]
				if !ok {
					val = []string{b.label}
				} else {
					val = append(val, b.label)
				}
				supportMap[belowBrick.label] = val
				dotString += fmt.Sprintf("%s -> %s;\n", belowBrick.label, b.label)
				val, ok = reverseSupportMap[b.label]
				if !ok {
					val = []string{belowBrick.label}
				} else {
					val = append(val, belowBrick.label)
				}
				reverseSupportMap[b.label] = val
			}
		}
	}
	dotString += "}"

	// For each item, it CAN be disintegrated if it:
	// it has NO bricks supporting it.
	// for ALL bricks supporting it, they have a DIFFERENT brick supporting them.
	disintegrationCount := 0
	for _, b := range bricks {
		_, ok := supportMap[b.label]
		// - supports nothing (G)
		if !ok {
			disintegrationCount++
			continue
		}

		// - supports something, but that something is supported by a different
		// label.
		canBeDisintegrated := true
		for _, v := range supportMap[b.label] {
			if len(reverseSupportMap[v]) == 1 {
				canBeDisintegrated = false
			}
		}
		if canBeDisintegrated {
			disintegrationCount++
		}
	}
	fmt.Println(disintegrationCount)
}
