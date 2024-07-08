package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func extractFields(s string) []int {
	var nums []int
	for _, str := range strings.Fields(s) {
		n, err := strconv.ParseInt(str, 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		nums = append(nums, int(n))
	}
	return nums
}

type rangeMapping = struct {
	destStart   int
	sourceStart int
	length      int
}

type entityRange = struct {
	start  int
	length int
}

func totalLength(rs []entityRange) int {
	// For sanity checking
	sum := 0
	for _, r := range rs {
		sum += r.length
	}
	return sum
}

func alignRanges(r rangeMapping, currentRange entityRange) ([]entityRange, []entityRange, bool) {
	mappingEnd := r.sourceStart + r.length
	currentEnd := currentRange.start + currentRange.length

	if currentRange.start >= r.sourceStart && currentEnd <= mappingEnd {
		// Option 1: currentRange contained in r.
		return []entityRange{currentRange}, []entityRange{}, true
	} else if currentRange.start < r.sourceStart && currentEnd <= mappingEnd && currentEnd > r.sourceStart {
		// Option 2: Split into 2 on the left
		leftLength := r.sourceStart - currentRange.start
		leftRange := entityRange{start: currentRange.start, length: leftLength}
		mappingRange := entityRange{start: r.sourceStart, length: currentRange.length - leftLength}

		return []entityRange{mappingRange}, []entityRange{leftRange}, true
	} else if currentRange.start > r.sourceStart && currentRange.start < mappingEnd && mappingEnd > currentEnd {
		// Option 3: Split into 2 on the right
		mappingLength := mappingEnd - currentRange.start
		mappingRange := entityRange{start: currentRange.start, length: mappingLength}
		rightRange := entityRange{start: r.sourceStart + r.length, length: currentRange.length - mappingLength}

		return []entityRange{mappingRange}, []entityRange{rightRange}, true
	} else if currentRange.start < r.sourceStart && currentEnd > mappingEnd {
		// Option 4: Split into 3
		leftRange := entityRange{start: currentRange.start, length: r.sourceStart - currentRange.start}
		mappingRange := entityRange{start: r.sourceStart, length: r.length}
		rightRange := entityRange{start: r.sourceStart + r.length, length: currentRange.length - mappingRange.length - leftRange.length}

		return []entityRange{mappingRange}, []entityRange{leftRange, rightRange}, true
	}

	return []entityRange{}, []entityRange{}, false
}

func day5(f *os.File) {
	scanner := bufio.NewScanner(f)
	var currentMapSource string
	var currentMapDest string

	var mapRanges map[string]map[string][]rangeMapping = make(map[string]map[string][]struct {
		destStart   int
		sourceStart int
		length      int
	})
	var entityRanges map[string][]entityRange = make(map[string][]struct {
		start  int
		length int
	})

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "seeds: ") {
			seedNums := extractFields(strings.Split(line, "seeds: ")[1])
			var seedRanges []entityRange
			for i := 0; i < len(seedNums); i += 2 {
				r := entityRange{start: seedNums[i], length: seedNums[i+1]}
				seedRanges = append(seedRanges, r)
			}
			entityRanges["seed"] = seedRanges
			continue
		}

		if strings.Contains(line, "map:") {
			typeParts := strings.Split(strings.Split(line, "map")[0], "-")
			currentMapSource = strings.TrimSpace(typeParts[0])
			currentMapDest = strings.TrimSpace(typeParts[2])
			if mapRanges[currentMapSource] == nil {
				mapRanges[currentMapSource] = make(map[string][]struct {
					destStart   int
					sourceStart int
					length      int
				})
			}
			if mapRanges[currentMapSource][currentMapDest] == nil {
				mapRanges[currentMapSource][currentMapDest] = make([]struct {
					destStart   int
					sourceStart int
					length      int
				}, 0)
			}
			continue
		}

		if len(line) == 0 {
			continue
		}

		nums := extractFields(line)
		r := rangeMapping{destStart: nums[0], sourceStart: nums[1], length: nums[2]}
		mapRanges[currentMapSource][currentMapDest] = append(mapRanges[currentMapSource][currentMapDest], r)
	}

	smallestLocationSeen := math.MaxInt
	for _, r := range entityRanges["seed"] {
		// So now we go through this ridiculous mapping process
		// seems like it's just a straight shot through the map at least
		// actually seems like the map is just a straight shot.
		currentEntity := "seed"
		// So we have a range, and we want to turn it into *more* ranges.
		// The puzzle input ranges are too big to realistically map number by number.
		currentEntityRanges := []entityRange{r}
		for {
			var nextEntity string
			// var nextEntityRanges []entityRange
			for k := range mapRanges[currentEntity] {
				nextEntity = k
				break
			}

			fmt.Println(currentEntity, currentEntityRanges)

			// alignment portion
			var alignedRanges []entityRange
			iterRanges := make([]entityRange, len(currentEntityRanges))
			copy(iterRanges, currentEntityRanges)

			for len(iterRanges) > 0 {
				rangeMappings := mapRanges[currentEntity][nextEntity]
				currentRange := iterRanges[0]
				iterRanges = iterRanges[1:]

				intersected := false
				for _, rangeMapping := range rangeMappings {
					ars, unalignedRanges, done := alignRanges(rangeMapping, currentRange)
					if !(len(ars) == 0 && len(unalignedRanges) == 0) {
						intersected = true
					}
					for _, ar := range ars {
						alignedRanges = append(alignedRanges, ar)
					}
					for _, r := range unalignedRanges {
						iterRanges = append(iterRanges, r)
					}
					if done {
						break
					}
				}
				// Totally disjoint
				if !intersected {
					alignedRanges = append(alignedRanges, currentRange)
				}
			}

			if totalLength(currentEntityRanges) != totalLength(alignedRanges) {
				panic("Split lengths did not match")
			}

			// next entity portion
			var nextRanges []entityRange
			for _, currentRange := range alignedRanges {
				rangeMappings := mapRanges[currentEntity][nextEntity]
				currentEnd := currentRange.start + currentRange.length

				wasMapped := false
				for _, rangeMapping := range rangeMappings {
					// either it is totally disjoint or totally contained
					mappingStart := rangeMapping.sourceStart
					mappingEnd := mappingStart + rangeMapping.length
					if currentRange.start >= mappingStart && currentEnd <= mappingEnd {
						// do the mapping
						offset := currentRange.start - rangeMapping.sourceStart
						mappedRange := entityRange{start: rangeMapping.destStart + offset, length: currentRange.length}
						nextRanges = append(nextRanges, mappedRange)
						wasMapped = true
						break
					}
				}
				if !wasMapped {
					nextRanges = append(nextRanges, currentRange)
				}
			}

			if currentEntity == "location" && nextEntity == "" {
				fmt.Println("final")
				// What's our smallest number?
				// It will be the smallest start of the remaining
				for _, r := range nextRanges {
					if r.start < smallestLocationSeen {
						smallestLocationSeen = r.start
					}
				}
				break
			}

			currentEntity = nextEntity
			currentEntityRanges = nextRanges
		}
	}
	fmt.Println(smallestLocationSeen)
}
