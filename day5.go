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
		seedNum, err := strconv.ParseInt(str, 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		nums = append(nums, int(seedNum))
	}
	return nums
}

type rangeMapping = struct {
	destStart   int
	sourceStart int
	length      int
}

func day5(f *os.File) {
	scanner := bufio.NewScanner(f)
	var seedNums []int
	var currentMapSource string
	var currentMapDest string

	var mapRanges map[string]map[string][]rangeMapping = make(map[string]map[string][]struct {
		destStart   int
		sourceStart int
		length      int
	})

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "seeds: ") {
			seedNums = extractFields(strings.Split(line, "seeds: ")[1])
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
	for _, seedNum := range seedNums {
		// So now we go through this ridiculous mapping process
		// seems like it's just a straight shot through the map at least
		// actually seems like the map is just a straight shot.
		currentEntity := "seed"
		currentNum := seedNum
		for {
			var nextEntity string
			var nextNum int = -1
			for k := range mapRanges[currentEntity] {
				nextEntity = k
				break
			}
			// Now we map the current num to the next num.
			// We go through the listing for the current entity to see if any
			// match.
			rangeMappings := mapRanges[currentEntity][nextEntity]
			for _, rangeMapping := range rangeMappings {
				if currentNum >= rangeMapping.sourceStart && currentNum < rangeMapping.sourceStart+rangeMapping.length {
					offset := currentNum - rangeMapping.sourceStart
					nextNum = rangeMapping.destStart + offset
					break
				}
			}
			if nextNum == -1 {
				nextNum = currentNum
			}

			if currentEntity == "location" && nextEntity == "" {
				if nextNum < smallestLocationSeen {
					smallestLocationSeen = nextNum
				}
				break
			}
			// fmt.Printf("%s %d corresponds to %s %d\n", currentEntity, currentNum, nextEntity, nextNum)
			currentEntity = nextEntity
			currentNum = nextNum
		}
	}
	fmt.Println(smallestLocationSeen)
}
