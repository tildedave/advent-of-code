package day19

import (
	"fmt"
	"math"
	"os"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func testSquare(program []int, x int, y int) int {
	var wg sync.WaitGroup
	input := make(chan int)
	output := make(chan int)

	wg.Add(1)
	go func() {
		intcode.ExecFull(program, input, output)
		defer wg.Done()
	}()
	input <- x
	input <- y
	defer wg.Wait()
	return <-output
}

func runPartOne(program []int) {
	numOnes := 0
	for y := 0; y < 50; y++ {
		for x := 0; x < 50; x++ {
			c := testSquare(program, x, y)
			fmt.Print(c)
			if c == 1 {
				numOnes++
			}
		}
		fmt.Println()
	}
	fmt.Println(numOnes)
}

func countOnes(
	program []int,
	startX, endX, y int,
) [2]int {
	var returnStartX int
	var returnEndX int
	for x := startX; x <= endX; x++ {
		c := testSquare(program, x, y)
		if returnStartX == 0 && c == 1 {
			returnStartX = x
		}
		if returnEndX == 0 && returnStartX != 0 && c == 0 {
			returnEndX = x
		}
	}
	if returnEndX == 0 {
		returnEndX = endX
	}
	fmt.Println("passed in", startX, endX, "returning", returnStartX, returnEndX)
	return [2]int{returnStartX, returnEndX}
}

func getOffsets(
	program []int,
	firstAngle, secondAngle float64,
	desiredDistance int,
) [3]int {
	triangleAngle := secondAngle - firstAngle
	ratio := float64(desiredDistance) / math.Sin(triangleAngle)
	triangle1hyp := ratio * math.Sin(math.Pi/2-secondAngle)

	guessX := math.Sin(firstAngle) * triangle1hyp
	guessY := math.Cos(firstAngle) * triangle1hyp

	fmt.Println(guessX, guessY, triangle1hyp, math.Sqrt(guessX*guessX+guessY*guessY))
	// so this can tell us when we have a row of length N, for whatever N.
	// we're looking for the "first" one, which could be wrong I suppose.
	// the distances are not consistent at lower numbers.
	// the guess of Y will need to be confirmed too.
	fudge := 4
	var foundSmaller = false
	for dy := -fudge; dy < fudge; dy++ {
		var y int = int(guessY) + dy
		offsets := countOnes(program, int(guessX)-fudge, int(guessX)+desiredDistance+fudge, y)
		numOnes := offsets[1] - offsets[0]
		if numOnes == desiredDistance && foundSmaller {
			return [3]int{offsets[0], offsets[1], y}
		}
		if numOnes < desiredDistance {
			foundSmaller = true
		}
	}
	panic("Did not find")

}

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)

	if !partTwo {
		runPartOne(program)
		return
	}

	// we can approximate the length of the tractor beam with some guesses
	// and, we can approximate the start of the tractor beam with some guesses
	// then we can run the program within the square to confirm.

	// seems like 250 gives a decent approximation.
	length := make([][2]int, 250)
	var lastStart int
	for y := 0; y < len(length); y++ {
		hasStart := false
		for x := lastStart; x <= y; x++ { // my angle is < 45 degrees so this should be safe.
			result := testSquare(program, x, y)
			if result == 1 && !hasStart {
				hasStart = true
				lastStart = x
			}
			if result == 0 && hasStart {
				length[y] = [2]int{lastStart, x}
				break
			}
		}
	}
	runPartOne(program)

	// calculate the angle for the last start and the last end.
	// then we can calculate the size of the triangle based on that to find the
	// first x/y with.
	lastMeasurement := length[len(length)-1]
	startX, endX := lastMeasurement[0], lastMeasurement[1]
	y := len(length) - 1
	firstAngle := math.Atan2(float64(startX), float64(y))
	secondAngle := math.Atan2(float64(endX), float64(y))
	fmt.Println(firstAngle, secondAngle)

	// we have a triangle like
	// |\
	// | \
	// |  \
	// |___\

	// we need to find a square so that the END of the offset - 100 is the
	// START of the offset 100 down.
	squareLength := 100
	// guessDistance is the length of the bottom triangle
	// that we think the n x n square might show up
	guessDistance := 140
	// this is the first row of length guessDistance
	topOffset := getOffsets(program, firstAngle, secondAngle, guessDistance)
	fmt.Println("topOffset", topOffset)
	y = topOffset[2]
	for {
		// this is where the ones start and top at the top (y guess)
		top := countOnes(
			program,
			0, y, y)
		// this is where the ones start and stop at the bottom (y + squareLength guess)
		bottom := countOnes(
			program,
			0, y, y+squareLength-1)

		if bottom[0] == top[1]-squareLength {
			numOnes := 0
			for x := bottom[0]; x < bottom[0]+squareLength; x++ {
				for ny := y; ny < y+squareLength; ny++ {
					numOnes += testSquare(program, x, y)
				}
			}
			fmt.Println("numOnes", numOnes)
			fmt.Println("we have it!", guessDistance, bottom[0], y, bottom[0]*10_000+y)
			break
		} else {
			fmt.Printf("y=%d, 1s started at %d vs %d\n", y, bottom[0], top[1]-squareLength)
			fmt.Println(countOnes(program, 0, 100, y))
		}
		y++
	}
}
