package day22

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"

	"github.com/tildedave/advent-of-code-2019/utils"
)

// Part 1 functions

func newDeck(numCards int) []int {
	deck := make([]int, numCards)
	for n := 0; n < numCards; n++ {
		deck[n] = n
	}
	return deck
}

func dealNewStack(deck []int) []int {
	deckLength := len(deck)
	newDeck := make([]int, deckLength)
	for n := range deck {
		newDeck[deckLength-1-n] = deck[n]
	}

	return newDeck
}

func cutCards(deck []int, n int) []int {
	result := make([]int, len(deck))
	if n > 0 {
		copy(result, deck[n:])
		copy(result[len(deck)-n:], deck[0:n])
		return result
	}

	copy(result, deck[len(deck)+n:])
	copy(result[-n:], deck[:len(deck)+n])
	return result
}

func dealWithIncrement(deck []int, increment int) []int {
	deckLength := len(deck)
	newDeck := make([]int, deckLength)
	idx := 0
	for n := range deck {
		newDeck[idx] = deck[n]
		idx = (idx + increment) % deckLength
	}
	return newDeck
}

func processLines(lines []string, deck []int) []int {
	dealWithIncrementRe := regexp.MustCompile(`^deal with increment (\d+)$`)
	cutRe := regexp.MustCompile(`^cut (-?\d+)$`)

	lastLine := ""
	for _, line := range lines {
		if deck[0] == 0 && deck[1] == 0 {
			log.Fatalf("Invalid, last line was %s\n", lastLine)
		}
		lastLine = line

		if line == "deal into new stack" {
			deck = dealNewStack(deck)
			continue
		}

		m := dealWithIncrementRe.FindStringSubmatch(line)
		if m != nil {
			res, err := strconv.ParseInt(m[1], 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			if res == 0 {
				log.Fatalf("Invalid parsing: %s\n", m[1])
			}
			deck = dealWithIncrement(deck, int(res))
			continue
		}

		m = cutRe.FindStringSubmatch(line)
		if m != nil {
			res, err := strconv.ParseInt(m[1], 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			deck = cutCards(deck, int(res))
			continue
		}

		log.Fatalf("Invalid command: %s\n", line)
	}
	return deck
}

// Part 2 functions

func reverseDealNewStack(numCards, pos int) int {
	return numCards - 1 - pos
}

func reverseCutCards(numCards, n, pos int) int {
	return utils.ModPositive(pos+n, numCards)
}

func reverseDealWithIncrement(numCards, inc, pos int) int {
	return utils.ModMult(utils.ModInverse(inc, numCards), pos, numCards)
}

func processLinesReverse(lines []string, numCards, pos int) int {
	dealWithIncrementRe := regexp.MustCompile(`^deal with increment (\d+)$`)
	cutRe := regexp.MustCompile(`^cut (-?\d+)$`)

	for n := range lines {
		line := lines[len(lines)-1-n]

		if line == "deal into new stack" {
			pos = reverseDealNewStack(numCards, pos)
			continue
		}

		m := dealWithIncrementRe.FindStringSubmatch(line)
		if m != nil {
			res, err := strconv.ParseInt(m[1], 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			if res == 0 {
				log.Fatalf("Invalid parsing: %s\n", m[1])
			}
			pos = reverseDealWithIncrement(numCards, int(res), pos)
			continue
		}

		m = cutRe.FindStringSubmatch(line)
		if m != nil {
			res, err := strconv.ParseInt(m[1], 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			pos = reverseCutCards(numCards, int(res), pos)
			continue
		}

		log.Fatalf("Invalid command: %s\n", line)
	}
	return pos
}

func Run(f *os.File, partTwo bool) {

	scanner := bufio.NewScanner(f)
	lines := make([]string, 0)

	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
	}

	if !partTwo {
		numCards := 10_007 // conveniently this is prime.
		deck := newDeck(numCards)
		result := processLines(lines, deck)

		for n, num := range result {
			if num == 2019 {
				fmt.Println(n)
				return
			}
		}
		panic("Did not find 2019 in deck")
	}

	// This comes from a Reddit solution, I had most of the pieces, but I
	// wasn't sure if the equation was linear.
	numCards := 119315717514047
	x := 2020
	y := utils.ModPositive(processLinesReverse(lines, numCards, x), numCards)
	z := utils.ModPositive(processLinesReverse(lines, numCards, y), numCards)
	t := utils.ModPositive(processLinesReverse(lines, numCards, z), numCards)

	a := utils.ModMult(y-z, utils.ModInverse(x-y, numCards), numCards)
	b := y - utils.ModMult(a, x, numCards)

	fmt.Println(a, b)
	fmt.Println(x, y, z, t)
	fmt.Println("ax + b", (utils.ModMult(a, x, numCards)+b)%numCards)
	fmt.Println("ay + b", (utils.ModMult(a, y, numCards)+b)%numCards)
	fmt.Println("az + b", (utils.ModMult(a, z, numCards)+b)%numCards)

	compute := func(numIterations int) int {
		eqn := []int{
			utils.ModExp(a, numIterations, numCards) * x,
			utils.ModExp(a, numIterations, numCards) - 1,
			utils.ModMult(utils.ModInverse(a-1, numCards), b, numCards),
		}
		return ((eqn[0] + utils.ModMult(eqn[1], eqn[2], numCards)) % numCards)

	}
	// Confirm these are right
	if compute(0) != x {
		panic("did not compute n = 0 correctly")
	}
	if compute(1) != y {
		panic("did not compute n = 1 correctly")
	}
	if compute(2) != z {
		panic("did not compute n = 2 correctly")
	}
	// this is our answer
	fmt.Println(compute(101741582076661))

	// for n := 0; n < 20; n++ {
	// 	fmt.Println(n, pos)
	// 	pos = processLinesReverse(lines, numCards, pos)
	// 	if n >= 101741582076661-10 {
	// 		fmt.Println(pos)
	// 	}
	// }
	// fmt.Println(pos)

	// well, we obviously can't process 119,315,717,514,047 cards
	// 101741582076661 times in a row.
	// we probably can't even run something in a loop 101 trillion times.

	// we can trace the lines backwards.
	// we can confirm our approach via our answer for part 1
	// I suppose we can confirm our approach with all the examples too.
}
