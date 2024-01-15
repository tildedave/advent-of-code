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

func reverseDealWithIncrement(numCards, inc, pos int) int {
	// so we want to figure out the original position of this.
	// We need a closed form for this, we can't really run a for loop with 119
	// trillion as an upper bound.

	// Inc 3, Position 6 / 3 = original was 2.  That's easy at least.

	// 0	0
	// 3	1
	// 6	2
	// 9 	3
	// 2	4 --> 12 4   ---> (n cycle before * numCards) / inc - of course we need to be careful about integer overflow ;-(
	// 5	5 --> 15 4
	// 8	6 --> 18 6
	// 1	7 --> 21
	// 4	8 --> 24
	// 7	9 --> 27
	// So next question: how do you know # of cycles before you?

	// For 10 it's -p % inc
	// For 20 it's p % inc
	// so it has to do with the offset difference.
	// 0 3 6 9 12 15 18 1 4
	// 0 1 2 3 4  5  6  7 8

	// So this is right but it will overflow since we can't multiply by numCards.
	// OK, we can, the increments are all small enough, thank god.
	// return (utils.ModPositive(-pos, inc)*numCards + pos) / inc

	// interestingly we probably need a closed form.
	// pos = (x * inc) % numCards?
	// pos + numCards *n = (x * inc)
	// This is the same solution we discovered before.
	for pos%inc != 0 {
		pos += numCards
	}
	return pos / inc
}

func reverseCutCards(numCards, n, pos int) int {
	if n > 0 {
		return utils.ModPositive(pos-(numCards-n), numCards)
	}
	return utils.ModPositive(pos+(numCards+n), numCards)
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

	numCards := 119315717514047
	pos := 3
	for n := 0; n < 20; n++ {
		fmt.Println(n, pos)
		pos = processLinesReverse(lines, numCards, pos)
		if n >= 101741582076661-10 {
			fmt.Println(pos)
		}
	}
	fmt.Println(pos)

	// well, we obviously can't process 119,315,717,514,047 cards
	// 101741582076661 times in a row.
	// we probably can't even run something in a loop 101 trillion times.

	// we can trace the lines backwards.
	// we can confirm our approach via our answer for part 1
	// I suppose we can confirm our approach with all the examples too.
}
