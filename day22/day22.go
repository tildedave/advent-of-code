package day22

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

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

func Run(f *os.File, partTwo bool) {
	numCards := 10_007 // conveniently this is prime.
	deck := newDeck(numCards)

	scanner := bufio.NewScanner(f)
	lines := make([]string, 0)

	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
	}

	result := processLines(lines, deck)

	for n, num := range result {
		if num == 2019 {
			fmt.Println(n)
			return
		}
	}
}
