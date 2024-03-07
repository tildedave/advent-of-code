package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

type hand = struct {
	entries   string
	bidAmount int
}

const FIVE_OF_A_KIND = 7
const FOUR_OF_A_KIND = 6
const FULL_HOUSE = 5
const THREE_OF_A_KIND = 4
const TWO_PAIR = 3
const ONE_PAIR = 2
const HIGH_CARD = 1

func getScore(c byte) int {
	switch c {
	case 'A':
		return 14
	case 'K':
		return 13
	case 'Q':
		return 12
	case 'T':
		return 10
	case '9':
		return 9
	case '8':
		return 8
	case '7':
		return 7
	case '6':
		return 6
	case '5':
		return 5
	case '4':
		return 4
	case '3':
		return 3
	case '2':
		return 2
	case 'J':
		return 1
	}
	log.Fatal("Invalid card passed")
	return -1
}

func getType(entries string) int {
	count := make(map[byte]int)
	for i := 0; i < len(entries); i++ {
		count[entries[i]] += 1
	}
	pairCount := 0
	hasThreeOfAKind := false
	hasFourOfAKind := false
	jokerCount := count['J']
	for k, v := range count {
		if k == 'J' {
			continue
		}
		if v == 5 {
			return FIVE_OF_A_KIND
		}
		if v == 4 {
			hasFourOfAKind = true
		}
		if v == 3 {
			hasThreeOfAKind = true
		}
		if v == 2 {
			pairCount += 1
		}
	}
	if hasFourOfAKind {
		if jokerCount == 0 {
			return FOUR_OF_A_KIND
		}
		if jokerCount == 1 {
			return FIVE_OF_A_KIND
		}
		log.Fatal("Impossible")
	}
	if hasThreeOfAKind {
		if jokerCount == 2 {
			return FIVE_OF_A_KIND
		}
		if jokerCount == 1 {
			return FOUR_OF_A_KIND
		}
		if jokerCount > 0 {
			log.Fatal("Impossible")
		}
		if pairCount == 1 {
			return FULL_HOUSE
		}
		return THREE_OF_A_KIND
	}
	if pairCount == 2 {
		if jokerCount == 1 {
			return FULL_HOUSE
		}
		if jokerCount > 1 {
			log.Fatal("Impossible")
		}
		return TWO_PAIR
	}
	if pairCount == 1 {
		if jokerCount == 1 {
			return THREE_OF_A_KIND
		}
		if jokerCount == 2 {
			return FOUR_OF_A_KIND
		}
		if jokerCount == 3 {
			return FIVE_OF_A_KIND
		}
		if jokerCount > 3 {
			log.Fatal("Impossible")
		}
		return ONE_PAIR
	}
	// No pairs
	if jokerCount == 5 {
		return FIVE_OF_A_KIND
	}
	if jokerCount == 1 {
		return ONE_PAIR
	}
	if jokerCount == 2 {
		return THREE_OF_A_KIND
	}
	if jokerCount == 3 {
		return FOUR_OF_A_KIND
	}
	if jokerCount == 4 {
		return FIVE_OF_A_KIND
	}
	return HIGH_CARD
}

func day7(f *os.File) {
	scanner := bufio.NewScanner(f)
	var hands []hand
	for scanner.Scan() {
		text := scanner.Text()
		res := strings.Split(text, " ")
		bidAmount, err := strconv.ParseInt(res[1], 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		h := hand{entries: res[0], bidAmount: int(bidAmount)}
		hands = append(hands, h)
	}
	sort.Slice(hands, func(i int, j int) bool {
		h1 := hands[i]
		h2 := hands[j]
		h1Type := getType(h1.entries)
		h2Type := getType(h2.entries)
		if h1Type != h2Type {
			return h1Type < h2Type
		}

		// type are equal, use tie-break
		for i := 0; i < len(h1.entries); i++ {
			h1Score := getScore(h1.entries[i])
			h2Score := getScore(h2.entries[i])
			if h1Score != h2Score {
				return h1Score < h2Score
			}
		}
		log.Fatal("Should not happen")
		return false
	})
	sum := 0
	for i, hand := range hands {
		sum += (i + 1) * hand.bidAmount
	}
	fmt.Println(sum)
}
