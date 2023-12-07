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
	case 'J':
		return 11
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
	for _, v := range count {
		if v == 5 {
			return FIVE_OF_A_KIND
		}
		if v == 4 {
			return FOUR_OF_A_KIND
		}
		if v == 3 {
			hasThreeOfAKind = true
		}
		if v == 2 {
			pairCount += 1
		}
	}
	if pairCount == 1 && hasThreeOfAKind {
		return FULL_HOUSE
	}
	if hasThreeOfAKind {
		return THREE_OF_A_KIND
	}
	if pairCount == 2 {
		return TWO_PAIR
	}
	if pairCount == 1 {
		return ONE_PAIR
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

		// type are equal, figure out later
		for i := 0; i < len(h1.entries); i++ {
			h1Score := getScore(h1.entries[i])
			h2Score := getScore(h2.entries[i])
			if h1Score != h2Score {
				return h1Score < h2Score
			}
		}
		log.Fatal("Unable to break tie between %s and %s", h1.entries, h2.entries)
		return false
	})
	sum := 0
	for i, hand := range hands {
		sum += (i + 1) * hand.bidAmount
	}
	fmt.Println(sum)
}
