package day22

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestOperations(t *testing.T) {
	deck := newDeck(10)
	assert.Equal(t, []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, deck)
	assert.Equal(t, []int{9, 8, 7, 6, 5, 4, 3, 2, 1, 0}, dealNewStack(deck))
	assert.Equal(t, []int{0, 7, 4, 1, 8, 5, 2, 9, 6, 3}, dealWithIncrement(deck, 3))
	assert.Equal(t, []int{3, 4, 5, 6, 7, 8, 9, 0, 1, 2}, cutCards(deck, 3))
	assert.Equal(t, []int{6, 7, 8, 9, 0, 1, 2, 3, 4, 5}, cutCards(deck, -4))
}

func TestExamples(t *testing.T) {
	assert.Equal(t,
		[]int{0, 3, 6, 9, 2, 5, 8, 1, 4, 7},
		processLines([]string{
			"deal with increment 7",
			"deal into new stack",
			"deal into new stack",
		}, newDeck(10)))
	assert.Equal(t,
		[]int{3, 0, 7, 4, 1, 8, 5, 2, 9, 6},
		processLines([]string{
			"cut 6",
			"deal with increment 7",
			"deal into new stack",
		}, newDeck(10)))
	assert.Equal(t,
		[]int{6, 3, 0, 7, 4, 1, 8, 5, 2, 9},
		processLines([]string{
			"deal with increment 7",
			"deal with increment 9",
			"cut -2",
		}, newDeck(10)))
	assert.Equal(t,
		[]int{9, 2, 5, 8, 1, 4, 7, 0, 3, 6},
		processLines([]string{
			"deal into new stack",
			"cut -2",
			"deal with increment 7",
			"cut 8",
			"cut -4",
			"deal with increment 7",
			"cut 3",
			"deal with increment 9",
			"deal with increment 3",
			"cut -1",
		}, newDeck(10)))
}
