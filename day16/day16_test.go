package day16

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestRunPattern(t *testing.T) {
	assert.Equal(t, 4, runPattern(0, []int{0, 1, 0, -1}, []int{1, 2, 3, 4, 5, 6, 7, 8}))
	assert.Equal(t, 8, runPattern(1, []int{0, 1, 0, -1}, []int{1, 2, 3, 4, 5, 6, 7, 8}))
	assert.Equal(t, 2, runPattern(2, []int{0, 1, 0, -1}, []int{1, 2, 3, 4, 5, 6, 7, 8}))
}

func TestRunPhase(t *testing.T) {
	assert.Equal(
		t,
		[]int{4, 8, 2, 2, 6, 1, 5, 8},
		RunPhase([]int{1, 2, 3, 4, 5, 6, 7, 8}, []int{0, 1, 0, -1}),
	)
}

// func TestRunPhaseTest(t *testing.T) {
// fmt.Println(RunPhase([]int{1, 2, 3, 4, 5, 6, 7, 8}, []int{0, 1, 0, -1}))
// fmt.Println(RunPhase([]int{1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8}, []int{0, 1, 0, -1}))
// assert.Equal(
// 	t,
// 	[]int{4, 8, 2, 2, 6, 1, 5, 8},
// 	RunPhase([]int{1, 2, 3, 4, 5, 6, 7, 8}, []int{0, 1, 0, -1}),
// )
// }

func TestRunPhaseMultiple(t *testing.T) {
	assert.Equal(
		t,
		[]int{0, 1, 0, 2, 9, 4, 9, 8},
		RunPhaseMultiple([]int{1, 2, 3, 4, 5, 6, 7, 8}, []int{0, 1, 0, -1}, 4),
	)
}

func TestRunPhaseMultipleExamples(t *testing.T) {
	assert.Equal(
		t,
		[]int{2, 4, 1, 7, 6, 1, 7, 6},
		RunPhaseMultiple([]int{8, 0, 8, 7, 1, 2, 2, 4, 5, 8, 5, 9, 1, 4, 5, 4, 6, 6, 1, 9, 0, 8, 3, 2, 1, 8, 6, 4, 5, 5, 9, 5}, []int{0, 1, 0, -1}, 100)[0:8],
	)
	assert.Equal(
		t,
		[]int{7, 3, 7, 4, 5, 4, 1, 8},
		RunPhaseMultiple([]int{1, 9, 6, 1, 7, 8, 0, 4, 2, 0, 7, 2, 0, 2, 2, 0, 9, 1, 4, 4, 9, 1, 6, 0, 4, 4, 1, 8, 9, 9, 1, 7}, []int{0, 1, 0, -1}, 100)[0:8],
	)
}
