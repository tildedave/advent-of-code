package day2

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIntcodeExec(t *testing.T) {
	assert.Equal(t, []int{2, 0, 0, 0, 99}, IntcodeExec([]int{1, 0, 0, 0, 99}))
	assert.Equal(t, []int{2, 3, 0, 6, 99}, IntcodeExec([]int{2, 3, 0, 3, 99}))
	assert.Equal(t, []int{2, 4, 4, 5, 99, 9801}, IntcodeExec([]int{2, 4, 4, 5, 99, 0}))
	assert.Equal(t, []int{30, 1, 1, 4, 2, 5, 6, 0, 99}, IntcodeExec([]int{1, 1, 1, 4, 99, 5, 6, 0, 99}))
	assert.Equal(t, []int{3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50}, IntcodeExec([]int{1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50}))
}
