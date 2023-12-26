package intcode

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIntcodeExec(t *testing.T) {
	result, err := Exec([]int{1, 0, 0, 0, 99})
	assert.Equal(t, []int{2, 0, 0, 0, 99}, result)
	assert.Nil(t, err)

	result, err = Exec([]int{2, 3, 0, 3, 99})
	assert.Equal(t, []int{2, 3, 0, 6, 99}, result)
	assert.Nil(t, err)

	result, err = Exec([]int{2, 4, 4, 5, 99, 0})
	assert.Equal(t, []int{2, 4, 4, 5, 99, 9801}, result)
	assert.Nil(t, err)

	result, err = Exec([]int{1, 1, 1, 4, 99, 5, 6, 0, 99})
	assert.Equal(t, []int{30, 1, 1, 4, 2, 5, 6, 0, 99}, result)
	assert.Nil(t, err)

	result, err = Exec([]int{1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50})
	assert.Equal(t, []int{3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50}, result)
	assert.Nil(t, err)

	result, err = Exec([]int{1101, 100, -1, 4, 0})
	assert.Equal(t, []int{1101, 100, -1, 4, 99}, result)
	assert.Nil(t, err)
}

func TestParamMode(t *testing.T) {
	result, err := Exec([]int{1002, 4, 3, 4, 33})

	assert.Equal(t, []int{1002, 4, 3, 4, 99}, result)
	assert.Nil(t, err)

	_, err = Exec([]int{11002, 4, 3, 4, 33})
	assert.NotNil(t, err)
	assert.Equal(t, errors.New("specified MODE_IMMEDIATE for destination"), err)
}

func TestWithInputAndOutput(t *testing.T) {
	input := make(chan int)
	output := make(chan int)
	halt := make(chan bool)
	// read value from input, multiply by 5, write to output, halt.
	// use 0 as temp register.
	go ExecFull([]int{3, 0, 1002, 0, 5, 0, 4, 0, 99}, input, output, halt)
	input <- 5
	v := <-output
	assert.Equal(t, 25, v)

	h, ok := <-halt
	assert.True(t, h)
	assert.True(t, ok)
}
