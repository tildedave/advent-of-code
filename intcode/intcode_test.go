package intcode

import (
	"errors"
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/tildedave/advent-of-code-2019/utils"
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

func TestDay5(t *testing.T) {
	fBytes, err := os.ReadFile("../inputs/day5.txt")
	if err != nil {
		assert.Fail(t, "Unable to read file", err.Error())
	}
	program, err := utils.ParseNumberList(strings.Split(strings.TrimSpace(string(fBytes)), ","))
	if err != nil {
		assert.Fail(t, "Unable to parse program", err.Error())
	}

	input := make(chan int)
	output := make(chan int, 100)
	halt := make(chan bool)
	go ExecFull(program, input, output, halt)
	input <- 1
	h := <-halt
	assert.True(t, h)

	outputList := make([]int, 0)
	for {
		o, ok := <-output
		if !ok {
			break
		}
		outputList = append(outputList, o)
	}
	for j, o := range outputList {
		if j != len(outputList)-1 {
			assert.Equal(t, o, 0, "Should have output zero prior to final diagnostic code")
		} else {
			assert.NotEqual(t, o, 0, "Final output should not have been zero (should have been my diagnostic code)")
		}
	}
}
