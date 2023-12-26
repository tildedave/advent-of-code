package intcode

import (
	"errors"
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func ReadProgram(t *testing.T, filename string) []int {
	fBytes, err := os.ReadFile(filename)
	if err != nil {
		assert.Fail(t, "Unable to read file", err.Error())
	}
	program, err := utils.ParseNumberList(strings.Split(strings.TrimSpace(string(fBytes)), ","))
	if err != nil {
		assert.Fail(t, "Unable to parse program", err.Error())
	}

	return program
}

var input chan int
var output chan int
var halt chan bool

func ExecWithChannels(program []int) {
	input, output, halt = make(chan int), make(chan int, 1), make(chan bool)
	go ExecFull(program, input, output, halt)
}

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
	program := ReadProgram(t, "../inputs/day5.txt")

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
			assert.Equal(t, o, 15386262, "Final output should not have been my diagnostic code")
		}
	}
}

func TestCompare(t *testing.T) {
	// equals 8, position mode
	p1 := []int{3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8}
	ExecWithChannels(p1)
	input <- 8
	assert.True(t, <-halt)
	assert.Equal(t, 1, <-output)

	ExecWithChannels(p1)
	input <- 5
	assert.True(t, <-halt)
	assert.Equal(t, 0, <-output)

	// less than 8, position mode
	p2 := []int{3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8}
	ExecWithChannels(p2)
	input <- 5
	assert.True(t, <-halt)
	assert.Equal(t, 1, <-output)

	ExecWithChannels(p2)
	input <- 12
	assert.True(t, <-halt)
	assert.Equal(t, 0, <-output)

	// equals 8, immediate mode
	p3 := []int{3, 3, 1108, -1, 8, 3, 4, 3, 99}
	ExecWithChannels(p3)
	input <- 8
	assert.True(t, <-halt)
	assert.Equal(t, 1, <-output)

	ExecWithChannels(p3)
	input <- 5
	assert.True(t, <-halt)
	assert.Equal(t, 0, <-output)

	// less than 8, immediate mode
	p4 := []int{3, 3, 1107, -1, 8, 3, 4, 3, 99}
	ExecWithChannels(p4)
	input <- 5
	assert.True(t, <-halt)
	assert.Equal(t, 1, <-output)

	ExecWithChannels(p4)
	input <- 12
	assert.True(t, <-halt)
	assert.Equal(t, 0, <-output)
}

func TestJump(t *testing.T) {
	p1 := []int{3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9}
	ExecWithChannels(p1)
	input <- 0
	assert.True(t, <-halt)
	assert.Equal(t, 0, <-output)

	ExecWithChannels(p1)
	input <- 12321
	assert.True(t, <-halt)
	assert.Equal(t, 1, <-output)

	p2 := []int{3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1}
	ExecWithChannels(p2)
	input <- 0
	assert.True(t, <-halt)
	assert.Equal(t, 0, <-output)

	ExecWithChannels(p2)
	input <- 559
	assert.True(t, <-halt)
	assert.Equal(t, 1, <-output)
}

func TestJumpsAndCompares(t *testing.T) {
	p := []int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
		1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
		999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}

	ExecWithChannels(p)
	input <- -12
	assert.True(t, <-halt)
	assert.Equal(t, 999, <-output)

	ExecWithChannels(p)
	input <- 8
	assert.True(t, <-halt)
	assert.Equal(t, 1000, <-output)

	ExecWithChannels(p)
	input <- 800
	assert.True(t, <-halt)
	assert.Equal(t, 1001, <-output)
}

func TestDay5PartTwo(t *testing.T) {
	program := ReadProgram(t, "../inputs/day5.txt")
	ExecWithChannels(program)

	input <- 5
	assert.True(t, <-halt)
	assert.Equal(t, 10376124, <-output)
}
