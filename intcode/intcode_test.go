package intcode

import (
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"
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

func ExecWithChannels(program []int) {
	input, output = make(chan int), make(chan int, 1)
	go ExecFull(program, input, output)
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
	// read value from input, multiply by 5, write to output, halt.
	// use 0 as temp register.
	go ExecFull([]int{3, 0, 1002, 0, 5, 0, 4, 0, 99}, input, output)
	input <- 5
	v := <-output
	assert.Equal(t, 25, v)
}

func TestDay5(t *testing.T) {
	program := ReadProgram(t, "../inputs/day5.txt")

	input := make(chan int)
	output := make(chan int, 100)
	go ExecFull(program, input, output)
	input <- 1

	outputList := make([]int, 0)
	for o := range output {
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
	assert.Equal(t, 1, <-output)

	ExecWithChannels(p1)
	input <- 5
	assert.Equal(t, 0, <-output)

	// less than 8, position mode
	p2 := []int{3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8}
	ExecWithChannels(p2)
	input <- 5
	assert.Equal(t, 1, <-output)

	ExecWithChannels(p2)
	input <- 12
	assert.Equal(t, 0, <-output)

	// equals 8, immediate mode
	p3 := []int{3, 3, 1108, -1, 8, 3, 4, 3, 99}
	ExecWithChannels(p3)
	input <- 8
	assert.Equal(t, 1, <-output)

	ExecWithChannels(p3)
	input <- 5
	assert.Equal(t, 0, <-output)

	// less than 8, immediate mode
	p4 := []int{3, 3, 1107, -1, 8, 3, 4, 3, 99}
	ExecWithChannels(p4)
	input <- 5
	assert.Equal(t, 1, <-output)

	ExecWithChannels(p4)
	input <- 12
	assert.Equal(t, 0, <-output)
}

func TestJump(t *testing.T) {
	p1 := []int{3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9}
	ExecWithChannels(p1)
	input <- 0
	assert.Equal(t, 0, <-output)

	ExecWithChannels(p1)
	input <- 12321
	assert.Equal(t, 1, <-output)

	p2 := []int{3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1}
	ExecWithChannels(p2)
	input <- 0
	assert.Equal(t, 0, <-output)

	ExecWithChannels(p2)
	input <- 559
	assert.Equal(t, 1, <-output)
}

func TestJumpsAndCompares(t *testing.T) {
	p := []int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
		1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
		999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}

	ExecWithChannels(p)
	input <- -12
	assert.Equal(t, 999, <-output)

	ExecWithChannels(p)
	input <- 8
	assert.Equal(t, 1000, <-output)

	ExecWithChannels(p)
	input <- 800
	assert.Equal(t, 1001, <-output)
}

func TestDay5PartTwo(t *testing.T) {
	program := ReadProgram(t, "../inputs/day5.txt")
	ExecWithChannels(program)

	input <- 5
	assert.Equal(t, 10376124, <-output)
}

func TestDay7Examples(t *testing.T) {
	var result int
	program1 := []int{3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0}
	perm := []int{4, 3, 2, 1, 0}
	outputs := make([]chan int, len(perm))
	for n := range perm {
		outputs[n] = make(chan int)
	}
	var wg sync.WaitGroup
	systemInput := make(chan int)

	for n, i := range perm {
		var input chan int
		if n == 0 {
			input = systemInput
		} else {
			input = outputs[n - 1]
		}
		wg.Add(1)
		go func(i int, input, output chan int) {
			ExecFull(program1, input, output)
			wg.Done()
		}(i, input, outputs[n])
		input <- i
	}

	systemInput <- 0
	for result = range outputs[len(perm) - 1] {
	}
	wg.Wait()
	assert.Equal(t, 43210, result)
	close(systemInput)

	systemInput = make(chan int)
	program2 := []int{3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0}
	perm = []int{0, 1, 2, 3,4}
	outputs = make([]chan int, len(perm))
	for n := range perm {
		outputs[n] = make(chan int)
	}
	for n, i := range perm {
		var input chan int
		if n == 0 {
			input = systemInput
		} else {
			input = outputs[n - 1]
		}
		wg.Add(1)
		go func(i int, input, output chan int) {
			ExecFull(program2, input, output)
			wg.Done()
		}(i, input, outputs[n])
		input <- i
	}
	systemInput <- 0
	for result = range outputs[len(perm) - 1] {
	}
	wg.Wait()
	assert.Equal(t, 54321, result)
	close(systemInput)

	systemInput = make(chan int)
	program3 := []int{3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0}
	perm = []int{1, 0, 4, 3, 2}
	outputs = make([]chan int, len(perm))
	for n := range perm {
		outputs[n] = make(chan int)
	}
	for n, i := range perm {
		var input chan int
		if n == 0 {
			input = systemInput
		} else {
			input = outputs[n - 1]
		}
		wg.Add(1)
		go func(i int, input, output chan int) {
			ExecFull(program3, input, output)
			wg.Done()
		}(i, input, outputs[n])
		input <- i
	}
	systemInput <- 0
	for result = range outputs[len(perm) - 1] {
	}
	wg.Wait()

	assert.Equal(t, 65210, result)
	close(systemInput)
}

func TestQuine(t *testing.T) {
	program := []int{109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99}
	output := make(chan int, 1)
	go ExecFull(program, make(chan int, 1), output)
	for o := range output {
		fmt.Println(o)
	}
}
