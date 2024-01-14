package day21

import (
	"fmt"
	"os"
	"strings"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	var wg sync.WaitGroup
	input := make(chan int)
	output := make(chan int)
	wg.Add(1)
	go func() {
		intcode.ExecFull(program, input, output)
		defer wg.Done()
	}()

	for ch := range output {
		fmt.Print(string(rune(ch)))
		if ch == 10 {
			break
		}
	}

	// Jumping clears 3 tiles.  We want to jump if we would die in the next
	// two, and if 4 tiles away is safe.

	// Jump if: (A FALSE OR b FALSE OR c FALSE) AND D true.

	// We also need to clear the JUMP register.
	// I wonder if we want to carry the T + J forward.
	// How to program: if jump is true, set to false.  Otherwise set to false.

	// Clear jump register
	// NOT J T
	// AND T J
	// Jump if A or B is false.
	// NOT A T
	// OR T J
	// NOT B T
	// OR T J
	// However, don't jump if C is false.
	// AND C J

	// So we need (A true OR B true) AND C false.

	instructions := []string{
		"NOT J T",
		"AND T J",
		"NOT A T",
		"OR T J",
		"NOT B T",
		"OR T J",
		"NOT C T",
		"OR T J",
		"AND D J",
		"WALK",
		"",
	}
	for _, ch := range strings.Join(instructions, "\n") {
		input <- int(ch)
	}
	for ch := range output {
		if ch > 0 && ch <= 127 {
			fmt.Print(string(rune(ch)))
		} else {
			// our answer
			fmt.Print(ch)
		}
	}
	wg.Wait()
}
