package day13

import (
	"fmt"
	"os"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func key(x int, y int) string {
	return fmt.Sprintf("%d,%d", x, y)
}

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	tiles := make(map[string]int)
	quit := make(chan bool)

	input := make(chan int)
	output := make(chan int)

	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		intcode.ExecFull(program, input, output)
		quit <- true
		defer wg.Done()
	}()
	go func() {
	ProcessingLoop:
		for {
			x := <-output
			y := <-output
			tileId := <-output
			tiles[key(x, y)] = tileId

			select {
			case <-quit:
				break ProcessingLoop
			default:
				// OK
			}
		}
		defer wg.Done()
	}()
	wg.Wait()

	if !partTwo {
		blockTiles := 0
		for _, v := range tiles {
			if v == 2 {
				blockTiles += 1
			}
		}
		fmt.Println(blockTiles)
	}
}
