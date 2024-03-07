package day13

import (
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
	"seehuhn.de/go/ncurses"
)

const LOOP_DELAY = 1

// observed size of grid from running program.
const X_MAX = 40
const Y_MAX = 20

func StringForTileId(tileId int) string {
	switch tileId {
	case 0:
		return "."
	case 1:
		return "|"
	case 2:
		return "B"
	case 3:
		return "U"
	case 4:
		return "o"
	default:
		panic("Invalid character at tile")
	}
}

func displayTiles(tiles map[string]int) string {
	// observed boundaries of game
	str := ""
	for y := 0; y < Y_MAX; y++ {
		for x := 0; x < X_MAX; x++ {
			str += StringForTileId(tiles[key(x, y)])
		}
		str += "\n"
	}
	return fmt.Sprintf("Score: %d\n%s", tiles[key(-1, 0)], str)
}

func computePaddleMove(
	win *ncurses.Window,
	tiles map[string]int,
	paddlePosition [2]int,
	lastBallPosition [2]int,
	ballPosition [2]int) int {
	x, _ := ballPosition[0], ballPosition[1]
	if paddlePosition[0] == x {
		return 0
	}

	if paddlePosition[0] < x {
		return 1
	}

	return -1
}

func key(x int, y int) string {
	return fmt.Sprintf("%d,%d", x, y)
}

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	if partTwo {
		// Free play
		program[0] = 2
	}

	tiles := make(map[string]int)

	input := make(chan int)
	output := make(chan int)
	quit := make(chan bool)

	// maxX := math.MinInt
	// minX := math.MaxInt
	// maxY := math.MinInt
	// minY := math.MaxInt

	gameHasStarted := false
	var wg sync.WaitGroup
	var score int
	wg.Add(2)
	go func() {
		intcode.ExecFull(program, input, output)
		quit <- true
		defer wg.Done()
	}()
	go func() {
		ncurses.Init()
		defer func() {
			ncurses.EndWin()
			fmt.Printf("Final score: %d\n", score)
		}()
		win := ncurses.NewWin(35, 120, 0, 0)
		var ballPosition [2]int
		var paddlePosition [2]int
		var paddleMoveDirection = 0

		defer func() {
			if r := recover(); r != nil {
				ncurses.EndWin()
				panic(r)
			}
		}()

	ProcessingLoop:
		for {
			win.Refresh()
			if gameHasStarted {
				time.Sleep(LOOP_DELAY * time.Millisecond)
			}
			select {
			case x := <-output:
				y := <-output
				tileId := <-output
				tiles[key(x, y)] = tileId
				if x != -1 {
					win.MvAddStr(y, x, StringForTileId(tileId))
				} else {
					win.MvAddStr(27, 0, fmt.Sprintf("Score %d", tileId))
					score = tileId
				}

				// So here we need to understand the trajectory of the ball.
				// Remember last ball position, calculate what will happen
				// (block collision? fun), decide what we want to do about it.
				// Not clear
				if tileId == 3 {
					paddlePosition = [2]int{x, y}
				}
				if tileId == 4 {
					paddleMoveDirection = computePaddleMove(
						win,
						tiles,
						paddlePosition,
						ballPosition,
						[2]int{x, y},
					)
					win.Refresh()
					ballPosition = [2]int{x, y}
				}
				if x == -1 {
					gameHasStarted = true
				}
			case input <- paddleMoveDirection:
				win.MvAddStr(32, 0, fmt.Sprintf("Moved in paddle direction %d ", paddleMoveDirection))
				// nothing
			case <-quit:
				win.MvAddStr(31, 0, "Received quit signal for whatever reason")
				break ProcessingLoop
			default:
				win.MvAddStr(31, 0, "No special things")
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
	}
}
