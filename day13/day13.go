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

const LOOP_DELAY = 750

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
	dx := ballPosition[0] - lastBallPosition[0]
	dy := ballPosition[1] - lastBallPosition[1]
	// I guess assume the ball just runs forward until it reaches something.
	// if it's heading towards the padding (dy position)
	// I guess it could hit the edge of the screen too, in which case dx/dy
	// would change.
	// hitting a block destroys the block and does a dx / dy thing.
	// we only have to destroy every block, we don't have to be super efficient
	// I suppose.  guess we'll see how much logic I need to program into this
	// to hit it at an edge or whatever.

	x, y := ballPosition[0], ballPosition[1]
	var path string
	willCollideWithPaddle := false
	numIterations := 0

	for y < Y_MAX {
		x += dx
		y += dy

		path += fmt.Sprintf("(%d, %d) ", x, y)

		// see what's at the location.
		// I guess we can be sort of dumb and not care about removing the
		// blocks here.
		// assumes dx/dy are always in the range [-1, 1]
		potentialXCollision := [2]int{x + dx, y}
		potentialYCollision := [2]int{x, y + dy}

		for n, collider := range [][2]int{potentialXCollision, potentialYCollision} {
			switch tiles[key(collider[0], collider[1])] {
			case 0:
				// nothing
			case 1:
				// wall.  based on if we're at x max, y max, whatever, we reverse.
				if x == 0 || x == X_MAX {
					dx = -dx
				}
				if y == 0 {
					dy = -dy
				}
				path += "[wall] "
			case 2:
				// a block.  based on this being an x-collision or a y-collision
				// we change trajectory.
				// it is valid that we could collide with both in which case we
				// should end up just reversing both dx/dy.

				var collisionType string
				if n == 0 {
					collisionType = "x"
					dx = -dy
				} else {
					collisionType = "y"
					dy = -dy
				}
				path += fmt.Sprintf("[%s-block] ", collisionType)
			case 3:
				willCollideWithPaddle = true
				path += "[paddle]"
			case 4:
				// the same as nothing as we're assuming only one ball.
			}
		}

		if willCollideWithPaddle {
			break
		}
		numIterations++
		if numIterations > 5 {
			path += "[max]"
			break
		}
	}

	win.MvAddStr(28, 0, fmt.Sprintf("Expected ball path: %s", path))
	win.MvAddStr(33, 0, path)
	win.MvAddStr(29, 0, fmt.Sprintf("Paddle position: (%d, %d)", paddlePosition[0], paddlePosition[1]))
	win.MvAddStr(30, 0, fmt.Sprintf("Expected final ball position: (%d, %d)", x, y))
	if willCollideWithPaddle || paddlePosition[0] == x {
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
	wg.Add(2)
	go func() {
		intcode.ExecFull(program, input, output)
		quit <- true
		defer wg.Done()
	}()
	go func() {
		ncurses.Init()
		defer ncurses.EndWin()
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
