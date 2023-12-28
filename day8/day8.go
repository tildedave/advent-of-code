package day8

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

func Run(f *os.File, partTwo bool) {
	scanner := bufio.NewScanner(f)
	var colors []int
	rows := 0
	for scanner.Scan() {
		if rows > 0 {
			panic("Invalid input")
		}
		line := scanner.Text()
		colors = make([]int, len(line))
		for j, c := range line {
			colors[j] = int(c - 48)
		}
		rows++
	}

	// Different examples are different
	layerWidth := 25
	layerHeight := 6
	layerLength := layerHeight * layerWidth
	if len(colors)%layerLength != 0 {
		panic("Height/width did not evenly divide length of input")
	}

	if !partTwo {
		minNumZeros := math.MaxInt
		answer := 0
		for layer := 0; layer < len(colors)/layerLength; layer++ {
			section := colors[layer*(layerLength) : (layer+1)*layerLength]
			numZeros := 0
			numOnes := 0
			numTwos := 0
			for _, n := range section {
				switch n {
				case 0:
					numZeros++
				case 1:
					numOnes++
				case 2:
					numTwos++
				}
			}
			if numZeros < minNumZeros {
				answer = numOnes * numTwos
				minNumZeros = numZeros
			}
		}
		fmt.Println(answer)
		return
	}

	numLayers := len(colors) / layerLength
	decodedImage := make([]int, layerLength)
	for layer := numLayers - 1; layer >= 0; layer-- {
		section := colors[layer*(layerLength) : (layer+1)*layerLength]
		for j, n := range section {
			switch n {
			case 0:
				decodedImage[j] = 0
				// Black
			case 1:
				decodedImage[j] = 1
				// White
			case 2:
				// Transparent, do nothing
			default:
				panic("did not recognize pixel")
			}
		}
	}
	str := ""
	for j, n := range decodedImage {
		if j > 0 && j%layerWidth == 0 {
			str += "\n"
		}
		str += fmt.Sprintf("%d", n)
	}
	fmt.Println(str)
	// I did not write code to OCR the image.  ;-)
}
