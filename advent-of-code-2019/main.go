package main

import (
	"flag"
	"log"
	"os"
	"runtime/pprof"

	"github.com/tildedave/advent-of-code-2019/day23"
)

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")

func main() {
	flag.Parse()
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}

	f, err := os.Open("./inputs/day23.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	day23.Run(f, false)
}
