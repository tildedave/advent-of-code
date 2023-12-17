package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"math"
	"os"
)

type GraphNode = struct {
	x        int
	y        int
	dir      int // we'll reuse DIR_LEFT, DIR_RIGHT, etc from past solution
	numSteps int
}

type GraphNodeWithPriority = struct {
	node     GraphNode
	priority int
}

type NodeHeap []GraphNodeWithPriority

func (h NodeHeap) Len() int {
	return len(h)
}
func (h NodeHeap) Less(i, j int) bool { return h[i].priority < h[j].priority }
func (h NodeHeap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }
func (h *NodeHeap) Push(x any) {
	// Push and Pop use pointer receivers because they modify the slice's length,
	// not just its contents.
	*h = append(*h, x.(GraphNodeWithPriority))
}

func (h *NodeHeap) Pop() any {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

func day17(f *os.File) {
	scanner := bufio.NewScanner(f)
	grid := make([][]int, 0)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for j, s := range line {
			row[j] = int(s - 48)
		}
		grid = append(grid, row)
	}

	// distance is based on if you've previously gone forward 1, 2, 3 nodes.
	// potentially also need to multiply by each of the directions.
	distance := make(map[GraphNode]int)
	queue := &NodeHeap{}
	heap.Init(queue)

	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid); j++ {
			for s := 1; s <= 3; s++ {
				distance[GraphNode{i, j, DIR_UP, s}] = math.MaxInt
				distance[GraphNode{i, j, DIR_DOWN, s}] = math.MaxInt
				distance[GraphNode{i, j, DIR_LEFT, s}] = math.MaxInt
				distance[GraphNode{i, j, DIR_RIGHT, s}] = math.MaxInt
			}
		}
	}
	distance[GraphNode{0, 0, DIR_RIGHT, 0}] = math.MaxInt

	// Top left is the only item that can be entered with "0" steps
	// https://cs.stackexchange.com/questions/118388/dijkstra-without-decrease-key
	startNode := GraphNode{0, 0, DIR_RIGHT, 0}
	heap.Push(queue, GraphNodeWithPriority{startNode, 0})
	for queue.Len() > 0 {
		n := heap.Pop(queue).(GraphNodeWithPriority)
		// fmt.Println(queue.Len())
		node, queueDistance := n.node, n.priority
		existingDistance := distance[node]
		if queueDistance < existingDistance {
			distance[node] = queueDistance
			// so now we look at the edges of this node.
			// we can go straight, left, right, but we can't go straight if
			// we're at.
			canUp := node.x > 0 && node.dir != DIR_DOWN && (node.dir != DIR_UP || node.numSteps < 3)
			canDown := node.x < len(grid)-1 && node.dir != DIR_UP && (node.dir != DIR_DOWN || node.numSteps < 3)
			canLeft := node.y > 0 && node.dir != DIR_RIGHT && (node.dir != DIR_LEFT || node.numSteps < 3)
			canRight := node.y < len(grid[0])-1 && node.dir != DIR_LEFT && (node.dir != DIR_RIGHT || node.numSteps < 3)
			// so now for each of these push them onto the queue with the
			// appropriate cost
			if canUp {
				var steps int
				if node.dir == DIR_UP {
					steps = node.numSteps + 1
				} else {
					steps = 1
				}
				n := GraphNode{node.x - 1, node.y, DIR_UP, steps}
				heap.Push(queue, GraphNodeWithPriority{n, distance[node] + grid[n.x][n.y]})
			}
			if canDown {
				var steps int
				if node.dir == DIR_DOWN {
					steps = node.numSteps + 1
				} else {
					steps = 1
				}
				n := GraphNode{node.x + 1, node.y, DIR_DOWN, steps}
				heap.Push(queue, GraphNodeWithPriority{n, distance[node] + grid[n.x][n.y]})
			}
			if canLeft {
				var steps int
				if node.dir == DIR_LEFT {
					steps = node.numSteps + 1
				} else {
					steps = 1
				}
				n := GraphNode{node.x, node.y - 1, DIR_LEFT, steps}
				heap.Push(queue, GraphNodeWithPriority{n, distance[node] + grid[n.x][n.y]})
			}
			if canRight {
				var steps int
				if node.dir == DIR_RIGHT {
					steps = node.numSteps + 1
				} else {
					steps = 1
				}
				n := GraphNode{node.x, node.y + 1, DIR_RIGHT, steps}
				heap.Push(queue, GraphNodeWithPriority{n, distance[node] + grid[n.x][n.y]})
			}
		}
	}

	minDistance := math.MaxInt
	for s := 1; s <= 3; s++ {
		for _, d := range []int{DIR_LEFT, DIR_RIGHT, DIR_UP, DIR_DOWN} {
			dist := distance[GraphNode{len(grid) - 1, len(grid[0]) - 1, d, s}]
			if dist < minDistance {
				minDistance = dist
			}
		}
	}
	fmt.Println(minDistance)
}
