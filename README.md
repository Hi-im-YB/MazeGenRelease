# Haskell Maze Generator

This software provides an implementation of several algorithms to generate
mazes. The mazes are shown with
[Gloss](https://hackage.haskell.org/package/gloss)
(2D vector graphics, package for Haskell). It also allows to draw paths from one
place to another, and is capable of generating perfect and non-perfect mazes.
This project is strongly inspired by
[this](http://weblog.jamisbuck.org/2011/2/7/maze-generation-algorithm-recap)
blog of Maze Generation algorithms

Algorithms implemented so far
- [Randomized Prims]
- [Growing Tree]
- [Randomized Kruskals]
- [Sidewinder]
- [Recursive Backtracker]
- [Eller's Algorithm]
#### Usage

The parameters for building the maze, are specified in `config.json` file. Which
by default is:

``` CONFIG.json
{
    "algorithm": "Prims",
    "maze-size": "(30,30)",
    "walls-to-remove": "10",
    "draw-paths": "True",
    "path-starting-point": "(0,0)",
    "path-ending-point": "(29, 29)"
}
```

`walls-to-remove` refers to the amount of walls to remove from the maze to make
a non-perfect maze. The default 0, generates perfect mazes (one and only one
path from any place to another).

When `draw-paths` is on, it I'll draw shortest paths in red, and the more large
ones in purple.


```