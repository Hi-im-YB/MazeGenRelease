# Haskell Maze Generator


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
