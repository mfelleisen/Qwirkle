## Players 

The component in this directory implements a Maze player.

A player implements the mechanism that employs "strategies" to compute
responses. For Maze, a strategy is a function from the current state
of the game and the player's current goal to a response.

Some player implementations realize 

- cheaters, which is the normal misbehavior in a game;
- exception raising ones, which represents misbehavior of local components;
- time-consuming ones, which represents misbehavior in distributed settings.

### Concepts and Relationships


```
+----------+
|  Player  |
+----------+               +----------+ 
| Strategy | ------------> | Strategy |
+----------+               +----------+ 
| setup    |               | closest  | (DistanceOrdering e-r)
| takeTurn |               | Euclid   |  use one of the two exported 
| win      |               | Riemann  |
+----------+               +----------+
     ^
     |
     |
  +--+------------------------+ .... 
  |                           |
+----------+               +----------+
| ExnPlyr  |               | InfPlyr  |
+----------+               +----------+
```

### Functionality

The player is a mostly-functional data representation. The goal is
handed over via `setup` and acts as "temporary" constant. The
`takeTurn` method combines the goal with the public-knowledge game
state it receives to compute an action.

### Organization


| file | purpose |
|--------------------- | ------- |
| [mechanics.rkt](mechanics.rkt) | the player mechanism; several broken variants, provided via "factories" to bundle them into groups | 
| [strategies.rkt](strategies.rkt) | simple strategies for picking a single placement and iterating those | 
