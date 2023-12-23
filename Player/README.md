## Players 

The component in this directory implements a Q player.

A player implements the mechanism that employs "strategies" to compute
responses. For Q, a strategy is a function from the current state
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
| setup    |               | dag      | 
| takeTurn |               | ldasg    | 
| win      |               |          |
+----------+               +----------+
     ^
     |
     | via macros and mixins 
     | 
     |
  +--+------------------------+ .... 
  |                           |
+----------+               +----------+
| ExnPlyr  |               | InfPlyr  |
+----------+               +----------+
```

### Functionality

The mechanism is a functional component.

A strategy is always a function. Even a history-sensitive variant
should consume a data representation of the game's history.  



| file | purpose |
|--------------------- | ------- |
| [mechanics.rkt](mechanics.rkt) | the player mechanism; several broken variants, provided via "factories" to bundle them into groups | 
| [strategies.rkt](strategies.rkt) | simple strategies for picking a single placement and iterating those | 
| [README.md](README.md) |  component in this directory implements a Q player. | 
| [README.source](README.source) |  component in this directory implements a Q player. | 
