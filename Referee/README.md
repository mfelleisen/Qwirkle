## Referee 

The component in this directory implements the Q referee. 

The referee is organized as a mechanism that sets up an initial state,
consults external players to cause transitions from one state
to the next, and applies a rule chhecker to each such transition
until a final state is reached. 

### Concepts and Relationships

```
+---------+
| Referee |
+---------+               +---------+
| State s |-------------->| RefState| (between two rounds)
+---------+   +---------- +---------+
              |           | Tiles x |
              |           | PlayerQ |             +-------+
              |           | Map m   |------------>| Map   |
              |           +---------|---+         +-------+          +------+
              |                         |         | Tiles |---*----->| Tile |
              |                         |         |       |          +------+
              |                         |         +-------+          | Color|
              V                         |                            | Shape|
         +-----------+                  |                            +------+
         | RuleCheck |                  * Queue 
         +-----------+                  |
                                        |         +---------+
                                        +-------->| SoP     |
                                                  +---------+
                                                  | Tiles   |
                                                  | Score   |
                                                  | PayLoad | (parametric, 
                                                  +---------+  connectionn to 
                                                               actual player)                                        
```

### Functionality 

The referee creates an initial referee state and then proceeds as follows: 

- it sets up the players with an initial public state and goal 
- it grants the players turns on a per round bassis,
- .. and each turn may change the state 
- it checks the legality via a call to its state representation 
- .. which yields the next (possibly same) state
- it scores each turn via a call to its state representation 
- it stops the game as any of the terminal conditions are met
- it informs the surviving players of their status as winners or losers.

### Referee and Observers

Some external code unit creates an observer in a new EventSpace and
hands it to the newly create referee (together with other data, say, a
player or states). The thread hands states to this thread as long as 

```
    External 
      |
  o = | ------------> Observer 
      |                   |
      |                   |              
      |                   | 
      |                   | 
      |                   | 
      |                   |  new(O)                 Referee  
      |                   | -------------------------> |
      |                   |                            | 
      |                   |                            |
      .                   |   update(State s)          | 
      .                   | <------------------------- | %% + a state before each legal turn
      .                   .                            .
      |                   .                            .
      |                   .                            .                               
      |                   |                            |
      |                   |   update(State s)          |
      |                   | <------------------------- | %% until the game ends 
      |                   |                            |    
      |                   |                            |
      |                   |   game_over() == #false    |
      |                   | <------------------------- |
      |                   |
      |                  ---- the observer shuts down and cannot accept more update calls 
```

The observer is a stateful function. When it receives a state, it
updates its view. When it receives `#false`, the game is over. On
`FLUSH`, it resets itself for the next use.

### Details: Referee 

The referee keeps track of ill-behaved players so that once they
perform any bad action, they are never contacted again.

To discover ill-behaved players, the referee protects _all_ calls to players with

```
 (xsend player method-name arg ...)
```     
so that it catches

- exn-s raised internally
- can terminate overly long calls.

The module introduces a form `xsend+`, which combines `xsend` with a
case analysis for all possible results of this eXternal call. 

TODO It should protect calls to observer, too. 

The referee's game state suffices to resume a game at exact round bondaries.

The module comes with one entry point:

- referee/zero, which consumes a list of players and runs the game with a random state
- referee/state, which consumes a preconfigured state (plus optional observer) for testing
- referee/config, which consumes
  - a referee configuration including an initial state
  - a list of external players (which implement `../Common/player-interface`. 

Configurations: A configuration dictionary may contain: 

- an initial state;
- a flag that controls whether some rudimentary game information goes to the "console";
- a per-turn time limit, which enforces how many seconds a player gets per call; and
- an observer.

The exported `options` variable lists the names of these fields. 

### Details: Observer

The actual observer implementation is functional and has the following
signature:

```
  #; (observer state_0) ; creates an observer function `f`
  #; (f state)          ; hands the observer another state
  #; (f #false)         ; no more states coming 
  observer (-> state? (-> (or/c #false state?) any/c))
```

### Organization 

| file | purpose |
|--------------------- | ------- |
| [referee.rkt](referee.rkt) | referee: mediates between external players (local or remote) and the referee state, via safe xsend | 
| [ref-state.rkt](ref-state.rkt) | a data representation of the referee's knowledge about the game | 
| [observer-2.rkt](observer-2.rkt) | ovide | 
| [observer-interface.rkt](observer-interface.rkt) | an interface for a primitive observer | 
| [observer.rkt](observer.rkt) | a primitive observer that saves all images in Tmp/ and allows users to view game developments | 
