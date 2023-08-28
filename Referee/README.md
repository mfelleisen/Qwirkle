## Referee 

The component in this directory implements the Maze referee. 

The referee is organized as a mechanism that sets up an initial state,
consults external players to cause transitions from one state
to the next, and applies a rule chhecker to each such transition
until a final state is reached. 

### Concepts and Relationships

```
+---------+
| Referee |
+---------+               +---------+
| State s |-------------->| RState  | (ordering of players)
| Rules r |---+           +---------+
+---------+   |           | Tile x  |
              |           | Undo u  |             +-------+
              |           | Board b |------------>| Board |
              |           | PlayerQ |---+         +-------+          +------+
              |           +---------+   |         | Tiles |---*----->| Tile |
              |                         |         | CoordS|          |      |
              |                         |         +-------+          +------+
              |                         |
         +-----------+                  |
         | RuleCheck |                  * Queue 
         +-----------+                  |
                                        |         +---------+
                                        +-------->| RIP     | (knowledge 
                                                  | Referee |  about individual
                                                  | Player  |  players)
                                                  +---------+
                                                  | Goal    |
                                                  | Home    |
                                                  | Current |
                                                  | Color   |
                                                  | PayLoad | (parametric, used to contact actual players)
                                                  +---------+
                                        
```

### Functionality 

The referee creates an initial referee state and then proceeds as follows: 

- it sets up the players with an initial public state and goal 
- it grants the players turns on a per round bassis, and each turn may change the state 
- it applies the rule book to each state after each transition
  (possibly yielding the same state)
- it stops the game as any of the terminal conditions are met
- it scores the final state, and
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
      |                   |   update_state(State s)    | 
      |                   | <------------------------- | %% the initial state 
      |                   |                            |
      .                   |   update_state(State s)    | 
      .                   | <------------------------- | %% + a state after each legal turn
      .                   .                            .
      |                   .                            .
      |                   .                            .                               
      |                   |                            |
      |                   |   update_state(State s)    |
      |                   | <------------------------- | %% until the game ends 
      |                   |                            |    
      |                   |                            |
      |                   |   game_over()              |
      |                   | <------------------------- |
      |                   |
      |                  ---- the observer shuts down and cannot accept more update calls 
```

### Details: Referee 

Refereeing a game proceeds in four and a half stages:

 1. setting up the players so that they can "think" about the initial state 
 2. running rounds of turns until
    - there is a winner
    - all players "pass" during a single round
    - `LIMIT` rounds have been completed without either of the preceding outcomes
 3. scoring the game
 4. informing winners and losers of the outcome of the game

On the side, the referee keeps track of ill-behaved players so that once they
perform any bad action, they are never contacted again. 

To discover ill-behaved players, the referee protects _all_ calls to players with

```
 (xsend player method-name arg ...)
```     
so that it catches

- exn-s raised internally
- can terminate overly long calls.

The module introduces a form `xxsend`, which combines `xsend` with a case analysis for all possible results. 

TODO It should protect calls to observer, too. 

The referee's game state suffices to resume a game at exact round bondaries.

The module comes with three entry points:

- referee/zero, which consumes a list of players and runs the game with a random state
- referee/state, which consumes a preconfigured state (plus optional observer) for testing
- referee/config, which is like referee/state and also allows the configuration of several items

Configurations: A configuration dictionary may contain: 

- an initial state;
- a limit on the number of rounds that the referee will run before declaring an end to the game;
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
| [ref-state.rkt](ref-state.rkt) | a data representation of the referee's knowledge about the game | 
| [referee.rkt](referee.rkt) | referee: mediates between external players (local or remote) and the referee state, via safe xsend | 
