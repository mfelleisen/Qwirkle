## Common 

this component represents the common ontology of Q players and the Q gaming framework 

### Table of Content


| file | purpose |
|--------------------- | ------- |
| [player-interface.rkt](player-interface.rkt) | a player interface that the referee can use to service players | 
| [placement.rkt](placement.rkt) | a data representation for player actions, esp. placements | 
| [q-rule.rkt](q-rule.rkt) | the Q specific rule for fitting a tile into a space | 
| [game-state.rkt](game-state.rkt) | a data representation of the generic game-state knowledge | 
| [state-of-player.rkt](state-of-player.rkt) | a data representation of the state of a Q player | 
| [map.rkt](map.rkt) | data representation of a Q map | 
| [coordinates.rkt](coordinates.rkt) | data representation of map coordinates | 
| [tiles.rkt](tiles.rkt) | data representation of tiles, shapes, and colors | 
| [tile-colors.rkt](tile-colors.rkt) | the colors of tiles | 
| [tile-shapes.rkt](tile-shapes.rkt) | the shapes of tiles | 



Here is a rough overview of the layers: 

```
 KEY CONCEPTS
 
        + ---------------- +
        | player-interface |
        + ---------------- +
        | - setup          | refers to `states`, `coordinates`
        | - take-turn      |
        | - new-tiles      |
        | - win            |
        | - name           |
        + ---------------- +
        
        + ---------------- +    *     + ---------------- +          
        | game-state       | -------> | state-of-players | 
        + ---------------- + ---+     + ---------------- +         
        | players          |    |     | score            |
        | tiles            |    |     | tiles            |
        | legal            |    |     | payload          |
        | score            |    |     + ---------------- +
        | active-*         |    |
        + ---------------- +    |
                                |
        ---------------------------------------------------------------------------------------------------
                                |
 BASIC CONCEPTS                 |
                                |
                                +-------> + ------------ + 
                                          | map          | 
                                          + ------------ +    
                                +-------- | sparse       | 
                                | +------ | graph        | 
                                | |       + ------------ + 
                                | |                        
                                | |       + ------------ + 
                                | +-----> | tiles        |          
                                |         + ------------ +          +------------+
                                |         | color        | -------> | color      |
                                |         | shape        | ----+    +------------+
                                |         + ------------ +     |    | is-color?  |
                                |                              |    | allColors? |
                                |         + ------------ +     |    +------------+
                                +-------> | Coordinates  |     |
                                          + ------------ +     |    +------------+
                                          | row relative |     +--> | shape      |
                                          | col relative |          +------------+
                                          | ordering     |          | is-shape?  |
                                          + ------------ +          | allShapes? |
                                                                    +------------+
```


### Generic Game State and Interaction Protocol 

```

  [apply-action
   ;; apply a one-tile placement to the state 
   (-> state? placement? state?)]

  [active-sop-finished?
   ;; did the active player just finish the game?
   (->* (state?) [(listof tile?)] boolean?)]

  [legal
   ;; is the series of placements legale in this state; if so compute the new map 
   (-> state? (listof placement?) (or/c #false map?))]
  
  [score
   ;; legal confirmed, new map evaluated with placements that produced it 
   ;; referee must add bonus for finish
   ;; SHOULD THIS BE JUST A PART OF `complete-turn`?? NO, because the ref adds the 'finish bonus'
   ;; the ref must consult the state and determine whether the active player has placed all tiles
   ;; --> introduce score+ function that determines by itself whether this is true??? 
   (->* (map? (listof placement?)) (#:finishing natural?) natural?)]

  [state-handouts
   #; (state-handouts s n) ; produce the tiles to be handed to the actual player and a revised state
   ;; -- if n is #false, use the tiles in possession of the player representation
   ;; -- otherwise, ; takes away at most n tiles from the current pile
   (-> state? (or/c #false natural?) (values (listof tile?) state?))]

  [state-rotate
   ;; make the first player the last one 
   (-> state? state?)]

-----------------------------------------------------------------------------

   game-state                     referee                         player (p_1) 
        |                            |                                | 
        |                            |   take-turn(PublicState)       | 
        |                            | -----------------------------> | 
        |                            |     seq[Placement]             | 
        | legal(seq[Placement])      | <============================  | 
        | <------------------------- |                                |
        |       m:Map                |                                |
        | =========================> |                                |
        |                            |                                |
        | score(m:Map,seq[Placement])|                                |
        | <------------------------- |                                |
        |       s:Natural            |                                |
        | =========================> |                                |
        |                            |                                |
        |                            |                                |
        |                            |                                | 
        |                            |     new-tiles(set[Tile])       | 
        |                            | -----------------------------> |
        | state-handouts(h:set[Tile])|                                |
        | <------------------------- |                                |
        | state-rotate()             |                                |
        | <------------------------- |                                |
        | 
```

### The Remote Protocol

The following sequence diagrams sketch how proxy referee and the proxy
player re-connect the referee and the players over TCP. 

#### Starting the Game 

```

           server side                                   many client sides (*)
-----------------------------------------------------------------------------------
referee                       proxy_player (p_*) //    proxy_ref (p_*)   player p_*
  |                               |              //      |                | 
  |                               |              //      |                | 
  |  setup(s:State,t:bagOTiles)   |              //      |                | 
  | ----------------------------> |              //      |                |   
  |                               |  (s,t):JSON  //      |                | 
  |                               | ~~~~~~~~~~~~ // ~~~> |                | 
  |                               |              //      |   setup(s,t)   | 
  |                               |              //      | -------------> | 
  |                               |              //      |   void         | 
  |                               |  void:JSON   //      | <============  | 
  |    void                       | <~~~~~~~~~~~ // ~~~~ |                | 
  | <============================ |              //      |                | 
  |                               |              //      |                | 
  |                               |              //      |                | 
  .                               .              //      .                . 
  .                               .              //      .                . 
```

#### Running Turns

```

           server side              proxies             clients
-----------------------------------------------------------------------------------
referee                              //     player (p_1) . . . player (p_n)
  |                                  //             |                 |
  |   take-turn(PublicState)         //             |                 | 
  | ---------------------------- ~~~ // ~~~ ---> -> |                 | 
                                     //          
  ACTION 1:                          //        
  |     PASS                         //             |                 | 
  | <=========================== ~~~ // ~~~ ===  =  |                 | 
  |                                  //             |                 | 
                                     //             

  ACTION 2:                          //             
  |     REPLACE                      //            |                 | 
  | <=========================== ~~~ // ~~~ ===  = |                 | 
  |                                  //            |                 | 
  |--+                               //            |                 |
  .  |                               //            .                 .
  .  |                               //            .                 .
  .  |                               //            .                 .
  .<-+                               //            .                 .
                                     //            

  ACTION 3:                          //            
  |     EXTENSION                    //            |                 | 
  | <============================ ~~ // ~~~ ===  = |                 | 
  |                                  //            |                 | 
  |--+                               //            |                 |
  .  |                               //            .                 . 
  .  |                               //            .                 . 
  .  |                               //            .                 . 
  .  |                               //            .                 . 
  .  |                               //            .                 . 
  .<-+                               //            .                 .


  IF: the player asks for REPLACE or EXTENSIONS, the referee completes the turn: 

  |                                  //            |                 |
  |     new-tiles(bagOfTiles)        //            |                 | 
  | -----------------------------> ~ // ~~~~ === > |                 | 
                                     //            |                 |
```

#### Ending the Game

```
  server side              proxies             clients
-----------------------------------------------------------------------------------
referee                              player (p_1) . . . player (p_n)
  |                         //               |                 |
  |                         //               |                 |
  |    win(Boolean)         //               |                 |
  | ---------------- ~~~~   //  ~~~~ ------> |                 | 
  |                         //               |                 | 
  .                         //               .                 . 
  .                         //               .                 . 
  .                         //               .                 . 
  .                         //               .                 .
  |    win(Boolean)         //               |                 |
  | ---------------- ~~~~   //  ~~~~ ------------------------> |
  |                         //               |                 |
  |                         //               |                 |
```

### The Logical Protocol 

#### Starting the Game 

```

referee                         player (p_1) . . . player (p_n)
  |                                |                 |
  |                                |                 |
  |                                |                 |
  |     setup(state,bagOfTiles)    |                 | % the initial map for this game
  | -----------------------------> |                 | % a set of tiles for this player
  |                                |                 | 
  .                                .                 .
  .                                .                 . % repeat for descending age
  .                                .                 . 
  |                                |                 |
  |     setup(state,bagOfTiles)    |                 | 
  | -----------------------------------------------> |
  |                                |                 |
```

#### Running Turns

```

referee                         player (p_1) . . . player (p_n)
  |                                |                 |
  |   take-turn(PublicState)       |                 | % player receives:
  | -----------------------------> |                 | % - current visible state            

  ACTION 1:
  |     PASS                       |                 | 
  | <============================  |                 | % pass on this turn 
  |                                |                 | 


  ACTION 2:
  |     REPLACE                    |                 | 
  | <============================  |                 | % replace request
  |                                |                 | 
  |--+                             |                 |
  .  |                             .                 . % if legal:
  .  |                             .                 . % completes turn
  .  |                             .                 . % otherwise: 
  .<-+                             .                 . % kick player out 


  ACTION 3:
  |     EXTENSION                  |                 | % an extension consists of a 
  | <============================  |                 | - non-empty sequence of 
  |                                |                 | - tile-coordinate pairs
  |--+                             |                 |
  .  |                             .                 . % if legal:
  .  |                             .                 . % referee modifies game state
  .  |                             .                 . % completes turn 
  .  |                             .                 . % otherwise: 
  .  |                             .                 . % kick player out 
  .<-+                             .                 .


  IF: the player asks for REPLACE or EXTENSIONS, the referee completes the turn: 

  |                                |                 |
  |     new-tiles(bagOfTiles)      |                 | % the player is handed
  | -----------------------------> |                 | % a new set of tiles 


  REPEAT: 
  |   take-turn(PublicState)       |                 |
  | -----------------------------------------------> |
  |     response                   |                 |
  | <=============================================== | 
  |                                |                 |
  .                                .                 .
  .                                .                 . % repeat until the referee 
  .                                .                 . % determines that the game is over
  .                                .                 .
```

#### Ending the Game

```

referee                        player (p_1) . . . player (p_n)
  |                                |                 |
  |                                |                 |
  |     win(Boolean)               |                 | 
  | -----------------------------> |                 | % true means "winner"
  |                                |                 | % false means "loser" 
  .                                .                 . 
  .                                .                 . 
  .                                .                 .
  .                                .                 .
  |     win(Boolean)               |                 |
  | -----------------------------------------------> | % both winners and 
  |                                |                 | % losers are informed 
  |                                |                 |
```
