## Common 

The components in this directory represent the common ontology that the players and the gaming framework must share.

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
        
        + ---------------- +          + ---------------- +          
        | game-state       |          | state-of-players |         
        + ---------------- +          + ---------------- +         
        | players          |          | score	    |
        | tiles       	   |          | tiles 	    |
        | legal       	   |          | payload	    |
        | score      	   |          + ---------------- +
        | active-*    	   |
        + ---------------- +
          
        ---------------------------------------------------------------------------------------------------
 BASIC CONCEPTS 

                                        + ------------ +
                                        | map          |
                                        + ------------ +
                                        | graph	       |
					| extend       |
                                        + ------------ +

                                        + ------------ +
                                        | tiles        |
                                        + ------------ +
                                        | color        |
                                        | shape	       |
                                        + ------------ +

                                        + ------------ +          
                                        | coordinates  |          
                                	+ ------------ +          
                                	| row relative |
                                	| col relative |
                                	| ordering     |
                                	+ ------------ +          
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
  |  setup(s:State,t:Tiles)       |              //      |                | 
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
  |     new-tiles(setOfTiles)        //            |                 | 
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
  |     setup(map,setOfTiles)      |                 | % the initial map for this game
  | -----------------------------> |                 | % a set of tiles for this player
  |                                |                 | 
  .                                .                 .
  .                                .                 . % repeat for descending age
  .                                .                 . 
  |                                |                 |
  |     setup(map,setOfTiles)      |                 | 
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
  |     new-tiles(setOfTiles)      |                 | % the player is handed
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

### Organization


| file | purpose |
|--------------------- | ------- |
| [coordinates.rkt](coordinates.rkt) | data representation of map coordinates | 
| [game-state.rkt](game-state.rkt) | a data representation of the generic game-state knowledge | 
| [map.rkt](map.rkt) | data representation of a Q map | 
| [placement.rkt](placement.rkt) | a data representation for player actions, esp. placements | 
| [player-interface.rkt](player-interface.rkt) | a player interface that the referee can use to service players | 
| [state-of-player.rkt](state-of-player.rkt) | a data representation of the state of a Q player | 
| [tiles.rkt](tiles.rkt) | data representation of tiles, shapes, and colors | 
