## Remotes 

The components in this directory implement the remote-proxy protocol
for Maze referees and players.

The `remote-player` wraps a TCP connection on the server side and then
implements the same interface as the actual player. When the referee
calls its methods, the arguments are turned into JSON, and sent across
the TCP connection. When a reply shows up, the method turns the JSON
message into an internal Maze-data representation and returns it to the
referee.

The `remote-referee` wraps a TCP connection on the client side and
then implements the referee interface. It waits for JSON messages on
the TCP input side, turns them into internal Maze-data
representations, and calls the player. Once the player returns, the
result is translated into JSON and sent back to the server. 

Three auxiliary components (`define-dispatcher.rkt`,
`define-remote.rkt`, `remote-testing.rkt`) are libraries that allow me
to express the remote-proxy implementation work as derived from the
signatures of the relevant methods.

### The Protocol

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

### Organization


| file | purpose |
|--------------------- | ------- |
| [define-dispatcher.rkt](define-dispatcher.rkt) | create a remote manager from types of the player's methods | 
| [define-remote.rkt](define-remote.rkt) | ovide | 
| [player.rkt](player.rkt) | this remote player implements the same interface as the player but conveys its arguments | 
| [referee.rkt](referee.rkt) | the proxy referee runs a player in the same context as a referee proper | 
| [remote-testing.rkt](remote-testing.rkt) | a facility for testing remote players | 
