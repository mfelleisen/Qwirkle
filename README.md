## The Q Project

### Install

```
$ git clone git:...
$ cd Qwirkle
$ raco pkg install
$ raco doc Qwirkle 
```

### Run

See [Run](Run/README.md)

#### Generate Readme

To re-generate the README file, run

```
$ racket readme.rkt 
```

The repository is a fully functioning game framework, intended to be uses
as both an educational and a research project.

### Read 

#### Table of Content 

The following table describes the purpose of each directory in this repository.
For detailed explanations of the files, follow the links. 


See [Modular Programming](https://felleisen.org/matthias/Thoughts/Modular_Programming.html)
for an explanation of how code files are organized in Racket.

| directory | purpose |
|--------------------- | ------- |
| [Run](Run/README.md) | how to run the system from either the server side or the client side (or both) | 
| [Server](Server/README.md) | this component implements the Q server. | 
| [Referee](Referee/README.md) | this component implements the Q referee | 
| [Common](Common/README.md) | this component represents the common ontology of Q players and the Q gaming framework | 
| [Client](Client/README.md) | this component directory implements the client code for the Q game. | 
| [Player](Player/README.md) | this component implements a Q player. | 
| [Integration](Integration/README.md) | this directory satisfies the basic specs of the integration tests of milestone 10 for Sw Dev F'23. | 
| [Lib](Lib/README.md) | this library provides functionality that should probably (or may) exist in Racket's libraries | 
| [Remote](Remote/README.md) | this component support the implementationn of a remote-proxy protocol, | 

| file | purpose |
|--------------------- | ------- |
| [LICENSE-APACHE](LICENSE-APACHE) |  | 
| [LICENSE-MIT](LICENSE-MIT) |  | 
| [homework.rkt](homework.rkt) | this file exists to export names for the milestone specifications of the Sw Dev course | 
| [xreadme](xreadme) | generate the README.md files from README.source files, *.txt files, and purpose statements | 
| [xtest](xtest) | run `raco test` on all relevant code files | 


#### The Idea 

"Q" is a table game. Players place tiles on an "infinitely large" table,
building a map. Each placement must satisfy certain constraints; each
placement is immediately scored. When a player succeeds in placing all
tiles, the game ends. 

This repository is a framework for programming competitive "Q" players,
a variant of the Qwirkle original game.  Participants design automated
players that run on their desktops and connect to a (remote) "Q"
server. This server runs a single game. Any misbehavior of a
player---non-responsiveness due to bugs or cheating---results in
immediate termination. So the competition is first about delivering
robust code and second about writing great strategies.

#### Milestones

See [Milestones](https://felleisen.org/matthias/4500-f23/index.html) for details.

|  Milestone | design & implement | script integration testing	     |
| -----------| ------------------------------------------------------------------------------------- | ----------- |
| 1. The Plan  | work out a plan of "sprints" (milestones), plus essential components; 		     | n/a	   | 
| 2. The Map   | basic game component; the component consists of five data representations  	     | n/a 	   | 
|    	       | (due to the simplicity of this game, add a rule-inspired functionality so that students can test) | |
| 3. The State | data representations for the referee's/the player's knowledge about a game	     | candidates for legal tile placements |
| 4. The Score | an algorithm for computing the score of a player's action     	       		     | legality of player action |
| 5. The Strategies | an atomic strategy for placing one tile, plus an iterator for such strategies  | scoring a player action |
| 6. Games! 	    | a referee and a player mechanism 	       	       		    	 	     | atomic actions |
| 7. The Clean Up   | pay down tech debt     							     | complete games |
| 8. The Observer   | a game observer for the referee's understanding of the process		     | games with failing players |
| 9. Remote 	    | separate the monolithic system into distributed component (via remote proxies) | games with cheating and looping players |
| 10. Revised! 	    | data representations for configurations; configuring servers, refs, clients    | just config scripts for clients and servers |

The final integration test runs

- students' servers with instructor players
- the instructor's server with a student's players
- plus bonus test that evaluate the robustness of students' servers (broken protocols)


### Concepts and Relationships

```
 +----------------------------+                           +----------------------------+
 | Client                     |                           | Server                     |
 +----------------------------+                           +----------------------------+
 | signup-with-server module  |                           | signing-up players module  |
 | player mechanism           | relies on      relies on  | referees, its game state   |
 | strategies                 |-----------+  +------------| observers                  |
 +----------------------------+           |  |            +----------------------------+
                                          |  |
                                          v  v
                 +---------------------------------------------------------+
                 | Common Ontology (of Clients and Server)                 |
                 +---------------------------------------------------------+
                 | player interface and protocol                           |
                 | the player's game state its own knowledge              |
                 |     plus its knowledge about others                     |
                 | the rules (legality checking)			   |
                 | basic game pieces and constants                         |
                 +---------------------------------------------------------+
```


#### The Common Ontology

For participating AI players to connect to the server and participate in games, they
need to understand the interaction protocol, which specify remote calls, their
signatures, and their proper sequencing.  While each message is just a piece JSON
data, the common ontology provides the proper interpretation of these pieces of data
in context.

Additionally, the players' creators need to know the rules of the game. The rules of
the game are expressed in terms of game states and other games pieces. 

Since even a detailed interpretation may leave questions about their meaning with
respect to the rules of the game. the repository's `Common` directory publishes the
code that interprets the remote calls with respect to the server and the sample
player in this repository.

#### Server Sign-Up

```
server                           client (c_1) ... client (c_i)
  |                                |                 | 
  |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ |                 | % tcp connect 
  |                                |                 |
  |   JName n_1                    |                 | 
  |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ |                 | % send a name 
  |                                |                 | % no reply 
  |                                |                 |
  | new(n_1) rpp_1                 |                 |
  |------->+                       |                 | % create remote-proxy player 
  |        |                       |                 |
  |        |                       |                 |
  .        |                       .                 .
  .        |                       .                 .
  .        |                       .                 .
  |        |                       |                 |
  |   JName  n_i                   |                 | 
  |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ | 
  |        |                       |                 |
  |        |                       |                 |
  | new(n_i)           rpp_i       |                 |
  |-------------------->+          |                 |
  |        |            |          |                 |
  |        |            |          |                 |
  |        |            |          |                 |
  |
  |
  |
  |
  | new(rpp_1,..., rpp_n)   referee                     % create referee to run a game 
  |-------------------------------------+               % with the remote proxies
  |                                     |
  |                                     |
  |                                     |
```

#### The Logical Interaction Protocol

The following sequence diagrams sketch how proxy referee and the proxy
player re-connect the referee and the players over TCP. 

_Starting the Game_

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

_Running Turns_

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

_Ending the Game_

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




