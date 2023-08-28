## The Q Project

#### Next Step

- remotes: (1) remote (2) client (3) server
- modify essential aspect

- integration tests for students 

- readme sources : clean up
  - explain the separation of `Common/game-state` and `Referee/ref-state` 

#### Install

```
$ git clone git:...
$ cd Qwirkle
$ raco pkg install
$ raco doc Qwirkle 
```

#### Run

- todo

#### Generate Readme

To re-generate the README file, run

```
$ racket readme.rkt 
```

The repository is a fully functioning game framework, intended to be uses
as both an educational and a research project.

### The Idea 

"Q" is a tole game. This repository is a framework for programming
competitive "Q" players, a variant of the Qwirkle original game.
Participants will design automated players that run on their desktops
and connect to a (remote) "Q" server. This server will run a single
game. Any misbehavior of a player---non-responsiveness due to bugs or
cheating---results in immediate termination. So the competition is
first about delivering robust code and second about writing great
strategies.

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
                 | the player's game state: its own knowledge              |
                 |     plus its knowledge about others                     |
                 | the rules (legality checking)			   |
                 | basic game pieces and constants                         |
                 +---------------------------------------------------------+
```


### The Common Ontology

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

### Server Sign-Up

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

### The Logical Interaction Protocol

The following sequence diagrams sketch how proxy referee and the proxy
player re-connect the referee and the players over TCP. 

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

The following table describes the purpose of each directory in this repository.
For detailed explanations of the files, follow the links. 




| directory | purpose |
|--------------------- | ------- |
| [Client](Client/README.md) | The component in this directory implements the client code for the | 
| [Common](Common/README.md) | The components in this directory represent the common ontology that the players and the gaming framework must share. | 
| [Lib](Lib/README.md) | functionality that should probably exist in Racket's libraries | 
| [Player](Player/README.md) | The component in this directory implements a Q player. | 
| [Referee](Referee/README.md) | The component in this directory implements the Q referee. | 
| [Remote](Remote/README.md) | The components in this directory implement the remote-proxy protocol | 
| [Server](Server/README.md) | The component in this directory implements the Q server. | 
