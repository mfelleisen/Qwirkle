## Server 

The component in this directory implements the Q server. 

The server signs up clients over TCP that connect on behalf of an AI
player and runs a Q game. 

A server configuration file has the following shape:

```
(define options
  (list
    PORT            ;; on which port the clients may connect 
    SERVER-WAIT     ;; the length [in s] of the sign-up period 
    SERVER-TRIES    ;; how many serving periods it waits 
    WAIT-FOR-SIGNUP ;; the number of s a client has to deliver a name
    QUIET           ;; suppress error message? 
    REF-SPEC))      ;; referee-specific configuration 
```

In addition to a configuratoon, a server requires two other mandatory
arguments:

- an `ordering`, which determines the order in which players get to go
- a list of house `players`, locally running players

plus one optional keyword argument (`#:result`), which accepts the
list of players and optionally displays them (or processes them in
other ways). The default is `void`.

Every client that connects gets represented by a remote-proxy player,
which satisfies the exact same interface as an ordinary player. Once
enough clients have signed up, the server delegates the task of
running the game to a referee. The results---the winning players and
those that misbehaved---are handed to the `#:result` function.

### The Sign-Up Protocol

The following sequence diagram sketches how clients sign up, how the
server wraps them in proxies, and how it eventually delegates to the
referee. 

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

### Running

The `xserver` script ... 

### Organization 

| file | purpose |
|--------------------- | ------- |
| [server.rkt](server.rkt) | a server that signs up players over TCP and runs a game | 
| [player.rkt](player.rkt) | this remote player implements the same interface as the player but conveys its arguments | 
