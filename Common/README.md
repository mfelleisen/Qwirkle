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
        | - win            |
        | - name           |
        + ---------------- +
        
        + ----------- +          + ------- +         + ------ + 
        | states      |          | players |         | rules  |
        + ----------- +          + ------- +         + ------ +
        | set-players |          | create  |         | legal? |
        | shift*insert|          | new-goal|         | clegal |
        | rotate    ? |          +-------- +         +--------+
        | active-move |
        | can-move?   |
        | move        |
        | turn-over   |
        | finished?   |
        | score-game  |
        | fold-players|
        + ------------+
          
        ---------------------------------------------------------------------------------------------------
 BASIC CONCEPTS 

                                        + ------------ +
                                        | boards       |
                                        + ------------ +
                                        | shift&insert |
                                        | can-reach?   |
                                        + ------------ +

                                        + ------------ +
                                        | tiles        |
                                        + ------------ +
                                        | can-reach?   |
                                        | go-from?     |
                                        + ------------ +

                                        + ------------ +
                                        | directions   |
                                        + ------------ +
                                        | dir->Δ       |
                                        | exit->entry  |
                                        | UP DOWN      |
                                        | LEFT RIGHT   |
                                        + ------------ +

                                + ------------ +          + ------------ +
                                | coordinates  |          | geometry     |
                                + ------------ +          + ------------ +
                                | setup        |          | for graphics |
                                | Eu--distance |          + ------------ +
                                | coordinate+  |          
                                + ------------ +          
```

| file | purpose |
|--------------------- | ------- |
| [coordinates.rkt](coordinates.rkt) | data representation of map coordinates | 
| [game-state.rkt](game-state.rkt) | a data representation of the generic game-state knowledge | 
| [map.rkt](map.rkt) | data representation of a Q map | 
| [placement.rkt](placement.rkt) | a data representation for player actions, esp. placements | 
| [player-interface.rkt](player-interface.rkt) | a player interface that the referee can use to service players | 
| [state-of-player.rkt](state-of-player.rkt) | a data representation of the state of a Q player | 
| [tiles.rkt](tiles.rkt) | data representation of tiles, shapes, and colors | 
