## Interactive Player

Allow human player to make decision 

## Games: Player and Referee

```
referee              player
  | --- takeTurn ----> |

(scenario 1)
  | <== Placements === | 
  | 	 	       |

(scenario 2)
  | <== [setof Tile] = | 
  | -- [setof Tile] -> |
  | <== Void ========= | 
```

## Strategies

greedy per tile
dumb shape and color

## Scoring 

Revise Game State interface 

```
score  : Map Coordinate Placements -> Natural
```

## Game State 

```
type [GameState X] = [rgs Map X]
type RefState      = [GameState SoPlayer]
type InfoState     = [GameState Natural]
type SoPlayer      = [ps [Setof Tile] Natural]

type Placements = [Listof [placement Coordinate Tile]]
legal? : Map Coordinate Placements -> Option<Map>
```

## Map

```
type Map
type Candidate  = [candidate Coordinate Option<Tile> Option<Tile> Option<Tile> Option<Tile>]
```

```
start-map : Tile -> Map
;; this places the initial tile (avoids problem of selecting the starter and order)
;; the tile is at (0,0)
```

```
add-tile : Map Coordinate Tile -> Map 
;; extend the given tile to the map at the specified coordinate
;; if the coordinate specifies a place that neighbors at least one existing tile
```

```
render : Map -> Image
;; show map graphically 
```

```
candidates : Map Tile -> [Listof Candidate]
;; at which coordinates can the given tile be placed and what are its neighbors
```

## Coordinates

```
type Coordinate = [Pair Integer Integer]
```

- row/column order like computer graphics 
- going negative means UP for a ROW
- going positive means DOWN for a ROW
- going negative means LEFT for a COLUMN
- going positive means RIGHT for a COLUMN 

## Tiles: Shapes and Colors 

- symbolic
- render
