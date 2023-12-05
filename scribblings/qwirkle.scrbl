#lang scribble/manual

@(require racket/runtime-path)
@(require racket/list)
@(require 2htdp/image)
@(require "shared.rkt")
@(require (prefix-in s: "../Common/tiles.rkt"))

@(define qwirkle-rules "https://en.m.wikipedia.org/wiki/Qwirkle")
@(define-runtime-path qwirkle.png "qwirkle.png")

@(require "spec.rkt")


@; -----------------------------------------------------------------------------
@author{Matthias Felleisen}

@top[#:tag "Q"]{@red{The Q Game}}

@margin-note*{@(scale .5 [bitmap/file qwirkle.png])}

@; -----------------------------------------------------------------------------

The game is inspired by @link[qwirkle-rules]{@emph{Qwirkle}}.  the actual game
may help develop some intuition but the physical game and the implementation
differ.

@margin-note{Most of the time when we discuss ideas, the words ``referee'',
``player'', and so on refer to software components @emph{not} people.
To remind you of their inanimate nature, it is best to use ``its'' or
``it''---as in ``its game pieces'' or ``it's taking its turn.''.}

@; -----------------------------------------------------------------------------
@bold{Informal Overview}

The Q game is a tile-based game for @(~a MIN-PLAYERS) to @(~a MAX-PLAYERS)
players. These players place the tiles on an infinitely large ``table.'' The
square shape of the tiles and the rules of the game induce the notions of row
and column on the evolving map formation.

Players place tiles according to rather basic rules. Every placement yields a
certain number of points. When a player can use all of its tiles during a single
turn, the game ends. The game also ends when all game tiles have been placed or
all players pass during a round. 

The player with the highest total score wins. 

@; -----------------------------------------------------------------------------
@bold{Game Pieces}

Our version of the game comes with 36 kinds of tiles:
@; -----------------------------------------------------------------------------
@nested[#:style 'inset]{
 @tabular[ #:sep @hspace[3]
 	   #:row-properties '[bottom-border top]
  (s:render-all-shapes t)
 ]
}
@; -----------------------------------------------------------------------------
Altogether there are a total of 1080 tiles, 30 of each kind. 

@; -----------------------------------------------------------------------------
@bold{Setting up the Game}

The @emph{referee} hands each player six randomly chosen tiles. It then places
one tile on the ``table.'' Once this first tile is placed, the players take
turns in descending order of age, starting with the oldest.

The players do not tell each other which tiles they own.

The current scores of the players is public knowledge as is the number of
remaining tiles and the order in which they take turns.

@; -----------------------------------------------------------------------------
@bold{Playing a Turn}

When granted a turn, a player may take one of the following three actions: 
@; -----------------------------------------------------------------------------
@itemlist[#:style 'ordered

  @item{@emph{pass};}
		  
  @item{@emph{exchange} all of its tiles for new ones, which is only possible if
  the referee still has enough tiles;

  The referee hands back the same number of tiles, drawn from its own
  (randomly arranged) collection.

  The returned tiles are added to the end of the referee's collection
  of tiles, to be handed out in the future as needed.}

  @item{@emph{place} at least one tile or several tiles, which are then added to
  the map in sequential order.

  A placement of tiles must satisfy these conditions:
  @; ------------------------------------------------------------------
  @itemlist[

    @item{every tile placed must share a side with at least one tile on the map
    that it extends;}

    @item{@strike{every tile placed must match the colors of @purple{all} its immediate @purple{(up/down, left/right)} neighbors (if
    any) @strike{along a line (row, column)} or it must match @purple{all} the shapes @purple{of all its immediate neihgbors} @strike{along a line};}}

    @item{@purple{every tile added to the map must simultaneously match both of
    its immediate neighbors along its row and column, separately, in terms of
    either shape or color;}}

    @item{all tiles placed during a turn must be in the same row or column,
    though not necessarily adjacent to each other.}
    ]
  @; --------------------------------------------------------------------------
  The referee hands the player as many tiles as it placed, drawn from its
  randomly arranged collection or all remaining tiles if the player placed more
  than the referee has left.}
  
]
@; -----------------------------------------------------------------------------
The referee eliminates any player that violates any rules during a turn. 
@purple{The tiles in the hand of the eliminated player get appended to the
referee's tiles.} 

@bold{Scoring a Turn} A player that passes or exchanges its tiles receives no
points. 

If the player requests a placement, the referee checks whether it is legal and
constructs the extended map. Based on this new map, the referee assigns points
for a turn as follows: 
@; -----------------------------------------------------------------------------
@itemlist[

@item{A player receives one point per tile placed.}

@item{A player receives one point per tile in a contiguous sequence of
 @purple{tiles} (in a row or column) that @purple{contains} at least one of its
 newly placed tiles @strike{@emph{extends}}.}

@item{A player receives @(~a [Q-BONUS]) bonus points for
 @emph{completing} a Q, which is a contiguous sequence of tiles that
 contains all shapes or all colors @purple{and nothing else}.}

@item{A player also receives @(~a [FINISH-BONUS]) bonus points for
 placing all tiles in its possession.} 
 
]
@; -----------------------------------------------------------------------------
The referee keeps track of the scores on a per turn basis and shares the scores
of all players with the player to whom it grants a turn.

@; -----------------------------------------------------------------------------
@bold{Ending a Game} 

The game ends if one of the following condition holds:

@itemlist[

 @item{at the end of a round if all remaining players pass or replace their tiles;}

 @item{at the end of a turn if a player has placed all tiles in its possession; or}

 @item{there are no players left after a turn.}

]
@; -----------------------------------------------------------------------------
Once a player has requested the placement of tiles, the current round of turns
cannot end the game even if this player drops out (for whatever reason) and even
if all other players pass or request exchanges. 

@(require (submod Qwirkle/Common/map examples))
@(require Qwirkle/Common/map)
@(require (prefix-in t: Qwirkle/Common/tiles))


@; -----------------------------------------------------------------------------
@bold{Sample Placements}

@(define (fake-test in tile place out)
 @nested[#:style 'inset]{
 @tt{(check-equal?
      (fits @(scale .5 [render-map in]) @tt{@place} @(scale .5 [t:render-tile @tile]))
      @scale[.5 (render-map out)])}})

@(define (fake-false in tile place) 
 @nested[#:style 'inset]{
 @tt{(check-equal?
      (fits @(scale .5 [render-map in]) @tt{@place} @(scale .5 [t:render-tile @tile]))
      #false)}})

@fake-test[special-map (s:tile 'star 'green) "P1" special-map+green-star-at--3-1]

@fake-false[special-map (s:tile 'star 'purple) "P1"]


@fake-test[special-map+purple-star-at-1-2 (s:tile 'star 'green) "P2" special-map+purple-star-at-1-2++]

@fake-false[special-map+purple-star-at-1-2 (s:tile 'star 'blue) "P2"]

@;(define p2 (placement (coordinate -4 1) (s:tile 'circle 'red)))
@;fake-false[(add-tile special-map+green-circle-at--2-2 p2) (s:tile 'circle 'red) "P2"]
