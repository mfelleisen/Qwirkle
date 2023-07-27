#lang scribble/manual

@(require racket/runtime-path)
@(require racket/list)
@(require 2htdp/image)
@(require "shared.rkt")
@(require (prefix-in s: "../Common/tiles.rkt"))

@(define qwirkle-rules "https://en.m.wikipedia.org/wiki/Qwirkle")
@(define-runtime-path qwirkle.png "qwirkle.png")

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

The Q game is a tile-based game. Players place the tiles on an infinitely large
``table.'' The square shape of the tiles and the rules of the game induce the
notions of row and column on the evolving map formation.

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

The current scores of the players is public knowledge as is the order in which
they take turns. 

@; -----------------------------------------------------------------------------
@bold{Playing a Turn}

When granted a turn, a player may take one of the following three actions: 
@; -----------------------------------------------------------------------------
@itemlist[#:style 'ordered

  @item{@emph{pass};}
		  
  @item{@emph{exchange} all of its tiles for new ones, which is only possible if
  the referee still has enough tiles;

  The referee hands back the same number of tiles, randomly drawn from its own
  collection. 

  The returned tiles are added to the referee's collection of tiles, to be
  handed out in the future as needed.}

  @item{@emph{place} at least one tile or possibly all. 

  A placement of tiles must satisfy these conditions:
  @; ------------------------------------------------------------------
  @itemlist[
    @item{every tile placed must share a side with at least one placed tile;}

    @item{every tile placed must match the color or shape of any adjacent
    tiles along a line;}

    @item{all tiles placed during a turn must be in the same row or column,
    though not necessarily adjacent to each other.}
    ]
  The referee hands the player as many tiles as it placed, randomly drawn from
  its collection or all remaining tiles if the player placed more than the
  referee owns.}
  
]
@; -----------------------------------------------------------------------------
The referee eliminates any player that violates any rules during a turn.

@bold{Scoring a Turn}

A player that passes or exchanges its tiles receives no points. For placing
tiles, a player receives points as follows:

@itemlist[

@item{A player receives one point per tile placed.}

@item{A player receives one point per tile within a row or column that one (or
 more) of its newly placed tiles extends.}

@item{A player receives six bonus points for completing a Q, which is a sequence
 of tiles that contains all shapes or all colors.}

@item{A player also receives six bonus points for placing all tiles in its
 possession.} 
 
]
@; -----------------------------------------------------------------------------
The referee scores each turn and tracks the scores of all players. It shares the
scores of all players with the player to whom it grants a turn.

@; -----------------------------------------------------------------------------
@bold{Ending a Game} 

The game ends if one of the following condition holds:

@itemlist[

 @item{at the end of a round if all remaining players pass or replace their tiles;}

 @item{at the end of a turn if a player has placed all tiles in its
 possession; or}

 @item{there are no players left after a turn.}

]
@; -----------------------------------------------------------------------------
