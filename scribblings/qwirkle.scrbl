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

@top[#:tag "qwirkle"]{@red{The Game: Q}}

@margin-note*{@(scale .5 [bitmap/file qwirkle.png])}

@; -----------------------------------------------------------------------------

The chosen game is inspired by @link[qwirkle-rules]{@emph{Qwirkle}}. Playing
the actual game may help develop some intuition but the physical game and the 
implementation differ. 

@margin-note{Most of the time when we discuss ideas, the words ``referee'',
``player'', and so on refer to software components @emph{not} people.
To remind you of their inanimate nature, it is best to use ``its'' or
``it''---as in ``its game pieces'' or ``it's taking its turn.''.}

@; -----------------------------------------------------------------------------
@bold{Informal Overview}

The Q game is a tile-based game without a board. Instead players place the tiles
on an infinitely large ``table.'' Still, the square shape of the tiles and the
rules of the game induce the notions of row and column on the evolving formation
of tiles.

Players place these tiles according to basic rules on the table. Every placement
yields a certain number of points. When a player can use all of its tiles during
a single turn, the game ends. The game also ends when all game tiles have been
placed or all players pass during a turn. 

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
There are a total of 1080 tiles, 30 of each kind. This is one possible rendering
of these tile shapes; your own style may vary as long as the shape and color are
distinct and recognizable as one of the above categories. 

@; -----------------------------------------------------------------------------
@bold{Setting up the Game}

The @emph{referee} hands each player six randomly chosen tiles. It then places
one tile on the ``table.'' Once this first tile is placed, the players take
turns in descending order of age, starting with the oldest.

The players do not tell each other which tiles they own. 

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

  @item{@emph{place} at least one tile.

  A placement must satisfy these conditions:

  @itemlist[
    @item{every place tile must share a side with an already placed tile;}

    @item{the first tile placed must match the color of the shape of an adjacent
    tile;}

    @item{all tiles placed during a turn must be in the same row or column,
    though not necessarily adjacent to each other.}
    ]
  The referee hands the player as many tiles as it placed, randomly drawn from
 its collection.}
  
]
@; -----------------------------------------------------------------------------
The referee eliminates any player that violates any rules during a turn.

@bold{Scoring a Turn}

A player that passes or exchanges its tiles receives no points. For placing
tiles, a player receives points as follows:

@itemlist[

@item{A player receives one point per tile placed with a row or column,
 including for existing tiles within this row or column, respectively.}

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

 @item{at the end of a round if all remaining players pass;}

 @item{at the end of a turn if a player has placed all tiles in its
 possession and the referee does not have any more tiles to hand out; or}

 @item{if there are no players left.}

]
@; -----------------------------------------------------------------------------

@; -----------------------------------------------------------------------------
@bold{Warning} Like with all software projects, the details of the game may
change as experimentation with the prototypes suggests improvements. 
