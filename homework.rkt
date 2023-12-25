#lang racket

;; this file exists to export names for the milestone specifications of the Sw Dev course 

(require (for-syntax syntax/parse))

(define-syntax (req->prov stx)
  (syntax-parse stx
    [(_ ((~literal prefix-in) x spec))
     #:with prov #'(all-from-out spec)
     #'(begin
         (require (prefix-in x spec))
         (provide prov))]
    [(_ spec)
     #:with prov #'(all-from-out spec)
     #'(begin
         (require spec)
         (provide prov))]))

;; the specs 
(req->prov Qwirkle/scribblings/spec)

;; code files 
(req->prov Qwirkle/Common/tiles)
(req->prov Qwirkle/Common/game-state)
(req->prov Qwirkle/Player/mechanics)
(req->prov Qwirkle/Referee/referee)
(req->prov Qwirkle/Referee/ref-state)
(req->prov Qwirkle/Server/server)
(req->prov (prefix-in client: Qwirkle/Client/client))

;; json specs 
(req->prov (submod Qwirkle/Common/tiles json))
(req->prov (submod Qwirkle/Common/coordinates json))
(req->prov (submod Qwirkle/Common/placement json))
(req->prov (submod Qwirkle/Common/game-state json))
(req->prov (submod Qwirkle/Common/state-of-player json))
(req->prov (submod Qwirkle/Player/mechanics json))
