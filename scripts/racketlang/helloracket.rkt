#! /usr/local/bin/racket
#lang racket/base
;;; @todo2 Make these into `night.rkt'
(define concat string-append-immutable)
;;;
(* 2 5)
(concat "hi
b" "hmm")

(println "yummy!")
;;;
(require racket/gui/base)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))

; Make a button in the frame
(define btn-clicked
  (lambda (button event)
    (send msg set-label
          (string-append-immutable
           "Clicked; "
           (number->string (random 0 9))
           ))))
(new button% [parent frame]
     [label "Click Me"]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (btn-clicked button event))])

; Show the frame by calling its show method
(send frame show #t)
(send frame show #f)
;;;
