# RpgLisp

## Description
This is simple toy lisp interpreter written in Haskell. I've always wanted to write my own programming language and this is my first fully functional interpreter. The objectives of one of my university project were to create simple language interpreter that allow to create text games similiar to Zork or Colossal Cave Adventure. I decided to create general purpose language instead of DSL and here it is.

## Code examples
``` lisp
    > (def x 2) ;; variables
    (Atom x)
    > (- (* x x x) (+ x 10))
    -4
    > (>= x 10)
    #f
    > (cats "simple" " toy " "lisp")
    "simple toy lisp"
    > (defn fib (x) (if (< x 2) 1 (+ (fib (- x 1)) (fib (- x 2)))))
    (Atom fib)
    > (fib 25)
    121393
    > '(+ 2 3) ;; quoting
    ((Atom +) 2 3)
    > (defn f (l) (let x l)) ;; local scope variables
    (Atom f)
    > (f 5)
    (Atom x)
    > x
    2
    > (map (fn (x) (+ x 10)) '(1 2 3 4 5)) ;; higher order functions and lambdas
    11
    > (cons (car (list 1 2 3)) (cdr (list 10 11 12))) ;; list functions
    (1 11 12)
    > (def d '()) ;; assoc lists
    (Atom d)
    > (set 'key1 'val1 d)
    (((Atom key1) (Atom val1)))
    > (def d2 (set 'key2 'val2 (set 'key1 'val1 d)))
    (Atom d2)
    > (get 'key2 d2)
    (Atom val2)
```
You can check out more examples in /example/ directory. You'll find full text adventure game there, but the story is written in Polish.
