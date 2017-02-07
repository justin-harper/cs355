;; Justin Harper
;; WSUID: 10696738
;; Scheme Assignment

;nth

;Define a function that returns the nth element of a list (0-based indexing). Assume that the
;index, n, is at least 0 and smaller than the length of the list. (When I say "assume that ..." 
;it means that you may write code that fails in arbitrary ways if the assumption does not hold.
;In this case, it means that you do not have to worry about error cases such as when n is 
;negative or too large.)

;The trick with problems like this is to reduce both the size of the list and the size of the 
;integer argument in recursive calls. The 3rd element of a list is the 2nd element of the cdr 
;of the list.

;>  (nth '(1 2 3 4) 1)
;2

(define (nth L l) (list-ref L l))

;((list-ref L N ) returns the (N+1)st element of L. So (list-ref L 0) returns the first element of L. (from interwebs)


;; For nth, it should follow the instructions to use basic built-in functions. See: http://www.eecs.wsu.edu/~hauser/cs355/schemeHomework.html

;; -5.

;; General rules
;; Unless directed otherwise, you should implement your functions using recursive definitions that you build up from the basic built-in functions such as cons, car, cdr, list, etc.

;; Don't use set! and don't define anything except functions. 

; repl

; Define a function repl
; (define (repl l i v) ... )
; that returns a (new) list which is the same as l except that the ith element is v. Again, assume that the index i is at least 0 and smaller
; than the length of the list.
; >  (repl '(1 2 3 4) 1 7)    
; (1 7 3 4)

(define (repl L l x)
    (if (null? L) L
        (cons (if(zero? l) x (car L))
        (repl (cdr L) (- l 1) x))))


; range

; Define a LISP function range like the range function in python:
; (define (range min max) ... )
; that return a list of integers (min min+1 ... max-1). If min ≥ max return the empty list.
; >  (range 4 6)
; (4 5)

(define (range min max)
    (if (>= min max) '()
    (cons min ( range (+ min 1) max))))


; merge2

; Define a function merge2 that merges two lists of integers, each already in ascending order, into a new list that is also in ascending order.
; The length of the new list is the sum of the lengths of the original lists.
; (define (merge2 l1 l2) ...)
; For example
; > (merge2 '(2 4 6) '(1 4 5))
; (1 2 4 4 5 6)


(define (merge2 list1 list2)
    (if (null? list1) list2
        (if (null? list2) list1
            (if (>= (car list1) (car list2))
                (cons (car list2) (merge2 list1 (cdr list2)))
                (cons (car list1) (merge2 (cdr list1) list2))))))

; fold

; Just use the definition given in class. You'll need it for the function below.

(define (fold fcom base X)
    (cond ((null? X) base)
        (#t (fcom (car X) (fold fcom base (cdr X))))))

; mergeN

; Using merge2 and the fold function defined above you can now define mergeN which takes a list of lists, each already in ascending order,
; and returns a new list containing all of the elements in ascending order. For example,
; > (mergeN '())
; ()
; > (mergeN '((2 4 6) (1 4 5)))
; (1 2 4 4 5 6)
; > (mergeN '((2 4 5) (1 4 6) (3 7 9)))
; (1 2 3 4 4 5 6 7 9)
; Note that this requires making sure that your understanding of fold encompasses seeing that it can return a list, not just a simple value;
; but the solution is, in fact, a very simple use of fold once you choose the right value for the base case corresponding to the empty list as input.
; In a comment, discuss the question of how many cons operations your function uses to produce its result, in terms of the sizes of the input lists.
; By "discuss the question" I mean not only to give an answer but also to give an argument as to how the answer is arrived at. For this problem,
; I suggest looking first at how many cons operations are used by merge2 for lists of length len1 and len2. If mergeN is used for a single list,
; what is the answer? For 2 lists? For n lists?

(define (mergeN list1) (fold merge2 '() list1'))

;; A typo in "list')", where should not be ' between list1 and )



;; Missing the discuss question of how many cons in terms of the size of input lists
;; See instructions.
;; -5


; unzip

; In class we defined a function named zip that takes two lists as its arguments and produces a list of pairs as its output. For this problem,
; define the inverse of zip, namely unzip, that takes a list of two-element lists as input and produces a list of two lists as 
; output: (unzip '((1 2) (3 4) (5 6))) is ((1 3 5) (2 4 6)). What should (unzip '()) be?
; Challenge (or a hint I suppose): can you implement unzip using fold by choosing the correct combining function and base case?


(define (unzip X) (if (null? X) X (apply map list X)))

; map is one of the CommonHigherOrderFunctions which transforms a list by applying a function to each of its elements. Its return value is the transformed list.
; There are many variations on the map function, the one described here is called map in Scheme, and mapcar in most Lisps. 
; apply

; general form:
; (apply function argument-list)
; This evalutes to the result of passing arguments in argument-list (all at once) to function. (Note that this entails a single call to the function.)

; (from interwebs)



;; Good work overall.
;; 90