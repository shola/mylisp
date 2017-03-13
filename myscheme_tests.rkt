#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "myscheme.rkt")


(define-test-suite lat?-suite
  (test-false "Argument must be a list with non-list values only"
              (lat? '(this (will break))))
  (test-true "Argument was a list of atoms" (lat? '(good "stuff")))
  (test-true "Empty lists are atomic by definition"
             (lat? '())))

(define-test-suite member?-suite
  (test-false "Argument must be a list with non-list values only"
              (member?  "something" '(this (will break))))
  (test-true "Member found in the list argument"
             (member? "stuff" '(good "stuff") ))
  (test-false "Empty lists contain no members"
             (member? "nada" '() )))

(define-test-suite rember-suite
  (test-equal? "No match found"
               (rember "something" '(what will happen next) )
               '(what will happen next))
  (test-equal? "Match found"
               (rember  "happen" '(what will "happen" next))
               '(what will next))
  (test-equal? "Remove all occurences from a list"
               (multirember "a" '("b" "a" "c" "e" "a" "a" "z"))
               '("b" "c" "e" "z")))

(define-test-suite firsts-suite
  (test-equal? "Test firsts with a list of lists"
               (firsts '((apple peach pumpkin)
                         (plum pear cherry)
                         (grape raisin pea)
                         (bean carrot eggplant)))
               '(apple plum grape bean))
    (test-equal? "Test firsts list of list of lists"
               (firsts '(((five plums) four)
                         (eleven green oranges)
                         ((no) more)))
               '((five plums) eleven (no))))

(define-test-suite insert-suite
  (test-equal? "Test insertR with a list of s-exp"
               (insertR 2017 2016 '(2014 2015 2016))
               '(2014 2015 2016 2017))
  (test-equal? "Test insertL with a list of s-exp"
               (insertL 2016 2017 '(2014 2015 2017))
               '(2014 2015 2016 2017))
  (test-equal? "Test multiinsertR with a list of s-exp"
               (multiinsertR 11 2 '(1 2 3 4 2 5 6 2))
               '(1 2 11 3 4 2 11 5 6 2 11))
   (test-equal? "Test multiinsertL with a list of s-exp"
               (multiinsertL 11 2 '(1 2 3 4 2 5 6 2))
               '(1 11 2 3 4 11 2 5 6 11 2)))

(define-test-suite subst-suite
  (test-equal? "Test subst with a list of s-exp"
               (subst 2016 2017 '(2014 2015 2017 2017))
               '(2014 2015 2016 2017)))

(define-test-suite subst2-suite
  (test-equal? "Test subst2 with a list of s-exp"
               (subst2 2020 2017 2016 '(2014 2015 2016 2017))
               '(2014 2015 2020 2017)))

(define-test-suite arithmetic-suite
  (test-equal? "Test adding 2 numbers"
               (myplus 11 20) 31)
  (test-equal? "Test subtracting 2 numbers"
               (mysub 11 2) 9)
  (test-equal? "Test subtracting 2 numbers"
               (mysub 1 20) 19))

(run-tests lat?-suite)
(run-tests member?-suite)
(run-tests rember-suite)
(run-tests firsts-suite)
(run-tests insert-suite)
(run-tests subst-suite)
(run-tests subst2-suite)
(run-tests arithmetic-suite)