(in-package :cl)
(defpackage :lets-tests
  (:use :cl :lets :rtest))

(in-package :lets-tests)

(rem-all-tests)

(deftest :memq.0 
  (lets::memq 'a '(a b c d))
  (a b c d))

(deftest :assq.0
  (lets::assq 'a '((a . 1) (b . 2) (c . 3) (d . 4))) 
  (a . 1))


