(in-package :cl)
(defpackage :lets-tests
  (:use :cl :lets :rtest))

(in-package :lets-tests)

(do-symbols (s :lets) (shadowing-import s))

(rem-all-tests)

(deftest :memq.0 
  (memq 'a '(a b c d))
  (a b c d))

(deftest :assq.0
  (assq 'a '((a . 1) (b . 2) (c . 3) (d . 4))) 
  (a . 1))

(deftest :putprop.0
  (progn
    (putprop 'foo 3 'lets-tests)
    (get 'foo 'lets-tests))
  3)

(deftest :comment.0
  (comment a b c d e f)
  comment)

(deftest :gf.0
  (with-output-to-string (out)
    (let ((*standard-output* out))
      (gf :dummy 42)))
  "42
")

#|(deftest :s-frag.0
  )|#

(deftest :s-new-var.0
  (let ((*gensym-counter* 0))
    (format nil "~A" (s-new-var 'g)))
  "G0")

#|(deftest :s-b)|#

(deftest :s-eq-car.0
  (s-eq-car '(a b c d)
                  'a)
  t)

(deftest :s-eq-car.1
  (s-eq-car '(a b c d)
                  'b)
  nil)

(deftest :s-variablep.0
  (s-variablep nil)
  nil)

(deftest :s-variablep.1
  (s-variablep "a")
  nil)

(deftest :s-variablep.2
  (s-variablep 'a)
  t)

(deftest :s-variablep.3
  (s-variablep t)
  nil)

(deftest :s-copyable-constant.0
  (s-copyable-constant 1)
  t)

(deftest :s-copyable-constant.1
  (s-copyable-constant "1")
  t)

(deftest :s-copyable-constant.2
  (s-copyable-constant t)
  (t nil))

(deftest :s-copyable-constant.3
  (s-copyable-constant nil)
  (nil))

(deftest :s-copyable-constant.4
  (s-copyable-constant '#'foo)
  t)

(deftest :s-copyable-constant.5
  (s-copyable-constant (list #\GREEK_SMALL_LETTER_LAMDA))
  t)

(deftest :s-copyable-constant.6
  (s-copyable-constant ''a)
  t)

;; '(&input &optional &rest &aux &output &flag)
(deftest :kind-order.0
  (kind-order '&input '&optional)
  (&optional &rest &aux &output &flag))

(deftest :kind-order.1
  (kind-order '&optional '&input)
  nil)

(deftest :s-kind.0
  (s-kind '(kind mode var info))
  kind)

(deftest :s-mode.0
  (s-mode '(kind mode var info))
  mode)

(deftest :s-var.0
  (s-var '(kind mode var info))
  var)

(deftest :s-info.0
  (s-info '(kind mode var info))
  info)

(deftest :kind-order.1
  (sort (reverse '((&input 1)
                 (&optional 2)
                 (&rest 3)
                 (&aux 4)
                 (&output 5)
                 (&flag 6)))
        #'kind-order :key #'car)
  ((&input 1) (&optional 2) (&rest 3) (&aux 4) (&output 5)
   (&flag 6)))

;; &inputと&outputしかないのでは?
(deftest :s-compress-arg-list.00
  (s-compress-arg-list '&input
                       '((&input &unitary foo nil)
                         (&input &unitary bar nil)
                         (&input &unitary baz nil)))
  (foo bar baz))

(deftest :s-compress-arg-list.01
  (s-compress-arg-list '&output
                       '((&output &unitary foo nil)
                         (&output &unitary bar nil)
                         (&output &unitary baz nil)))
  (foo bar baz))

(deftest :s-compress-arg-list.0
  (s-compress-arg-list '&input
                             '((&input mode var info)
                               (&optional mode var info)
                               (&rest mode var info)
                               (&aux mode var info)
                               (&output mode var info)
                               (&flag mode var info)))
  (mode var 
   &optional (var info)
   &rest var
   &aux var
   &output var
   &flag (var info)))

(deftest :s-compress-arg-list.1
  (s-compress-arg-list '&input
                       '((&input mode var info)
                         (&optional mode var info)
                         (&rest mode var info)
                         (&aux mode var info)
                         (&output mode var info)
                         (&flag mode var info)
                         ;; input1
                         (&input mode1 var1 info1)
                         ;; input2
                         (&input mode2 var3 info3)
                         ))
  (MODE VAR MODE1 VAR1 MODE2 VAR3 &OPTIONAL MODE (VAR INFO) &REST VAR &AUX VAR
        &OUTPUT VAR &FLAG (VAR INFO)))


(deftest :s-compress-arg-list.2
  (s-compress-arg-list '&output
                       '((&input mode var info)
                         (&optional mode var info)
                         (&rest mode var info)
                         (&aux mode var info)
                         (&output mode var info)
                         (&flag mode var info)
                         ;; output1
                         (&output mode1 var1 info1)
                         ;; output2
                         (&output mode2 var3 info3)
                         ))
  (&INPUT MODE VAR 
   &OPTIONAL (VAR INFO)
   &REST VAR
   &AUX VAR
   &OUTPUT VAR MODE1 VAR1 MODE2 VAR3
   &FLAG MODE (VAR INFO)))


(deftest :s-expand-arg-list.0
  (s-expand-arg-list '&input '(foo bar baz))
  ((&input &unitary foo nil)
   (&input &unitary bar nil)
   (&input &unitary baz nil)))


(deftest :s-expand-arg-list.1
  (s-expand-arg-list '&output '(foo bar baz))
  ((&output &unitary foo nil)
   (&output &unitary bar nil)
   (&output &unitary baz nil)))


;(Rlist (Elist 1 2 3 4))

#|'(s-args
  s-returns
  s-icode
  s-code1
  s-code2
  s-pcode
  s-ucode)|#

