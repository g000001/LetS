;-*- Mode:LISP; Package: (LETS GLOBAL 1000) ; base:10.; -*-

; The macros in this file are described in detail in MIT/AIM-680.  Read that
;before you look at anything in this file.  All comments suggestions etc.
;should be sent to DICK@AI.
; Every fn which is internal to this file begins with S-.
;all internal functions and symbols are in the package LETS: on lispm.
; NOTE THAT THERE IS NOTHING IN THIS FILE WHICH IS NEEDED IN ORDER FOR
;COMPILED CODE TO RUN SO YOU ONLY HAVE TO LOAD IT FOR INTERPRETATION
;AND COMPILATION.

;The only functions intended to be used by users are: DEFUNS, LETS*,
;LETS, DONE, and the library sequence functions which are defined in
;the file LETSLB.  These functions are globalized on lispm.

(in-package :lets)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar S-token-params () )
  (defvar S-token-args () ) )

(defun-compile-time GF (&rest args)
  (format T "~{~A~^ ~}~%" (cdr args)))

;;This is here for debugging only

(defun-compile-time S-frag (&rest frag)
  (GF "{'('*_(1<*,>)A(1<*,>)+-6[<A(1<*->)>]')'}"
      's-frag (s-compress-arg-list '&input (s-args frag))
      (s-compress-arg-list '&output (s-returns frag)) (cdddr frag)))

(putprop 's-frag 'Gformat 'defun)

(defun-compile-time s-debug ()
  (eval '(progn ;to make lispm happy
          (defun r fexpr (form)
                 (cond (form (setq r (car form))))
                 (prog (f)
                       (setq f (copy-tree r))
                    L (cond ((not (equal (cons (car f) (cdr f))
                                         (setq f (macroexpand-1 f))))
                             (cond ((s-eq-car f 'lets*) (go L)))
                             (pl f)
                             (cond ((Y-or-N-p "continue") (go L))))
                            (T (return (eval f))))))
          #|(defun (S-frag :Gformat) (frag)
          (GF "{'('*_(1<*,>)A(1<*,>)+-6[<A(1<*->)>]')'}"
          's-frag (s-compress-arg-list '&input (s-args frag))
          (s-compress-arg-list '&output (s-returns frag)) (cdddr frag)))|#

          (defun (defunS :Gformat) (expr)
            (GF "(2*_*_(1<*,>)<A*>)" expr))
          (defun (s-defunS :Gformat) (expr)
            (GF "(2*_*_(1<*,>)<A*>)" expr))
          (defun (letS :Gformat) (expr)
            (GF "(2*_(1<*,>)<A*>)" expr))
          (defun (letS* :Gformat) (expr)
            (GF "(2*_(1<*,>)<A*>)" expr))
          (defun (s-lets :Gformat) (expr)
            (GF "(2*_(1<*,>)<A*>)" expr)))))



;This makes up a unique name.  The key requirement is that it must not
;clash with anything at all.  These names are eliminated if possible
;when we do simplifications.  (Eventually will be just gensym.)

(defun-compile-time s-new-var (root)
  (gensym (string root)))

;This is called to signal internal errors.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar S-ERROR nil "holds debugging info when error hit."))

(defun-compile-time S-B (&rest values)
  (setq S-ERROR `("Internal LetS BUG:" ., values))
  (let (*print-level* *print-length* *print-lines*)
    (error "~A" S-ERROR)))

;Just makes it easy to robustly test the car.

(defmacro s-eq-car (item atom)
  (cond ((symbolp item) `(and (consp ,item) (eq (car ,item) ,atom)))
        (T (let* ((s-item (gensym)))
             `(let* ((,s-item ,item)) (s-eq-car ,s-item ,atom))))))

;;This tests whether a thing is a variable name.
(defun-compile-time s-variablep (thing)
  (and thing (symbolp thing) (not (eq thing T))))

(defun-compile-time s-copyable-constant (thing)
  (or (numberp thing)
      (stringp thing)
      (memq thing '(T NIL))
      (s-eq-car thing 'function)
      (and (s-eq-car thing 'quote) (symbolp (cadr thing)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Here are basic constructor/deconstructors for the key internal form:
  ;; (S-frag args returns icode code1 code2 pcode ucode)

  (defstruct (s-frag (:type list)
                     (:conc-name :s-)
                     :named)
    args
    returns
    icode
    code1
    code2
    pcode
    ucode) )

(defmacro s-make-frag (a r i c1 c2 p u)
  `(s-check (make-s-frag :args ,a :returns ,r :icode ,i
                         :code1 ,c1 :code2 ,c2 :pcode ,p :ucode ,u)))

(defmacro s-frag? (thing)
  `(s-frag-p ,thing))

;A basic part of every fragment is its arg list.  This is a list of
;quadruples [kind mode var info]  where
;KIND is one of &INPUT &OPTIONAL &REST &AUX for inputs
;    and one of &OUTPUT &FLAG for outputs
;MODE is one of &SEQUENCE &UNITARY &END-UNITARY
;     end-unitary values are only available at the end of the loop.
;VAR is a variable (gensymed and unique in the fragment)
;INFO is the optional value for &optional and the list of controlled
;    vars for &flag.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (s-arg (:type list)
                    (:conc-name :s-))
    kind
    mode
    var
    info) )

(defmacro s-make-arg (k m v i)
  `(make-s-arg :kind ,k :mode ,m :var ,v :info ,i))



;these fns convert arg lists to and from compressed form.

(defun-compile-time s-compress-arg-list (in-out arg-list)
  (setq arg-list (sort arg-list #'kind-order :key #'car))
  (do ((kind in-out) (mode '&unitary) (result)
       (list arg-list (cdr list))) ((null list) (nreverse result))
    (let* ((this-kind (s-kind (car list)))
           (this-mode (s-mode (car list)))
           (var (s-var (car list)))
           (info (s-info (car list))))
      (cond ((not (eq kind this-kind))
             (setq kind this-kind)
             (push kind result)))
      (cond ((not (eq mode this-mode))
             (setq mode this-mode)
             (push mode result)))
      (cond ((memq kind '(&optional &flag))
             (push (list var info) result))
            (T (push var result))))))

(defun-compile-time kind-order (x y)
  (memq y (cdr (memq x '(&input &optional &rest &aux &output &flag)))))

(defun-compile-time s-expand-arg-list (in-out compressed-arg-list)
  (do ((kind in-out)
       (mode '&unitary)
       (result)
       (list compressed-arg-list (cdr list)))
      ((null list) (reverse result))
    (cond ((memq (car list) '(&optional &rest &aux &flag))
           (setq kind (car list)))
          ((memq (car list) '(&unitary &sequence &end-unitary))
           (setq mode (car list)))
          ((memq kind '(&optional &flag))
           (push (s-make-arg kind mode (caar list) (cadar list)) result))
          (T (push (s-make-arg kind mode (car list) nil) result)))))


;This macro supports a usful brand of looping on lists.  It is much like
;mapcar, except that the way the body is specified is different.  It iterates
;over a list (which must have a nil final cdr) and puts successive elements of
;it in the variable ITEM.  It then conses up the results of the body
;except that results of NIL are ignored.  Also you can push additional things
;onto the variable S-RESULT in order to include them in the output.
;Finally you can trigger an extraordinary exit by setting the variable
;S-CONTINUE to one of the following two values.
;1- COPY-REST the rest of the input arglist is copied to the output and
;    processing stops.
;2- DONE-NOW processing immediately stops with no additional output.
; At anytime you can do a return which stops the looping, and
;returns what you say.

(defmacro s-mapcar (list &body body)
  `(prog (s-list s-result s-continue)
         (setq s-list ,list)
    L    (cond ((null s-list) (setq s-continue 'done-now)))
         (case s-continue
           (copy-rest (let ((res (revappend s-result s-list)))
                        (return res)))
           (done-now (let ((res (reverse s-result)))
                       (return res))))
         (let* ((item (car s-list))
                (value (progn ,@body)))
           (cond (value (push value s-result))))
         (setq s-list (cdr s-list))
         (go L)))

; This is a hairy macro (based on s-mapcar) which is used a lot in the
;functions below.  It runs through an expnaded frag arg list and executes its
;body on each separate arg.  Each time when the body is executed, the
;variables KIND MODE VAR INFO are bound to the appropriate features of
;the arg.
; The parameter RETURN controls what happens to the results created by
;the body.  It has one of two values.
;1- ARGLIST the result is a new arglist.  If the the result is NIL
;    then nothing is put in the output.  Other wise the values of the variables
;    KIND MODE VAR INFO are used to specify the output.  Note that if you
;    don't change these vars then the arg from the input is copied to the
;    output.  To change it alter the appropriate var(s).
;2- OTHER the results of the body are themselves consed up into a list
;   except that NIL values are skipped.

(defmacro s-process-args (list result &body body)
  `(s-mapcar ,list
     (let (kind mode var info)
       (setq kind (s-kind item)
             mode (s-mode item)
             var (s-var item)
             info (s-info item))
       ,(case result
          (arglist `(cond ((progn ., body) (s-make-arg kind mode var info))))
          (other `(progn ,@body))))))


;the following are some things that we assume about frags, Note that
;the library frags at the end have to obay these religiously!
;1- the args are unique in the body.  Note that an output can have the
;   same name as a (non &rest) input as long as it is just passing along
;   its value (eg filters truncators).  They will always be renamed together.
;2- every frag has exactly one return value
;3- every &sequence input variable must be read at least once in the
;   code1-code2 unless both are nil.  If you have nothing useful to do with
;   it (eg in rcount) you can just say (COMMENT (READING VAR)) to indicate
;   where it is logically being used.  We need to know this for filters in
;   order to know whether the code1-code2 needs to be conditionalized.
;4- when two frags are combined, it is OK to rename the output var of
;   one to be the same as the input var of the other.  To insure this and
;   to minimize the number of variables needed we inforce the following zones
;   of exclusion.
; A- &unitary outputs can be arbitrarily referenced by the creater in icode
;    and cannot be referenced by him in code1-ucode.  &unitary inputs can be
;    arbitrarily referenced by the user in icode-ucode.
; B- &end-unitary outputs can be arbitrarily referenced by the creater in
;    icode-pcode and cannot be referenced in the ucode.  &end-unitary inputs
;    can be referenced arbitrarily but only in the pcode-ucode.
; C- &sequence outputs can be arbitrarily referenced in the icode-code1.
;    But note that their values are not guarranted to be
;    preserved between iterations.  They cannot be referenced in the
;    code2-ucode.  &sequence inputs can be arbitrarily referenced in
;    code1-ucode.
;  Note that the zones of exclusion work just fine as long as each output is
;  used in only 1 place.  If it is used in two places that both modify it,
;  there could be a conflict.  But note that a thing can only be used in more
;  than 1 place if it is a variable.  To protect against problems, if an input
;  is a variable and the input is setqed, then an additional setq is
;  used to protect the input variable.
;  [Side-effects could still cause problems, and the user must
;  guard against destroying some other fragment's internal state.]

(defun-compile-time s-check (frag)
  (let (vars first-output m)
    (or (= (length frag) 8) (push "wrong number of parts" m))
    (s-process-args (s-args frag) other
      (or (memq mode '(&sequence &unitary &end-unitary))
          (push (cons "bad mode" var) m))
      (or (s-variablep var) (push (cons "bad variable" var) m))
      (or (not (memq var vars)) (push (cons "repeated variable" var) m))
      (push var vars)
      (or (memq kind '(&input &optional &rest &aux))
          (push (cons "bad kind" var) m))
      (or (case kind
            (&optional T)
            (T (null info)))
          (push (cons "bad info" var) m))
      (cond ((memq kind '(&input &optional &rest))
             (case mode
               (&unitary T)
               (&end-unitary
                (or (not (s-referencesp
                          (append (s-icode frag) (s-code1 frag) (s-code2 frag))
                          (list var)))
                    (push (cons "&end-unitary out used icode-code2" var) m)))
               (&sequence
                (or (not (s-referencesp (s-icode frag) (list var)))
                    (push (cons "&sequence in used in icode" var) m))
                (or (and (null (s-code1 frag)) (null (s-code2 frag)))
                    (s-readsp (s-code1 frag) (list var))
                    (s-readsp (s-code2 frag) (list var))
                    (push (cons "seq var not read" var) m)))))))
    (s-process-args (s-returns frag) other
      (or (memq mode '(&sequence &unitary &end-unitary))
          (push (cons "bad mode" var) m))
      (or (s-variablep var) (push (cons "bad variable" var) m))
      (let ((ret var))
        (s-process-args (s-args frag) other
          (and (eq var ret) (not (memq kind '(&input &optional &aux)))
               (push (cons "bad shared output" var) m))))
      (case kind
        (&output (or (null first-output) (push "too many returns" m))
                 (setq first-output T))
        (&flag)
        (T (push (cons "bad kind" var) m)))
      (or (cond ((eq kind '&flag) (consp info))
                (T (null info)))
          (push (cons "bad info" var) m))
      (or (eq kind '&flag)
          (case mode
            (&unitary
             (or (not (s-referencesp
                       (append (s-code1 frag) (s-code2 frag)
                               (s-pcode frag) (s-ucode frag))
                       (list var)))
                 (push (cons "&unitary out used in code1-ucode" var) m)))
            (&sequence
             (or (not (s-referencesp
                       (append (s-code2 frag) (s-pcode frag) (s-ucode frag))
                       (list var)))
                 (push (cons "&sequence out used in code2-ucode" var) m)))
            (&end-unitary
             (or (not (s-referencesp (s-ucode frag) (list var)))
                 (push (cons "&end-unitary out used in ucode" var) m))))))
    (cond (m (s-b "malformed fragment" m frag))
          (T frag))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;             here are the actual macro definitions                     ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Note &body must be in right place to make zwei happy.

(defmacro defunS (name args &body body)
  "Used to define new sequence functions"
  (let (doc dcl)
    (cond ((stringp (car body))
           (setq doc (pop body))))
    (cond ((s-eq-car (car body) 'declare)
           (setq dcl (pop body)))
          (T nil))
    (let* ((ret (s-parse-it `(defunS ,name ,args ., body) 'defunS args body))
           (new-args (car ret))
           (new-body (cdr ret)))
      `(s-defuns ,name ,new-args ,doc ,dcl
          ., new-body))))

;This exists so that the user can do a macroexpand-1 and see the
;results after parsing.

(defmacro s-defuns (name args doc dcl &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (s-defmacro ,name ,doc ,dcl
       ',(s-normalize (s-combine args body)))))

;This is the main entry point.  Note that isolated loop expressions
;are coerced by wrapping letS* around them.

(defmacro letS* (args &body body)
  "Used to define a loop expression"
  (let* ((ret (s-parse-it `(letS* ,args ,@body) 'letS*
                          `(&aux &sequence ,@args) body))
         (new-args (car ret))
         (new-body (cdr ret)))
    `(s-lets (,@(cddr new-args)) ,@new-body)))

;This exists so that the user can do a macroexpand-1 and see the
;results of parsing.

(defmacro s-lets (args &body body)
  (s-make-loop (s-combine (list* '&aux '&sequence args) body)))

;For historical compatability.

(defmacro letS (variable-value-pairs &body body)
 "Used to define a loop expression"
  `(lets* ,variable-value-pairs ., body))

;This makes a normal form fragment out of a combined fragment by:
;Making the flags refer only to the outputs.  Making sure each
;sequence input is actually read.  Note we don't have to gensym
;the vars anymore because this happens before parsing.

(defun-compile-time s-normalize (frag)
  (setq frag (s-variable-rename (s-rename-alist frag) frag))
  (let* ((vars (s-process-args (s-returns frag) other
                 (cond ((eq kind '&output) var)))))
    (setf (s-returns frag)
          (s-process-args (s-returns frag) arglist
            (cond ((eq kind '&flag)
                   (setq info (s-mapcar info
                                (cond ((memq item vars) item))))))
            T)))
  (s-process-args (s-args frag) other
    (cond ((and (eq mode '&sequence) (memq kind '(&input &optional))
                (not (s-readsp (s-code1 frag) (list var)))
                (not (s-readsp (s-code2 frag) (list var))))
           (push `(comment (reading ,var)) (s-code1 frag)))))
  (s-check frag))

;This has two completely different behaviors based on whether it is
;encountered in the outside world, or is being macroexpanded after
;parsing has been completed.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar S-SEQUENCE-VARS nil "the sequence vars in a letS")
  (defvar S-INSIDE-LETS nil "internal flag used by letS") )

(defmacro s-frag-for (symbol)
  `(and (symbolp ,symbol) (get ,symbol 's-frag)))

(defmacro s-defmacro (name doc dcl frag)
  (declare (ignore dcl doc))
  `(progn
     (putprop ',name ,frag 's-frag)
     (defmacro ,name (&body body)
       ;; ,@(cond (dcl (list dcl)))
       ;; ,@(cond (doc (list doc)))
       (let ((call (cons ',name body)))
         (cond (S-INSIDE-LETS (s-frag-apply call))
               (T (list 'lets* nil call)))))))

(defun-compile-time s-combine (compressed-args body)
  (let* ((S-INSIDE-LETS T)
	 (args (s-expand-arg-list '&input compressed-args))
	 (S-sequence-vars (s-process-args args other
			    (cond ((eq mode '&sequence) var))))
	 (clean-args (s-process-args args arglist
		       (cond ((eq kind '&aux) (setq mode '&unitary)))
		       T))
	 (frag (macroexpand (car body))))
    (s-mapcar (cdr body)
      (s-auxify-ret frag)
      (setq frag (s-merge frag (macroexpand item))))
    (setf (s-args frag) (append clean-args (s-args frag)))
    frag))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;                               Parsing                                 ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Here are some variables used during parsing

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar S-FORM nil "holds the top form which started macro processing")
  (defvar S-PARSE nil "the most recent loop parse")
  (defvar S-USER-RENAMES nil "The mosr recent set of user var renamings") )

;This makes it easier to print error messages.  Note that all user
;error messages are generated during this phase.  During parsing the
;system maintains S-FORM containing the outermost form which triggered
;the operation of the macros.

(defun-compile-time S-E (&rest values)
  (setq S-ERROR values)
  (let (*print-level* *print-length* *print-lines*)
    (error "~A" (format nil "~{~<~A~; ~>~}in letS form: ~%~A" S-ERROR S-FORM))))

;This does the actual parsing of an arglist and body.  It is called by
;both letS* and defunS.  Note that all user variables are renamed into
;uninterned symbols with the same pname.  This is so that we don't
;have to worry about name clashes with function names after this
;point.  We save the renaming so that we can undo it at the end if we
;construct an actual loop.  The user can depend on his names being
;accessable during debugging and the like.  [We reinitialize the
;variable counter just to make things easier to read.]

(defun-compile-time s-parse-it (S-form form arglist body)
  (setq *gensym-counter* 0)
  (cond ((null body) (s-e "no body")))
  (let* ((ret (s-args-parse arglist))
         (argl (car ret))
         (extra-body (cadr ret)))
    (setq S-user-renames (s-process-args argl other
                                         (cons var (copy-symbol var nil))))
    (setq argl (sublis s-user-renames argl))
    (setq body (cdr (s-variable-rename s-user-renames
                                       `(progn ,@extra-body ,@body))))
    (let* ((S-sequence-vars (s-process-args argl other
                                            (cond ((eq mode '&sequence) var)))))
      (setq S-parse (cons (s-compress-arg-list '&input argl)
                          (s-parse form body))))))

;On the Lispm, this checks to see that all of the keywords are in the
;right package.  It also checks that we are not using any extranious keywords.

(defun-compile-time s-check-keywords (arg-list)
  (s-mapcar arg-list
    (cond ((memq item '(T NIL)) (s-e item "Not allowed in argument list"))
          ((not (symbolp item)) item)
          ((memq item '(&optional &rest &aux &sequence &unitary)) item)
          ((string-equal item "&SEQUENCE") '&sequence)
          ((string-equal item "&UNITARY") '&unitary)
          ((char= (aref (string item) 0) #\&)
           (s-e item "extranious keyword"))
          (T item))))

;This interprets the bound variable list.
;(Note keywords &optional &rest &aux &sequence &unitary and destructuring.)
;note that the user cannot directly specify the keywords
;&flag or &end-unitary.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar s-argl)
  (defvar s-code ()))

(defun-compile-time s-args-parse (arg-list)
  (let* (s-argl s-code (kind '&input) (mode '&unitary))
    (s-mapcar (s-check-keywords arg-list)
              (cond ((memq item '(&optional &rest &aux))
                     (cond ((memq kind (memq item '(&optional &rest &aux)))
                            (s-e arg-list "out of order keyword" item)))
                     (setq kind item))
                    ((memq item '(&sequence &unitary))
                     (setq mode item))
                    (T (let* (to info)
                         (cond ((and (eq kind '&rest) (not (s-variablep item)))
                                (s-e item "destructuring &rest args not supported")))
                         (cond ((or (eq kind '&input) (not (consp item)))
                                (setq to item info nil))
                               (T (setq to (car item) info (cadr item))))
                         (cond ((eq kind '&aux)
                                (cond ((and (consp item)
                                            (or  info (eq kind '&sequence)))
                                       (s-arg-code to info mode)))
                                (s-args-convert kind mode to nil))
                               ((not (s-variablep to))
                                (let* ((new (s-new-var 'd)))
                                  (s-arg-code to new mode)
                                  (s-args-convert '&aux mode to nil)
                                  (s-args-convert kind mode new info)))
                               (T (s-args-convert kind mode to info)))))))
    (list (nreverse s-argl) (nreverse s-code))))

(defun-compile-time s-arg-code (to from mode)
  (let* ((set-fn (cond ((s-variablep to) 'setq) (T 's-desetq)))
         (expr `(,set-fn ,to ,from)))
    (cond ((eq mode '&unitary)
           (setq expr `(at-start #'(lambda () ,expr)))))
    (push expr s-code)))

(defun-compile-time s-args-convert (kind mode to info)
  (s-mapcar (s-destructure-parse to)
    (push (s-make-arg kind mode item info) s-argl)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar s-vars ()))

(defun-compile-time s-destructure-parse (list)
  (let (s-vars) (s-destructure-parse1 list) (nreverse s-vars)))

(defun-compile-time s-destructure-parse1 (list)
  (cond ((s-variablep list) (push list s-vars))
        ((null list))
        ((not (consp list)) (s-e list "bad argument specification"))
        (T (s-destructure-parse1 (car list))
           (s-destructure-parse1 (cdr list)))))

;This takes in the body of a letS and parses each of the forms in it.
;ALL user error checking occures in this phase.  Note that we don't
;care what the return type is (since any returns are discarded) except
;for the last form.

(defun-compile-time s-parse (form body)
  (maplist #'(lambda (list)
               (s-parse1 (car list)
                 (cond ((cdr list) 'none)
                       ((eq form 'letS*) '&end-unitary)
                       (T 'any))))
           body))

(defun-compile-time s-parse1 (expr type)
  (cond ((memq expr S-sequence-vars)
	 (case type
	   (&unitary (s-e expr "Unitary argument is sequence var"))
	   (&end-unitary `(Rlast ,expr))
	   (&sequence expr)
	   (none `(Rignore-no-ret ,expr))
	   (any `(Msequence-var-out ,expr))))
	((or (s-copyable-constant expr) (s-variablep expr))
	 (case type
	   (&unitary expr) ;if var and AT-END we have an undetected bug
	   ((&end-unitary any) `(at-end #'(lambda () ,expr)))
	   (&sequence  `(mapS #'(lambda () ,expr)))
	   (none `(Rignore-no-ret (Gsequence ,expr)))))
	((and (consp expr) (s-frag-for (car expr)))
	 (let* ((ret-type (s-return-mode (s-frag-for (car expr)))))
	   (cond ((and (null ret-type) (not (eq type 'none)))
		  (s-e expr "nested sequence function has no return value")))
	   (case type
	     (&unitary
	      (case ret-type
		(&end-unitary (setq expr `(at-start #'(lambda () ,expr))))
		(&sequence
		 (s-e expr "Sequence provided where unitary value expected")))
	      (cond ((s-referencesp (ncons expr) S-sequence-vars)
		     (s-e expr "Initializing code references sequence vars"))))
	     (&end-unitary
	      (case ret-type
		(&unitary (setq expr `(Rlast (Gsequence ,expr))))
		(&sequence (setq expr `(Rlast ,expr)))))
	     (&sequence
	      (case ret-type
		(&unitary (setq expr `(Gsequence ,expr)))
		(&end-unitary (s-e expr "Implicit nesting not supported"))))))
	 (s-parse-parameters expr))
	(T (let* ((ret (s-tokenize expr)) (fn (car ret)) (params (cdr ret))
		  (has-at-end-rets
		   (s-mapcar params
		     (cond ((eq (s-return-mode (s-frag-for (car item)))
				'&end-unitary)
			    (return T)))))
		  (meta-fn
		   (case type
		     (&unitary 'at-start)
		     (&end-unitary (cond (has-at-end-rets 'at-end)
					 (T 'mapS)))
		     (&sequence 'mapS)
		     (none (cond (has-at-end-rets 'at-end-no-ret)
				 (T 'mapS-no-ret)))
		     (any (cond (has-at-end-rets 'at-end) (T 'mapS))))))
	     (cond ((and has-at-end-rets (consp expr) (eq (car expr) 'setq)
			 (consp (cdr expr)) (memq (cadr expr) s-sequence-vars))
		    (s-e expr "attempt to assign at-end value to sequence var")))
	     (s-parse1 `(,meta-fn ,fn .,params) type)))))


;This returns the mode of the first return value (if any) of a frag.
(defun-compile-time s-return-mode (frag)
  (s-process-args (s-returns frag) other
    (cond ((eq kind '&output) (return-from nil mode)))))

;this takes in an expr and returns a cons of:
;1- A lambda corresponding to all the top stuff down to seq-things.
;   Note that this will always be at least a variable.
;2- A list of seq stuff args.  (Maybe none.)

(defun-compile-time s-tokenize (expr)
  (let* (S-token-args S-token-params (bod (s-tokenize1 expr)))
    (cons `#'(lambda ,(nreverse S-token-args) ,bod)
          (nreverse S-token-params))))

;S-tokenize1 is a program that should understand macros and fexprs.
;It is defined below.

;This checks that the number of parameters is correct, and
;recurses to parse each of the parameters themselves.
(defun-compile-time s-parse-parameters (expr)
  (let ((args (s-args (s-frag-for (car expr))))
        (params (cdr expr))
        (result (list (car expr))))
    (s-process-args args other
      (case kind
        (&input (cond ((null params) (s-e expr "Too few parameters")))
                (push (s-parse1 (pop params) mode) result))
        (&optional (cond (params (push (s-parse1 (pop params) mode) result))))
        (&rest (s-mapcar params
                 (push (s-parse1 item mode) result) nil)
               (setq params nil)
               (return nil)))
      nil)
    (cond (params (s-e expr "too many parameters")))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;                       Combination of Fragments                        ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this creates a frag from a frag application.

(defun-compile-time s-frag-apply (call)
  (let* ((name (car call))
         (params (cdr call))
         (frag (s-handle-optional-and-rest
                 (s-uniquize (s-frag-for name)) params))
         (params-frag))
    (s-process-args (s-args frag) other
      (cond ((eq kind '&input)
             (let* ((param (pop params))
                    (frag? (and (consp param) (s-frag-for (car param)))))
               (cond (frag? (setq param (macroexpand param))))
               (setq frag (s-handle-input param var frag))
               (cond ((and frag? params-frag)
                      (setq params-frag (s-merge params-frag param)))
                     (frag? (setq params-frag param)))))))
    (cond (params-frag (setq frag (s-merge params-frag frag))))
    frag))

;This conses up a new fragment with new unique arg names.  It depends on the
;fact that the old args are already unique and therefore aren't bound anywhere
;or anything.  It forces total copying to protect the prototypes.
;Note extra work may have to be done because some output may be the same name
;as an input.

(defun-compile-time s-uniquize (frag)
  (sublis (s-rename-alist frag) (copy-tree frag)))

(defun-compile-time s-rename-alist (frag)
  (let ((renames (s-process-args (s-args frag) other
                   (cons var (s-new-var var)))))
    (s-process-args (s-returns frag) other
      (cond ((not (assq var renames))
             (push (cons var (s-new-var var)) renames))))
    renames))


;This takes care of optional arguments in the frag which is going to
;be applied.  (Note that error checking for number and type of args happens
;in s-parse.)

(defun-compile-time s-handle-optional-and-rest (frag params)
  (let* (rest-var new-rest-vars)
    (setf (s-args frag)
          (s-process-args (s-args frag) arglist
            (case kind
              (&input (pop params) T)
              (&optional (cond ((null params)
                                (cond ((or info (eq mode '&sequence))
                                       (s-make-interface var mode info frag)))
                                (setq kind '&aux)
                                (setq mode '&unitary))
                               (T (pop params)
                                  (setq kind '&input)))
                         (setq info nil) T)
              (&rest
               (setq rest-var var)
               (setq new-rest-vars
                     (mapcar #'(lambda (ignore) (declare (ignore ignore))
                                       (s-new-var 'param)) params))
               (setq s-result (nreconc (s-mapcar new-rest-vars
                                         (s-make-arg '&input mode item nil))
                                       s-result))
               (setq s-continue 'copy-rest) nil)
              (T T))))
      (cond (rest-var
             (setq frag (subst `(list ., new-rest-vars) rest-var frag))))
      frag))

;This combines an input parameter into a fragment.
;Note comments (near s-check) on why so much renaming is ok.

(defun-compile-time s-handle-input (param input frag)
  (cond ((s-frag? param)
         (s-rename-input (s-auxify-ret param) input frag))
        ((and (or (s-variablep param) (s-copyable-constant param))
              (not (s-var-in-args input (s-returns frag)))
              (not (s-writesp (append (s-icode frag) (s-code1 frag) (s-code2 frag)
                                      (s-pcode frag) (s-ucode frag))
                              (list input))))
         (s-rename-input param input frag))
        (T (setf (s-args frag)
                 (s-process-args (s-args frag) arglist
                   (cond ((eq var input)
                          (s-make-interface input mode param frag)
                          (setq kind '&aux)
                          (setq mode '&unitary)
                          (setq s-continue 'copy-rest)))
                   T))
           frag)))

(defun-compile-time s-auxify-ret (frag)
  (let (ret)
    (setf (s-returns frag)
          (s-process-args (s-returns frag) arglist
            (cond ((eq kind '&output)
                   (setq ret (or ret var))
                   (cond ((not (s-var-in-args var (s-args frag)))
                          (push (s-make-arg '&aux '&unitary var nil)
                                (s-args frag))))
                   nil)
                  (T T))))
    ret))


(defun-compile-time s-var-in-args (v args)
  (s-process-args args other
    (cond ((eq v var) (return T)))))

;This puts a setq in the right place to get the source into the indicated var.

(defun-compile-time s-make-interface (var mode source frag)
  (let* ((set `(setq ,var ,source)))
    (case mode
      (&unitary (push set (s-icode frag)))
      (&sequence
       (cond ((s-readsp (s-code1 frag) (list var))
              (push set (s-code1 frag)))
             ((s-readsp (s-code2 frag) (list var))
              (push set (s-code2 frag)))
             (T (push set (s-code1 frag)))))
      (&end-unitary (push set (s-pcode frag))))
    frag))

;This renames an input to a thing.
(defun-compile-time s-rename-input (param input frag)
  ;; debug mc
  ;(print (list frag (s-args frag)))
  (setf (s-args frag)
        (s-process-args (s-args frag) arglist
          (cond ((eq var input) (setq s-continue 'copy-rest) nil)
                (T T))))
  (subst param input frag))

;This merges two frags together.
(defun-compile-time s-merge (fraga fragb)
  (let* ((fragb-sequence-out
          (append (s-mapcar S-sequence-vars
                    (cond ((or (s-writesp (s-code1 fragb) (list item))
                               (s-writesp (s-code2 fragb) (list item)))
                           item)))
                  (s-process-args (s-returns fragb) other
                    (cond ((and (eq kind '&output) (eq mode '&sequence))
                           var)))))
         filter-flags)
    (setf (s-returns fraga)
          (s-process-args (s-returns fraga) arglist
            (cond ((and (eq kind '&flag)
                        (or (s-readsp (s-code1 fragb) info)
                            (s-readsp (s-code2 fragb) info)))
                   (push var filter-flags)
                   (setq info (append info fragb-sequence-out))))
            T))
    (cond ((cdr filter-flags) (setq filter-flags (cons 'and filter-flags)))
          (T (setq filter-flags (car filter-flags))))
    (cond (filter-flags
           (cond ((s-code1 fragb)
                  (setf (s-code1 fragb)
                        `((cond (,filter-flags . ,(s-code1 fragb)))))))
           (cond ((s-code2 fragb)
                  (setf (s-code2 fragb)
                        `((cond (,filter-flags . ,(s-code2 fragb)))))))))
    (s-make-frag (append (s-args fraga) (s-args fragb))
                 (append (s-returns fraga) (s-returns fragb))
                 (append (s-icode fraga) (s-icode fragb))
                 (append (s-code1 fraga) (s-code1 fragb))
                 (append (s-code2 fraga) (s-code2 fragb))
                 (append (s-pcode fraga) (s-pcode fragb))
                 (append (s-ucode fraga) (s-ucode fragb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;                   Production of Actual Loop Code                      ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;These hold gensym'ed constants used below

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar S-PROG nil "the most recent loop expansion")
  (defvar S-LOOP (gensym "LETS/L0") "LETS looping label")  ;must be same on each loading!
  (defvar S-END  (gensym "LETS/E0") "LETS loop end label"))

;This function creates a loop for a fragment.  Nested inputs are an
;error.  Due to the let fixnum variable bug, we use ugly nested progs
;for the unwind-protect stuff.

(defun-compile-time s-make-loop (frag)
  (setq frag (s-apply-simplification frag))
  (let* (bod (vars (s-process-args (s-args frag) other var))
         (rets (s-process-args (s-returns frag) other
                 (cond ((not (memq var vars)) (push var vars)))
                 (cond ((eq kind '&output) var)))))
    (setq bod `(        ,@(s-icode frag)
                ,S-LOOP ,@(s-code1 frag)
                        ,@(s-code2 frag)
                          (go ,S-LOOP)
                ,S-END  ,@(s-pcode frag)
                          (return-from T ., rets)))
    (cond ((s-ucode frag)
           (setq s-prog
                 `(let ,vars (unwind-protect (block T (let () (tagbody ., bod)))
                               ., (s-ucode frag)))))
          (T (setq s-prog `(block T (let ,vars (tagbody ., bod))))))
    (setq s-prog (sublis (s-mapcar s-user-renames
                           (cons (cdr item) (car item)))
                         s-prog))))

(defmacro done (&rest return-values)
  "Used to exit from a loop"
  (cond ((cdr return-values) `(return-from T .,(cdr return-values)))
        (T `(go ,S-END))))

;I would just use the ordinary desetq, but it isn't lispm standard.
;Note the fact that the TO is an ok destination has already been
;tested during parsing!

(defmacro s-desetq (to from)
  (cond ((s-variablep to) `(setq ,to ,from))
        (T (let ((v (let ((*gensym-counter* 0)) (s-new-var 'list))) body)
             (do ((tos to (cdr tos)))
                 ((null tos))
               (cond ((s-variablep tos)
                      (push `(setq ,tos ,v) body)
                      (return nil)))
               (cond ((not (null (car tos)))
                      (push `(s-desetq ,(car tos) (car ,v)) body)))
               (cond ((cdr tos) (push `(setq ,v (cdr ,v)) body))))
             `(let ((,v ,from)) ., (nreverse body))))))

;This simplifies expressions of the form (apply #'thing . args).  it
;is included because it is essential in order to make loops compile
;much at all (and to make them readable) The program uses a helping
;function to locate the instances of APPLY and FUNCALL.  This helping
;function should know about fexprs and macros.

(defun-compile-time s-apply-simplification (frag)
  (s-find-applies (s-icode frag))
  (s-find-applies (s-code1 frag))
  (s-find-applies (s-code2 frag))
  (s-find-applies (s-pcode frag))
  (s-find-applies (s-ucode frag))
  frag)

(defun-compile-time s-simplify-apply (apply)
  (prog (new-expr fn args)
    (cond ((or (s-eq-car (cadr apply) 'function)
               (and (s-eq-car (cadr apply) 'quote)
                    (symbolp (cadadr apply))))
           (setq fn (cadadr apply)))
          ((and (consp (cadr apply)) (string-equal (caadr apply) 'lambda))
           (setq fn (cons 'lambda (cdadr apply))))
          (T (return apply)))
    (cond ((s-eq-car apply 'funcall) (setq args (cddr apply)))
          ((not (s-eq-car apply 'apply)) (return apply))
          ((s-eq-car (caddr apply) 'list) (setq args (cdaddr apply)))
          ((and (s-eq-car (caddr apply) 'list*)
                (s-eq-car (car (last (caddr apply))) 'list))
           (setq args (nreconc (cdr (reverse (cdaddr apply)))
                               (cdar (last (caddr apply))))))
          (T (return apply)))
    (cond ((symbolp fn)
           (setq new-expr (cons fn args)))
          ((not (s-eq-car fn 'lambda)) (return apply))
          ((not (let ((fn-args (cadr fn)))
                  (s-mapcar args
                    (let ((fn-arg (pop fn-args)))
                      (cond ((not (or (s-variablep item)
                                      (and (s-copyable-constant item)
                                           (not (s-writesp (cddr fn)
                                                           (list fn-arg))))))
                             (return T))))
                    nil)))
           (let* ((body (cddr fn))
                  (alist (mapcar #'cons (cadr fn) args)))
             (cond ((cdr body) (setq body (cons 'progn body)))
                   (T (setq body (car body))))
             (setq new-expr (s-variable-rename alist body))))
          (T (let* ((body (cddr fn))
                    (pairs (mapcar #'list (cadr fn) args)))
               (setq new-expr `(let ,pairs .,body)))))
    (return new-expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;             Functions That Understand Macros and Fexprs               ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This gathers into one place all of the functions which need to
;understand fexprs and macros.  In general these functions would be
;more logically grouped other places.  Since they don't now have any
;understanding, they are now buggy.  Note however, that the task of
;fixing them is simplified by the fact that all of the other fns above
;don't have to understand fexprs and the like.

;This should be doing a macroexpand-all and a proper code walk.  Worst
;of all, due to the fact that macros like back-quote can cause a
;function call to end up as not the first item in a list
;{ie `(1 ,(Elist a))} we must be sensitive to sequence fn names in ALL list
;positions at present.

(defun-compile-time s-tokenize1 (thing)
  (cond ((or (not (consp thing))
             (memq (car thing) '(quote function letS letS*))) thing)
        (T (do ((exprs thing (cdr exprs))
                (result nil (cons (s-tokenize1 (car exprs)) result)))
               ((not (consp exprs)) (nreconc result exprs))
             (cond ((and (consp exprs) (s-frag-for (car exprs)))
                    (push exprs S-token-params)
                    (push (s-new-var 'V) S-token-args)
                    (return (nreconc result (car S-token-args)))))))))

;this fn takes in an alist indicating variable renamings and performs them
;all on a frag.  It can take advantage of the fact that the new names are
;guarranteed to be unique gensyms.  However the sources may not.  It should
;be checking that it is only changing references to these variables, not
;function calls with the same name and quoted constants etc.

(defun-compile-time s-variable-rename (alist frag)
  (sublis alist frag))

;this looks at the car of every list to see if it is apply or funcall

(defun-compile-time s-find-applies (exprs)
  (do ((e exprs (cdr e))) ((not (consp e)))
    (cond ((or (not (consp (car e))) (memq (caar e) '(quote function))))
          (T (prog ()
                L (cond ((and (consp (car e)) (memq (caar e) '(apply funcall)))
                         (let ((new (s-simplify-apply (car e))))
                           (cond ((not (eq new (car e)))
                                  (rplaca e new)
                                  (go L)))))))
             (s-find-applies (car e))))))

;these are little utilities for detecting what variables are
;referenced.  Note the way it assumes every instance of a symbol is a
;reference, and that the only writing operation is setq.  This must be
;true for any of the variables we are interseted in.  Or we risk
;trouble.  Note that the first arg to each of these must be a list of
;forms and the second arg must be a list of variables.

(defun-compile-time s-readsp (forms vars)
  (and vars (s-mapcar forms (cond ((s-r1 item T nil vars) (return T))))))

(defun-compile-time s-writesp (forms vars)
  (and vars (s-mapcar forms (cond ((s-r1 item nil T vars) (return T))))))

(defun-compile-time s-referencesp (forms vars)
  (and vars (s-mapcar forms (cond ((s-r1 item T T vars) (return T))))))

;Note that it is vitally important that this function thinks that
;(COMMENT (READING VAR)) is a read of VAR.

(defun-compile-time s-r1 (form read? write? vars)
  (cond ((and read? (memq form vars)) T)
        ((or (not (consp form)) (s-eq-car form 'quote)) nil)
        ((memq (car form) '(setq psetq))
         (do ((stuff (cdr form) (cddr stuff))) ((null stuff) nil)
           (cond ((or (and write? (memq (car stuff) vars))
                      (s-r1 (cadr stuff) read? write? vars))
                  (return T)))))
        (T (do ((f form (cdr f))) ((not (consp f)) nil)
             (cond ((s-r1 (car f) read? write? vars) (return T)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;                    Library of Sequence Functions                      ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;first we have an internal function for directly creating fragments.
;It is used here for two reasons:
; 1. It makes it possible to define the fundamental sequence functions
;    which cannot be defined using the other facilities provided.
; 2. It makes it possible to fine tune some of the sequence functions so that
;    they will compile more effeciently when used.  In particular we can
;    use many fewer variables.
;Note that every internal var gets initialized to NIL so you don't
;have to do this initialization explicitly.

(defmacro defrag (name doc arg-list return-list icode code1 code2 pcode ucode)
  (let* ((argl (cond ((not (memq '&aux arg-list)) arg-list)
                     (T (nreverse (cdr (memq '&aux (reverse arg-list))))))))
    `(s-defmacro ,name ,doc (values '(arglist ., argl))
       (s-make-frag (s-expand-arg-list '&input ',arg-list)
                    (s-expand-arg-list '&output ',return-list)
                    ',icode ',code1 ',code2 ',pcode ',ucode))))

(defrag at-start "Evals a fn at the start of a loop"
        (function &rest args) (value)
  ((setq value (apply function args))) () () () ())

(defrag at-end "Evals a fn at the end of a loop"
        (function &rest &end-unitary args) (&end-unitary value)
  () () () ((setq value (apply function args))) ())

(defrag at-unwind "Evals a fn when unwinding after a loop"
        (function &rest &end-unitary args) (&end-unitary value)
  () () () () ((apply function args)))

(defrag mapS "Evals a fn on succesive elements of sequences"
        (function &rest &sequence args) (&sequence map)
  () ((setq map (apply function args))) () () ())

(defrag previouS "Evals a fn on the previous elements of sequences"
        (init function &rest &sequence args &aux) (&sequence prior)
  () ((setq prior init)) ((setq init (apply function args))) () ())

(defrag filterS "Filters a sequence of values"
        (function &sequence sequence &rest args)
        (&sequence sequence &flag (flag (sequence)))
  () ((setq flag (apply function (list* sequence args)))) () () ())

(defrag truncateS "Truncates a sequence of values"
        (function &sequence sequence &rest args) (&sequence sequence)
  () ((cond ((apply function (list* sequence args)) (done)))) () () ())

(defrag generateS "Generates a sequence of values"
        (function init &rest &sequence args) (&sequence value)
  () ((setq value init))
     ((setq init (apply function (list* init args)))) () ())

(defrag enumerateS "Enumerates a sequence of elements"
        (trunc-function gen-function init) (&sequence value)
  () ((cond ((funcall trunc-function init) (done))) (setq value init))
     ((setq init (funcall gen-function init))) () ())

(defrag scanS "Evals a fn on successive elements of sequences with feedback"
        (function init &rest &sequence args) (&sequence value)
  () ((setq init (apply function (list* init args)))
      (setq value init)) () () ())

(defrag reduceS "Reduces a sequence to a unitary value"
        (function init &rest &sequence args) (&end-unitary init)
  () ((setq init (apply function (list* init args)))) () () ())

(defrag Pvalue "Creates a delayed sequence of values"
        (&sequence sequence &optional &unitary (init nil)) (&sequence out)
  () ((setq out init)) ((setq init sequence)) () ())

(defrag Fselect "Filters one sequence based on another"
        (&sequence sequence boolean-sequence)
        (&sequence sequence &flag (boolean-sequence (sequence)))
   () () () () ())

(defrag Tselect "Truncates a sequence based on another"
        (&sequence sequence boolean-sequence) (&sequence sequence)
  () ((cond (boolean-sequence (comment (reading sequence)) (done))))
     () () ())

(defrag Gsequence "Converts a unitary object into a sequence"
        (item) (&sequence item)
  () () () () ())

(defrag Rlast "Takes the last value of a sequence"
        (&sequence sequence &optional &unitary (default nil))
        (&end-unitary default)
  () ((setq default sequence)) () () ())

(defrag Rignore "Consumes a sequence returning NIL"
        (&sequence item) (&end-unitary value)
  () () () () ())

;These next four are used only for special internal reasons.

(defrag Rignore-no-ret "Consumes a sequence returning nothing"
        (&sequence item) ()
  () () () () ())

(defrag mapS-no-ret "Evals a fn on elements of sequences discarding the values"
        (function &rest &sequence args) ()
  () ((apply function args)) () () ())

(defrag at-end-no-ret "Evals a fn at the end of a loop discarding the value"
        (function &rest &end-unitary args) ()
  () () () ((apply function args)) ())

(defrag Msequence-var-out "Takes in a sequence and returns it"
        (&sequence in) (out &sequence)
  () () () () ())

(defrag Grange "Generates a sequence of integers"
        (&optional (start 1) (by 1)) (&sequence out)
  () ((setq out start)) ((setq start (+ start by))) () ())

(defrag Erange "Enumerates a sequence of integers"
        (start end &optional (by 1)) (&sequence out)
  () ((cond ((> start end) (done)))
     (setq out start)) ((setq start (+ start by))) () ())

(defrag Gsublists "Generates successive sublists of a list"
        (list) (&sequence out)
  () ((setq out list)) ((setq list (cdr list))) () ())

(defrag Esublists "Enumerates successive sublists of a list"
        (list) (&sequence out)
  () ((cond ((null list) (done))) (setq out list))
     ((setq list (cdr list))) () ())

(defrag Glist "Generates successive elements of a list"
        (list) (&sequence out)
  () ((setq out (car list))) ((setq list (cdr list))) () ())

(defrag Elist "Enumerates successive elements of a list"
        (list) (&sequence out)
  () ((cond ((null list) (done))) (setq out (car list)))
     ((setq list (cdr list))) () ())

(defrag Elist* "Enumerates successive elements of a list with non-list cdr"
        (list &aux end) (&sequence out)
  ((setq end (null list)))
  ((cond (end (done))
         ((consp list) (setq out (car list) list (cdr list)))
         (T (setq out list end T)))) () () ())

(defrag Ealist "Enumerates succesive key-value pairs from an alist"
        (alist &aux ptr label) (&sequence pair)
  ((setq ptr (cdar alist)))
  ((cond ((null alist) (done)))
   (setq pair (cons (caar alist) (car ptr))))
  ((setq ptr (cdr ptr))
   (cond ((null ptr) (setq alist (cdr alist) ptr (cdar alist)))))
  () ())

(defrag Eplist "Enumerates succesive property-value pairs from a plist"
        (plist &aux subplist) (&sequence pair)
  ((setq subplist (cdr plist)))
  ((cond ((null subplist) (done)))
   (setq pair (cons (car subplist) (cadr subplist))))
  ((setq subplist (cddr subplist))) () ())

(defrag Evector "Enumerates successive elements of a one-dimensional array"
        (vector &optional (first 0) (last (1- (length vector))))
        (&sequence element)
  () ((cond ((> first last) (done)))
      (setq element (aref vector first)))
     ((setq first (1+ first))) () ())

(defrag Efile-read "Enumerates successive forms in a file"
        (file-name &aux file eof) (&sequence thing)
  ((#+sbcl sb-sys:without-interrupts
    #-sbcl progn
           (setq file (open file-name :direction :input)))
   (setq eof (gensym)))
  ((setq thing (read file nil eof))
   (cond ((eq thing eof) (done))))
  () () ((cond (file (close file)))))

(defrag Efile "Enumerates successive forms in a file"
        (file-name &aux file eof) (&sequence thing)
  ((#+sbcl sb-sys:without-interrupts
    #-sbcl progn
           (setq file (open file-name :direction :input)))
   (setq eof (gensym)))
  ((setq thing (read-line file nil eof))
   (cond ((eq thing eof) (done))))
  () () ((cond (file (close file)))))

#|(defrag Fpositive "Selects integers greater than 0 from sequence"
        (&sequence integers)
        (&sequence integers &flag (f (integers)))
  () ((setq f (plusp integers))) () () ())|#

;; なんか説明と違うような。
;; (Rlist (Fpositive (Elist '(-1 2 nil 3))))
;; は通るらしいのだが。
(defrag Fpositive "Selects integers greater than 0 from sequence"
        (&sequence integers)
        (&sequence integers &flag (f (integers)))
  () ((setq f (and (numberp integers)
                   (plusp integers)))) () () ())


(defrag Fgreater "Selects integers greater than a given value from sequence"
        (&sequence integers &optional &unitary (limit 0))
        (&sequence integers &flag (f (integers)))
  () ((setq f (> integers limit))) () () ())

(defrag Rlist "Makes a list out of a sequence of objects"
        (&sequence object) (&end-unitary revlist)
  () ((setq revlist (cons object revlist))) ()
     ((setq revlist (nreverse revlist))) ())

(defrag Rbag "Makes a list out of a sequence of objects (order undefined)"
        (&sequence object) (&end-unitary list)
  () ((setq list (cons object list))) () () ())

(defrag Rnconc "Nconcs together a sequence of lists"
        (&sequence sublist &aux end) (&end-unitary list)
  () ((cond (sublist
             (cond (end (rplacd (last end) sublist)))
             (setq end sublist)
             (cond ((null list) (setq list sublist)))))) () () ())

(defrag Rappend "Appends together a sequence of lists"
        (&sequence sublist &aux end) (&end-unitary list)
  () ((cond (sublist
             (setq sublist (copy-list sublist))
             (cond (end (rplacd (last end) sublist)))
             (setq end sublist)
             (cond ((null list) (setq list sublist)))))) () () ())

(defrag Rlist* "Combines objects into a list - last object is last cdr"
        (&sequence item) (&end-unitary revlist)
  () ((setq revlist (cons item revlist))) ()
  ((cond (revlist (setq revlist (nreconc (cdr revlist) (car revlist)))))) ())

(defrag Rset "Combines objects together into a list without EQUAL duplicates"
        (&sequence item) (&end-unitary set)
  () ((cond ((not (member item set)) (setq set (cons item set))))) () () ())

(defrag Reqset "Combines objects together into a list without EQ duplicates"
        (&sequence item) (&end-unitary set)
  () ((cond ((not (memq item set)) (setq set (cons item set))))) () () ())

(defrag Ralist "Combines keys and values into an alist sorted by EQUAL keys"
        (&sequence key value) (&end-unitary alist)
  () ((let ((entry (assoc key alist)))
        (cond (entry (rplacd entry (cons value (cdr entry))))
              (T (setq alist (cons (list key value) alist)))))) () () ())

(defrag Reqalist "Combines keys and values into an alist sorted by EQ keys"
        (&sequence key value) (&end-unitary alist)
  () ((let ((entry (assq key alist)))
        (cond (entry (rplacd entry (cons value (cdr entry))))
              (T (setq alist (cons (list key value) alist)))))) () () ())

(defrag Rplist "Combines sequences of properties and values into a plist"
        (&sequence name value) (&end-unitary revplist)
  () ((setq revplist (list* value name revplist))) ()
  ((setq revplist (cons nil (nreverse revplist)))) ())


(defrag Rvector "Stores a sequence of objects in a one dimensional array"
        (vector &sequence element &optional &unitary (first 0)
                (last (1- (length vector))))
        (&end-unitary vector)
  ()
  ((cond ((> first last) (done)))
   (setf (aref vector first) element))
  ((setq first (1+ first))) () ())


#|(defrag Rfile "Writes a sequence of objects into a file"
        (file-name &sequence object &aux outfile finish) (&end-unitary ret)
  ((without-interrupts (setq outfile (open file-name 'out))))
  (((lambda (prinlength prinlevel) (print object outfile)) nil nil))
  ()
  ((setq ret T finish T))
  ((cond ((null outfile) nil)
         ((and (null finish) (y-or-n-p "delete partial output file"))
          (delete-file outfile))
         (T (close outfile)))))|#

(defrag Rfile "Writes a sequence of objects into a file"
        (file-name &sequence object &aux outfile finish) (&end-unitary ret)
  ((#+sbcl sb-sys:without-interrupts
    #-sbcl progn
           (setq outfile (open file-name :direction :output))))
  (((lambda (prinlength prinlevel) (write-line object outfile)) nil nil))
  ()
  ((setq ret T finish T))
  ((cond ((null outfile) nil)
         ((and (null finish) (y-or-n-p "delete partial output file"))
          (delete-file outfile))
         (T (close outfile)))))

(defrag Rsum "Adds up a sequence of fixnums"
        (&sequence num) (&end-unitary sum)
  ((setq sum 0))
  ((setq sum (+ sum num))) () () ())

(defrag Rsum$ "Adds up a sequence of flonums"
        (&sequence num) (&end-unitary sum)
  ((setq sum 0.0))
  ((setq sum (+ sum num))) () () ())

(defrag Rmax "computes the MAX of a sequence of numbers"
        (&sequence num) (&end-unitary max)
  () ((cond ((or (null max) (> num max)) (setq max num)))) () () ())

(defrag Rmin "computes the MIN of a sequence of numbers"
        (&sequence num) (&end-unitary min)
  () ((cond ((or (null min) (< num min)) (setq min num)))) () () ())

(defrag Rcount "Counts the number of things in a sequence"
        (&sequence item) (&end-unitary count)
  ((setq count 0))
  ((comment (reading item)) (setq count (1+ count))) () () ())

(defrag Rand "Takes the AND of a sequence of objects"
        (&sequence item) (&end-unitary bool)
  ((setq bool T))
  ((setq bool (and bool item))) () () ())

(defrag Rand-fast
 "Takes the AND of a sequence of objects, (stops loop when NIL encountered)"
        (&sequence item) (&end-unitary bool)
  ((setq bool T))
  ((cond ((null (setq bool item)) (done nil)))) () () ())

(defrag Ror "Takes the OR of a sequence of objects"
        (&sequence item) (&end-unitary bool)
  () ((setq bool (or bool item))) () () ())

(defrag Ror-fast
 "Takes the OR of a sequence of objects, (stops loop when non-NIL encountered)"
        (&sequence item) (&end-unitary bool)
  () ((cond ((setq bool item) (done item)))) () () ())

;; eof
