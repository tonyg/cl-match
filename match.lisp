;;; cl-match:  extended--ML-style pattern matching
;;-------------------------------------------------------------------------
;; This software is Copyright (c) 2008 Daniel S. Bensen.
;; You are hereby granted permission to distribute and use this software
;; as governed by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;; This software is provided "as is" with no express or implied warranty.
;;-------------------------------------------------------------------------

;;TODO: array string

(declaim (optimize (debug 3)))

(std:in-defpackage :cl-match (:use :cl :std) (:export :cl-match-pattern)
		   (:export :ifmatch :letmatch :match :defpattern))
(use-std-readtable)

(def all-equal (xs)
  [(not xs) or (lett x0 (car xs) (apply #'andf (mapcar #f[_ equal x0] (cdr xs))))])

(defstruct vgt  vars gensyms test) ;; a complete test with vars & gensyms
(defstruct conj vars tests gensyms ors whens) ;; a conjunction of subtests
;;;conj:
;;;The first occurance of each variable name is pushed onto vars.
;;;Gensyms, tests, and whens are all pushed onto their respective fields.

(defstruct x-or x patrns level) ;; x = expr, implicit OR
(def new-x-or (x patrns level) (make-x-or :x x :patrns patrns :level level))

(def new-vgt (vars gensyms test) (make-vgt :vars vars :gensyms gensyms :test test))

(def new-conj (&optional vars tests gensyms ors whens)
  (make-conj :vars vars :tests tests :gensyms gensyms :ors ors :whens whens))

(defmac let-vgt  (fields val &body body) `(letstruct (vgt-  ,@fields) ,val ,@body))
(defmac let-conj (fields val &body body) `(letstruct (conj- ,@fields) ,val ,@body))

(defmac asrt (test vars mesg &rest mesg-args)
  `(assert ,test ,vars ,(concstr "~A: " mesg) gfuncname ,@mesg-args))

(def is-small-type (x) "Arg's printed representation is compact."
     (typep x '(or symbol boolean float fixnum character keyword)))

(def is-big-type (x) (not [x is-small-type]))

(def is-literal (x) "Arg is a literal value."
     (typep x '(or boolean number character array keyword)))

(def is-wildcard (x) [(symbolp x) and [(symbol-name x) string= "_"]])

(defmacro setft     (x val)            `(progn (setf ,x ,val) t))
(defmacro setf-cons (x val) `(and (consp ,val) (setf ,x ,val)))

(defmac with-gensym-conj ((conj gensym expr &rest tests) &body body)
  "Create and bind a conj with a gensym, a setft form, and optional tests."
  (lett gensym-is-list [gensym islist]
    (if gensym-is-list
	(assert [gensym has-length 2] (gensym)
		"with-gensym-conj: list gensym must have exactly two elements.")
	(assert [gensym is-symbol] (gensym) "gensym must be a symbol or list."))
    (let* (( x   (if gensym-is-list (car  gensym) gensym))
	   (name (if gensym-is-list (cadr gensym)  nil))
	   (gensym-form (if name `(gensym ,name) '(gensym))))
      (with-gensyms (expr-is-big all-tests)
	`(let* ((,expr-is-big [,expr is-big-type])
		(,x (if ,expr-is-big ,gensym-form ,expr))
		(,all-tests (list ,@tests)))
	  (when ,expr-is-big (push `(setft ,,x ,,expr) ,all-tests))
	  (lett ,conj (new-conj nil (nreverse ,all-tests) [,expr-is-big and (list ,x)])
	    ,@body))))))

(def patrn-type (x)
  (if [x isstring] x
      (if [x is-symbol]  (symbol-name x)
	  (error "Bad pattern type: ~S" x))))

(def type= (x y) [(patrn-type x) string= (patrn-type y)])

(defmacro pushand (test tests) `(unless [,test eq t] (push ,test ,tests)))

(def push-gensym (gensym conj)                     (push gensym (conj-gensyms conj)))
(def push-test   ( test  conj) (unless [test eq t] (push  test  (conj-tests   conj))))

(def push-conj (src dest)
  (asrt [dest conj-p] (dest) "dest is not a conj")
  (when src
    (asrt [src conj-p] (src) "src is not a conj")
    (pushlist (conj-vars    src) (conj-vars    dest)) ; uniq only!
    (pushlist (conj-tests   src) (conj-tests   dest))
    (pushlist (conj-gensyms src) (conj-gensyms dest))
    (pushlist (conj-ors     src) (conj-ors     dest))
    (pushlist (conj-whens   src) (conj-whens   dest)))
  dest)

(def quote-conj (expr parts)
  (asrt [parts has-length 1] (parts) "must have exactly one part.")
  (new-conj nil (list `[,expr eql ',(car parts)])))

(def atom-conj (expr atom vars)
  (cond	([atom is-wildcard]    (new-conj))
	((not atom)            (new-conj nil (list `(not ,expr))))
	([[atom is-literal]
	  or (find atom vars)] (new-conj nil (list `[,expr eql ,atom])))
	(t                     (new-conj (list atom) (list `(setft ,atom ,expr))))))

(def as-conj (expr parts vars)
  (assert            parts   (parts) "AS pattern:  no variable name.")
  (assert (not (cddr parts)) (parts) "AS pattern:  too many parts.")
  (let ((name  (car  parts))
	(patrn (if (cdr parts) (cadr parts) '_)))
    (assert (not [name is-literal]) (name)
	    "Literal found in AS pattern. 1st arg must be a name.")
    (lett conj (cond ([name is-wildcard] (new-conj))
		     ((find name vars)  (new-conj nil (list `[,expr eql ,name])))
		     (t                (new-conj (list name) (list `(setft ,name ,expr)))))
      (push-conj (patrn-conj expr patrn vars)
		 conj))))

;;; BOOLEAN

(def and-conj (expr patrns vars)
  (case (length patrns)
    (0 (new-conj))
    (1 (patrn-conj expr (car patrns) vars))
    (t (with-gensym-conj (conj (x "AND") expr)
	   (dolist (patrn patrns conj)
	     (push-conj (patrn-conj x patrn (append (conj-vars conj) vars))
			conj))))))

(def or-vgts (x-or vars more when)
  (let ((x     (x-or-x     x-or))
	(level (x-or-level x-or)))
    (mapcar #f(patrn-vgt x _ vars more when level) (x-or-patrns x-or))))

 (def ors-vgt (vars ors when)
  "vars & test for a list/conjunction of OR forms"
  (eif (no ors)
      (new-vgt nil nil [when or t])
      (let* ((item (car ors)) ;; x-or (from OR) or conj (from NOT)
	     (more (cdr ors))
	     (vgts ;;(typecase item
		     ;;(x-or
	      ( or-vgts item vars more when))
		     ;;(conj (not-vgts item vars more when))
		     ;;(t (error "ORS-VGT:  Bad item: ~S" item))))
	     (varlists (mapcar #'vgt-vars vgts)))
	(asrt (all-same-vars varlists) (varlists) "Found unequal varlists.")
	(new-vgt (vgt-vars (car vgts))
		 (apply #'append (mapcar #'vgt-gensyms vgts))
		 `(or ,@(mapcar #'vgt-test vgts))))))

(def is-vals-patrn (patrn) [[patrn islist :min-length 1] and [(car patrn) type= :vals]])
(def is-when-guard (patrn) [[patrn islist   :length   3] and [(car patrn) type= :when]])

(def are-all-vals (parts)
  (apply #'andf (mapcar (lambda (part)
			  [[part is-vals-patrn]
			   or [[part is-when-guard] and [(caddr part) is-vals-patrn]]])
			parts)))

(def or-conj (expr parts vars &optional old-level)
  (case (length parts)
    (0 (new-conj nil (list nil)))
    (1 (patrn-conj expr (car parts) vars old-level))
    (t (lett new-level (case old-level (:top :top) (t :or))
	 (eif (not [parts are-all-vals])
	     (with-gensym x
	       (new-conj nil (list `(setft ,x ,expr))
			 (list x) (list (new-x-or x parts new-level))))
	   (asrt [old-level eq :top] () "VALS pattern is allowed only at the top level.")
	   (new-conj nil nil nil (list (new-x-or expr parts new-level))))))))

(def list*-conj (expr parts vars &optional (type "LIST*"))
  (asrt parts nil "No tail in ~A pattern" type)
  (eif (no (cdr parts))
      (patrn-conj expr (car parts) vars) ;tail only
    (with-gensym-conj (conj (x type) expr)
      (let* ((revparts (nreverse parts))
	     (tail (car revparts))
	     (elements (nreverse (cdr revparts))))
	(flet ((push-patrn (x patrn)
		 (push-conj (patrn-conj x patrn (append (conj-vars conj) vars))
			    conj)))
	  (cond ((not (cdr elements)) ;only 1 cons, don't need CELL
		 (push-test `(consp ,x) conj)
		 (push-conj (patrn-conj `(car ,x) (car elements) vars) conj)
		 (push-patrn `(cdr ,x) tail))
		(t (with-gensym (cell "CELL") ;at least 2 conses
		     (push-test `(setf-cons ,cell ,x) conj)
		     (push-gensym cell conj)
		     (flet ((push-item (item)
			      (with-gensym (car "CAR")
				(push-test `(setft ,car (car ,cell)) conj)
				(push-gensym car conj)
				(push-patrn car item))))
		       (push-item (car elements))
		       (dolist (item (cdr elements))
			 (push-test `(setf-cons ,cell (cdr ,cell)) conj)
			 (push-item item))
		       (push-patrn `(cdr ,cell) tail)))))
	    conj)))))

(def list-conj  (expr parts vars) (list*-conj expr (append parts (list nil)) vars "LIST"))
(def cons-conj  (expr parts vars)
  (assert [parts has-length 2] (parts) "CONS pattern must have exactly two parts.")
  (list*-conj expr parts vars "CONS"))

(def std-object-conj (expr parts vars typestr f-exists f-bound f-expr)
  (with-gensym-conj (conj (x typestr) expr `(typep ,x 'standard-object))
    (dolist (part parts conj)
      (let* ((part-is-atom (atom part))
	     (name (if part-is-atom part (car part))))
	(when f-exists
	  (push-test (funcall f-exists x name) conj))
	(asrt [part-is-atom or (not (cddr part))] (part) "too many items in ~A entry" typestr)
	(lett slot-patrn (if part-is-atom part (if (cdr part) (cadr part) '_))
	  (unless [slot-patrn is-wildcard]
	    (when f-bound
	      (push-test (funcall f-bound x name) conj))
	    (push-conj (patrn-conj (funcall f-expr x name) slot-patrn
				   (append (conj-vars conj) vars)) conj)))))))

(def acsrs-conj (expr parts vars)
  (std-object-conj expr parts vars "ACSRS" nil nil (lambda (x acsr) `(,acsr ,x))))

(def slots-conj (expr parts vars)
  (std-object-conj expr parts vars "SLOTS"
		   (lambda (x slot) `(slot-exists-p ,x ',slot))
		   (lambda (x slot) `(slot-boundp   ,x ',slot))
		   (lambda (x slot) `(slot-value    ,x ',slot))))

(def struct-conj (expr parts vars)
  (assert parts nil "No accessor prefix in STRUCT pattern")
  (let* ((struct- (car parts))
	 (prefix (string-or-symbol-name struct-))
	 (struct-p (find-symbol (concstr prefix "P"))))
    (with-gensym-conj (conj (x "STRUCT") expr `(,struct-p ,x))
      (dolist (part (cdr parts) conj)
	(let* ((part-is-atom (atom part))
	       (field-symbol (if part-is-atom part (car part)))
	       (struct-field (find-symbol (concstr prefix (symbol-name field-symbol))))
	       (field-expr `(,struct-field ,x))
	       (all-vars (append (conj-vars conj) vars)))
	  (eif part-is-atom
	      (push-conj (atom-conj field-expr part all-vars) conj)
	    (assert (not (cddr part)) (part) "STRUCT pattern:  too many items in field entry")
	    (when [(cdr part) and (not [(cadr part) is-wildcard])]
	      (push-conj (patrn-conj field-expr (cadr part) all-vars) conj))))))))

(def array-dims (rank vals)
  (asrt [rank isint :>= 0] (rank) "Rank must be a non-negative integer.")
  [[rank >0] and
   (let* ((dim (length vals))
	  (lower-dims (mapcar #f(array-dims [rank - 1] _) vals)))
     (asrt (all-equal lower-dims) (lower-dims) "dimensions don't match.")
     (cons dim (car lower-dims)))])

(def subarray-conj (array hi-bak-ns lo-dims patrn vars elt-type)
  (eif (no lo-dims)
      (patrn-conj `(aref ,array ,@(reverse hi-bak-ns))
		  (if elt-type `(type ,elt-type ,patrn) patrn) vars)
    (asrt [(length patrn) = (car lo-dims)] (lo-dims patrn) "dimension mismatch.")
    (lett conj (new-conj)
      (count-list (n p patrn conj)
	  (push-conj (subarray-conj array (cons n hi-bak-ns) (cdr lo-dims)
				    p (append (conj-vars conj) vars) elt-type)
		     conj)))))

(def array-conj (expr parts vars)
  (asrt [parts islist :length 2] (parts) "ARRAY pattern takes exactly 2 args.")
  (let* ((spec (car parts))
	 (spec-is-list [spec islist]))
    (when spec-is-list
      (asrt (not (cddr spec)) (spec) "Too many elements in ARRAY pattern spec."))
    (let* ((  rank   (if spec-is-list (car  spec) spec))
	   (elt-type (if spec-is-list (cadr spec) nil))
	   (patrn (cadr parts))
	   (dims (array-dims rank patrn)))
    (with-gensym-conj (conj (x "ARRAY") expr `(arrayp ,x) `[(array-dimensions ,x) equal ',dims])
      (push-conj (subarray-conj x nil dims patrn vars elt-type)
		 conj)))))

(def vec-conj (expr parts vars) ; (array-conj expr (cons (list 1) parts) vars))
  (asrt [parts islist :min-length 1 :max-length 2] (parts) "VEC pattern takes 1 or 2 args.")
  (let* ((patrn (car parts))
	 (elt-type (cadr parts))
	 (length (length patrn)))
    (with-gensym-conj (conj (x "VEC") expr `(vectorp ,x) `[(length ,x) = ,length])
      (push-conj (subarray-conj x nil (list length) patrn vars elt-type)
		 conj))))

(def type-conj (expr parts vars)
  (asrt [parts islist :min-length 1 :max-length 2] (parts) "TYPE pattern takes 1 or 2 args.")
  (lett type (car parts)
    (with-gensym-conj (conj (x "TYPE") expr `(typep ,x ',type))
      (when (cdr parts) (push-conj (patrn-conj x (cadr parts) vars) conj))
      conj)))

(def vals-conj (gensyms patrns level)
  (asrt [level eq :top] () "Vals pattern is allowed only at the top level.")
  (asrt [(length gensyms) = (length patrns)] (gensyms patrns)
	"gensym & pattern list lengths don't match.")
  (eif (no patrns) (new-conj)
    (let ((conj (new-conj))
	  (pairs (mapcar #'cons gensyms patrns)))
      (dolist (pair pairs conj)
	(push-conj (patrn-conj (car pair) (cdr pair) (conj-vars conj))
		     conj)))))

(def when-conj (expr parts vars level)
  (asrt (find level '(:top :or)) (level) "misplaced WHEN guard.")
  (asrt parts nil "No test in WHEN guard.")
  (asrt (not (cddr parts)) (parts) "Too many parts in WHEN guard")
  (lett conj (new-conj nil nil nil nil (list (car parts)))
    (when (cdr parts)
      (push-conj (patrn-conj expr (cadr parts) vars level)
		 conj))
    conj))

(def default-conj (expr patrn vars &optional level)
  (let ((type (patrn-type (car patrn)))
	(parts (cdr patrn)))
    (strcase type
	     ("ACSRS"  ( acsrs-conj expr parts vars))
	     ("AND"    (   and-conj expr parts vars))
	     ("ARRAY"  ( array-conj expr parts vars))
	     ("AS"     (    as-conj expr parts vars))
	     ("CONS"   (  cons-conj expr parts vars))
	     ("LIST"   (  list-conj expr parts vars))
	     ("LIST*"  ( list*-conj expr parts vars))
	     ("QUOTE"  ( quote-conj expr parts))
	     ("SLOTS"  ( slots-conj expr parts vars))
	     ("STRUCT" (struct-conj expr parts vars))
	     ("OR"     (    or-conj expr parts vars level))
	     ("TYPE"   (  type-conj expr parts vars))
	     ("VALS"   (  vals-conj expr parts      level))
	     ("VEC"    (   vec-conj expr parts vars))
	     ("WHEN"   (  when-conj expr parts vars level))
	     (t (error "Unknown pattern: ~S" type)))))

(def patrn-conj (expr patrn vars &optional level)
  (eif (atom patrn)
      (atom-conj expr patrn vars)
    (lett type-spec (car patrn)
      (eif [type-spec has-type '(or string keyword)]
	  (default-conj expr patrn vars level)
	(assert [type-spec is-symbol] (type-spec) "Pattern type must be a string or symbol.")
	(lett xformr (get type-spec 'cl-match-pattern)
	  (eif xformr
	      (patrn-conj expr (funcall xformr (cdr patrn)) vars level)
	    (default-conj expr            patrn             vars level)))))))

(def all-same-vars (unsorted-lists)
  (lett lists (copy-tree unsorted-lists)
    (mapc #f(sort _ #'string< :key #'symbol-name)
	  lists)
    (all-equal lists)))

(def patrn-vgt (expr patrn &optional oldvars old-ors old-when level)
  (let-conj (vars gensyms tests ors whens)
      (patrn-conj expr patrn oldvars level)
    (let ((vars          (nreverse vars   ))
	  (gensyms       (nreverse gensyms))
	  (tests         (nreverse tests  ))
	  (ors   (append (nreverse ors    )  old-ors))
	  (whens (append (nreverse whens  ) [old-when and (list old-when)])))
      (if (no ors)
	  (new-vgt vars gensyms [(not [tests or whens]) or `(and ,@tests ,@whens)])
	  (with-gensym when
	    (let-vgt ((or-vars    vars)
		      (or-gensyms gensyms)
		      (or-test    test))
		(ors-vgt vars ors [whens and `(,when)])
	      (lett test
		  (if (no whens)
		      `(and ,@tests ,or-test)
		      `(and ,@tests (flet ((,when () (and ,@whens))) ,or-test)))
		(new-vgt
		 (append vars or-vars)
		 (append [whens and (list when)] gensyms or-gensyms)
		 test))))))))

(def patrn-vgt-top (expr patrn) (patrn-vgt expr patrn nil nil nil :top))

(eval-always
  (def binding-test (multibindings)
    (lett tests '()
      (dolist (mb multibindings)
	(lett var (car mb)
	  (dolist (expr (cdadr mb))	;the rest of the bindings
	    (push `[,var eql ,expr] tests))))
      (case (length tests)
	(0 t)
	(1 (car tests))
	(t `(and ,@(nreverse tests)))))))

(def num-gensyms (patrn)
  [[patrn iscons] and
   (let ((type  (car patrn))
	 (parts (cdr patrn)))
     (if [type type= :vals] (length parts)
	 (and [type type= :or]
	      [parts are-all-vals]
	      (lett nums (mapcar (lambda (part) (length (cdr part))) parts)
		(if (reduce #'= nums) (car nums)
		    (error "OR pattern:  VALS must all have the same length."))))))])

(def clause-form (block x clause)
  (let-vgt (vars gensyms test) (patrn-vgt-top x (car clause))
    (let* ((bindings (append vars gensyms))
	   (body (cdr clause))
	   (form `(return-from ,block ,(if (cdr body) `(progn ,@body) (car body)))))
      (unless [test eq t] (setf form `(when  ,test   ,form)))
      (when    bindings   (setf form `(let ,bindings ,form)))
      form)))

(def match-core (block x clauses)
    (if (and (no (cddr clauses)) [(cadr clauses) eq t]) ;ifmatch
	(let-vgt (vars gensyms test) (patrn-vgt-top x (caar clauses))
	  (let* ((bindings (append vars gensyms))
		 (form `(progn ,(cdar clauses)))
		 (got-a-test (not [test eq t]))
		 (failbody (cddr clauses)))
	    (when got-a-test (setf form `(when ,test ,(if failbody `(return-from ,block ,form)
							  form))))
	    (when bindings                  (setf form `(let ,bindings ,form)))
	    (when [failbody and got-a-test] (setf form `(block ,block ,form ,@failbody)))
	    form))
      (mapcar #1f(clause-form block x) clauses)))

(def succeed (code) (if (cdr code) `(progn ,@code) (car code)))

(defmac match (expr &rest clauses) ;;TODO: check for symbol macros.
  (with-gensyms ((var "VAR") (block "MATCH"))
    (let* ((num-gensyms (num-gensyms (caar clauses))) ;; patrn is vals
	   (expr-is-big (not [expr is-small-type]))
	   (expr-syms [num-gensyms and (gensyms num-gensyms)])
	   (expr-var (if num-gensyms expr-syms (if expr-is-big var expr)))
	   (code (match-core block expr-var clauses)))
      (if num-gensyms
	  (setf code (list `(multiple-value-bind ,expr-syms ,expr
			     (declare (ignorable ,@expr-syms)) ,@code)))
	  (when expr-is-big
	      (setf code (list `(let ((,var ,expr)) (declare (ignorable ,var)) ,@code)))))
      `(block ,block ,@code))))

(defmac ifmatch (patrn expr succeed &body failbody)
  "Evaluate success form if pattern matches expression, failure forms if not."
  `(match ,expr (,patrn ,succeed) (_ ,@failbody)))

(defmac letmatch (patrn expr &body success)
  `(ifmatch (,patrn ,expr ,(succeed success) (error "LETMATCH:  mismatch."))))

(defmac defpattern (patrn args &body body)
  (check-type patrn symbol "DEFPATTERN: 1st arg must a symbol")
  `(eval-always
    (setf (get ',patrn 'cl-match-pattern) #f(lett ,args _ ,@body))))

#|

(def array-conj (expr parts vars)
  (asrt [parts islist :min-length 2 :max-length 3] (parts) "ARRAY pattern takes 2 or 3 args.")
  (let* ((rank     (car   parts))
	 (patrn    (cadr  parts))
	 (elt-type (caddr parts))
	 (dims (array-dims rank patrn)))
    (with-gensym-conj (conj (x "ARRAY") expr `(arrayp ,x) `[(array-dimensions ,x) equal ',dims])
      (push-conj (subarray-conj x nil dims patrn vars elt-type)
		 conj))))

;;(def not-vgts (conj oldvars more when) ;(not conj) -> (or (not tests) {(not or)}* (not guards))
;;  (let* ((allvars (allvars conj oldvars more when))
;;	 (test-vgt (test-vgt `(not (and ,@tests)) oldvars more when))
;;	 (conj-vgts (mapcar #f(conj-vgt _ oldvars more when)
	   ;;each negated OR becomes a conj
	   ;;(not conj) -> (or test conj* guard) test, conj's, & guard have been negated

;;(def not-patrn (patrn) `(not ,patrn))

;;(def not-or-conj (x patrns vars) (and-conj x (mapcar #'not-patrn patrns) vars))

(def not-conj (expr parts vars)
  (assert [parts has-length 1] (parts) "NOT pattern must have exactly one part.")
  (with-gensym-conj (not-conj (x "NOT") expr)
    (lett patrn-conj (patrn-conj x (car parts) vars)
      (eif [(conj-ors patrn-conj) or (conj-whens patrn-conj)]
	  (setf (conj-ors not-conj) (list patrn-conj)) ;; patrn-conj will be negated in OR-VGT
	(let* ((tests (append (conj-tests patrn-conj)
			      (conj-tests   not-conj))
	       (not-test (case (length tests)
			   (0 nil)
			   (1 `(not ,(car tests)))
			   (t `(not (and ,@(nreverse tests)))))))
	  (setf (conj-tests not-conj) (list not-test)))))
    not-conj))
|#