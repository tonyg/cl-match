;;; cl-match:  extended--ML-style pattern matching
;;; unit tests using pcl-test
;;-------------------------------------------------------------------------
;; This software is Copyright (c) 2008 Daniel S. Bensen.
;; You are hereby granted permission to distribute and use this software
;; as governed by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;; This software is provided "as is" with no express or implied warranty.
;;-------------------------------------------------------------------------

;;(declaim (optimize (debug 3)))

(cl:defpackage :cl-match-test (:use :cl :pcl-unit-test :cl-match))
(cl:in-package :cl-match-test)

(defmacro patrn-errs (patrn)
  `(let ((val (gensym "VAL"))) (errs (cl-match::patrn-vgt val ',patrn))))

(defmacro     matches (patrn expr) `(ifmatch ,patrn ,expr t nil))
(defmacro not-matches (patrn expr) `(not (matches ,patrn ,expr)))

(defmacro pairs-match (pairs)
  `(progn ,@(loop for pair in pairs
		  collect (cons 'matches pair))))

(defstruct foo bar gak)

(defpattern foo (&rest fields) `(struct foo- ,@fields))

(defclass urp () ((wuf :initarg :wuf :accessor wuf)
		  (eek :initarg :eek :accessor eek)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun test-patrn (form)
  (if (eq (car form) :quote)
      (cadr form)
      (let ((patrn (car  form))
	    (expr  (cadr form))
	    (matching (not (cddr form))))
	(if (eq expr :errs)
	    `(patrn-errs ,patrn)
	    (if matching
		`(matches ,patrn ,expr)
		`(not-matches ,patrn ,expr)))))))

(defmacro def-test-match (&body tests)
  `(deftest test-match ()
    (test-forms
      ,@(loop for test in tests collect (test-patrn test)))))

(def-test-match
      (x 1)
    (_ nil)
    ;;list, list*, cons
    ((list*)   :errs)
    ((cons)     :errs)
    ((cons x)    :errs)
    ((cons x y z) :errs)
    ((list)        nil)
    ((list x)     '(nil)) ;setf vs. setft
    ((list* x _)   '(nil))
    ((list* x _)   (list* nil 1))
    ((list x y)    '(a b))
    ((:list x y)    '(a b)) ;keyword default
    (("LIST" x y)    '(a b)) ;string default
    ((list x y)    (cons 1 2) :not)
    ((list x)          1      :not) ;wrong type
    ((list x y x)     '(a b)   :not) ; too short
    ((list x y)       '(a b c)   :not) ; too long
    ((list* car cdr)   '(a b c))   
    ((cons car cdr)    '(a b c))
    ((list x (when x x))  :errs)
    ;;as
    ((as x) nil)
    ((as _) nil) 
    ((as x _) nil) 
    (  (as)    :errs)
    ( (as x a b)   :errs)
    ((as lx2 (list x x)) '(1 1))
    ((list x (as x _))  '(a a))
    ((list (as x _) x)   '(a a))
    ((list x (list y z))  '(a (b c)))
    ((:list x (list y z) x)  '(a (b c) a))
    ((:list x (list y z) x)   '(a (b c) d)  :not)
    ((when (< x y) (:list x y))  '(1 2))
    ((when (> x y) (:list x y))  '(1 2)  :not)
    ;; ((list* x y) '(a b c d))
    ((list* x (list y z w)) '(a b c d))
    ( (cons x y)   '(a))
    (  (cons x)   :errs)
    ((list x (as x _))  '(1 1))
    ((list x (as x 1))  '(1 1))
    ((list x (as x 2))  '(1 1)  :not)
    ;;array
    ((array 1 ((x y) (z w)))  :errs) ;ranks differ
    ((array 2 ((x y) (z)))    :errs) ;dims differ
    ((array 2 ((x y) (z w)))  (make-array '(2 2) :initial-contents '((1 2) (3 4))))
    ((array (2 string) ((x y) (z w))) (make-array '(2 2) :element-type 'integer
						  :initial-contents '((1 2) (3 4)))  :not)
    ((array (2 integer) ((x y) (z w))) (make-array '(2 2) :element-type 'integer
						  :initial-contents '((1 2) (3 4))))
    ((array 2 ((x y) (y z)))  (make-array '(2 2) :initial-contents '((1 2) (2 1))))
    ((array 2 ((x y) (y z)))  (make-array '(2 2) :initial-contents '((1 2) (3 4)))   :not)
    ;;vector
    ((vec (x y z))  (make-array '(3) :initial-contents '(1 2 3)))
    ((vec (x y))    (make-array '(3) :initial-contents '(1 2 3)) :not)
    ((vec (x y) string)  (make-array '(2) :element-type 'integer :initial-contents '(1 2)) :not)
    ((vec (x y) integer) (make-array '(2) :element-type 'integer :initial-contents '(1 2)))
    ;;or
    ((or 2 3)  1 :not)
    ((list x y (or x y)) '(a b a))
    ((list x (or (when (< x 1) x) (when (> x 1) y)) y)  '(0 0 2))
    ((list x (or (when (< x 1) x) (when (> x 1) y)) y)  '(2 1 1))
    ((list x (or (when (< x 1) x) (when (> x 1) y)))    :errs)
    ;;when
    ((list x (when (> x 0) x))  :errs) ;not top level
    ( (when t)  nil)
    (:quote (let ((val 1)) (matches val (when (= val 1)))))
    ((when (= x 1) x)  1)
    ;;vals
    ((list (vals x y))    :errs)
    (    (vals x x)        (values 1 1))
    ((or (vals x 1) (vals 1 x))  (values 1 1))
    ((or (vals x 2) (vals 2 x))  (values 1 1) :not)
    ((or (vals 2 x) (when (> x 1) (vals 1 x)))  (values 1 2))
  ;;[(values 1 '(2 3))    (patrn-vgt '(or (vals x (list y z)) (vals (list x y) z)))]
    ;;struct
    ((struct)  :errs)
    (                (struct foo- (bar     1)    (gak 2)) (make-foo :bar     1      :gak 2))
    ((when (= x gak) (struct foo- (bar (list x y)) gak))  (make-foo :bar (list 1 2) :gak 1))
    ((struct foo- (a b c))  :errs)
    ((foo (bar 1) (gak 2))  (make-foo :bar 1 :gak 2))
    ;;slots, accsrs
    ((slots (a b c)) :errs)
    ((slots (wuf) (eek)) (make-instance 'urp)) ;default patrn is _
    ((slots  wuf   eek ) (make-instance 'urp) :not) ;slot-boundp prevents error
    ((slots wuf eek)  (make-instance 'urp :wuf 1 :eek 2))
    ((acsrs (wuf) (eek))  (make-instance 'urp))
    ;;type
    ((type) :errs)
    ((type string) "FOO")
    ((type symbol) "FOO" :not)
    ;;quote
    ('a 'a)
    ('b 'a :not)
    ;; match
    (:quote (match '(a b) ((list x x) nil) ((list x y z) nil) ((list x y) t) (_ nil)))
    (:quote (match (values 1 2)
		   ((or (when (> x 1) (vals x 1)) (vals x x)) nil)
		   ((or (when (> x 1) (vals 1 x)) (vals 2 x)) t)
		   (_ nil)))
    (:quote (not (match '(a b) ((list x x) t) ((list x y z) t))))
    ;;not-or
    ;;[1    (not [2 or 3])]
  )

;;(def tm () (test-match))
