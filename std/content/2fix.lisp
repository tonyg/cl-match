;;; "2-fix" syntax reader macro, with associated functions
;;; by Dan Bensen
#|
-------------------------------------------------------------------------
This software is Copyright (c) 2008 Daniel S. Bensen.
Permission is granted to use, copy, modify, and distribute this software,
provided that this copyright and permission notice is included in full
in all copies and supporting documentation.
This software is provided "as is" with no express or implied warranty.
-------------------------------------------------------------------------
|#

;;(defpackage :2fix (:use :cl)
;;	    (:export isatom islist iscons ischar isnum isint
;;		     += *= or= and= not= xor=
;;		     ^ ^2 ^3 ^4 ^5 ^6 ^7 ^8 def-op= =0 >0 <0 >=0 <=0 !=0))
;;(in-package :2fix)

(in-package :standard-cl)

(defun |read-[2-fix]| (stream char) 
  (declare (ignore char)) 
  (let* ((list (read-delimited-list #\] stream t))
	 (op (cadr list)))
    ;;(unless op (error "No operator in 2-fix: ~S" list))
    (list* op (car list) (cddr list))))

;; predicates

(eval-always
  (defmac def-isnumeric (istype typep)
    `(def ,istype (x &key = > < >= <=)
      (and (,typep x)
       (if = (= x =)
	   (and (if > (> x >) (or (not >=) (>= x >=)))
		(if < (< x <) (or (not <=) (<= x <=)))))))))

(def-isnumeric isnum  numberp)
(def-isnumeric isint integerp)
(def-isnumeric isfloat floatp)
(def-isnumeric isreal   realp)

(eval-always
  (def has-length (x n)
    (assert (typep x 'sequence)  (x) "has-length: not a sequence")
    (assert (isint n :>= 0)  (n)    "has-length: bad length")
    (if (not (listp x))
	(= (length x) n)
	(if (zerop n)
	    (not x)
	    (lett last-cdr (nthcdr (1- n) x)
	      (and last-cdr (not (cdr last-cdr)))))))

  (defmac def-isseq (isseq seqp)
    `(def ,isseq (x &key length min-length max-length)
      (and (,seqp x)
       (or (not (or length min-length max-length))
	(let ((L (length x)))
	  (if length (= L length)
	      (and (or (not min-length) (>= L min-length))
		   (or (not max-length) (<= L max-length)))))))))

  (def-isseq isstring stringp)
  (def-isseq isvector vectorp)

  (def islist (x &key length min-length max-length)
    (and (listp x)
	 (or (not (or length min-length max-length))
	     (if length
		 (if (zerop length) (not x)
		     (let ((last-cell (nthcdr (1- length) x)))
		       (and last-cell (not (cdr last-cell)))))
		 (if (or (not   min-length)
			 (zerop min-length))
		     (not (and max-length (nthcdr max-length x)))
		     (let ((cdr-min (nthcdr (1- min-length) x)))
		       (and cdr-min
			    (not (and max-length
				      (nthcdr (- (1+ max-length) min-length) cdr-min))))))))))
)
(def isarray (x &key dims rank)
  (and (arrayp x)
       (if dims
	   (equal dims (array-dimensions x))
	   (or (not rank)
	       (= (array-rank x) rank)))))

;;; synonyms

(macrolet
    ((def-synonyms (&body pairs)
	 (let ((x (gensym)))
	   `(progn
	     ,@(loop for pair in pairs
		     collect `(defun ,(car pair) (,x)
				 (declare (inline ,(cadr pair)))
				 (,(cadr pair) ,x)))))))
  (def-synonyms
      (is-adjustable-array adjustable-array-p)
      (is-alpha-char alpha-char-p)
    (isalphanum alphanumericp)
    (has-array-fill-pointer array-has-fill-pointer-p)
    (is-array-in-bounds array-in-bounds-p)
    (isatom atom)
    (is-bit-vector bit-vector-p)
    (is-both-case both-case-p)
    (isbound boundp)
    (ischar characterp)
    ;;  (iscommon commonp)
    (is-compiled-func compiled-function-p)
    (iscomplex complexp)
    (iscons consp)
    (isconstant constantp)
    (isdigchar digit-char-p)
    (isend endp)
    (iseven evenp)
    (isfbound fboundp)
    (isfunc functionp)
    (is-hashtable hash-table-p)
    (iskeyword keywordp)
    (is-lower-case lower-case-p)
    (isodd oddp)
    (ispackage packagep)
    (ispathname pathnamep)
    (isrational rationalp)
    (isreadtable readtablep)
    (is-simple-string simple-string-p)
    (is-simple-vector simple-vector-p)
    ;; (slot-exists slot-exists-p)
    ;; (is-special-form special-form-p)
    (isstream streamp)
    (is-symbol symbolp)
    (is-upper-case upper-case-p)
    (is-wild-pathname wild-pathname-p)
    (=0 zerop)
    (>0 plusp)
    (<0 minusp)
    ))

(macrolet ((def-synonyms-2 (&body pairs)
  (let ((x (gensym))
	(y (gensym)))
    `(progn
      ,@(loop for pair in pairs
	      collect `(defun ,(car pair) (,x ,y)
			(declare (inline ,(cadr pair)))
			(,(cadr pair) ,x ,y)))))))
(def-synonyms-2
  (is-tail-of tailp)
  (is-subtype-of subtypep)
  (has-type typep)
))
  
(def !=0 (x) (not (zerop  x)))
(def <=0 (x) (not (plusp  x)))
(def >=0 (x) (not (minusp x)))
  
(macrolet ((def-op= (op= op)
	       `;;(progn
		 (defmacro ,op= (place val)
		   `(setf ,place (,',op ,place ,val)))
		 ;;(export ',op=)
));)
  (def-op=  += +) (def-op=  -= -)
  (def-op=  *= *) (def-op= //= /)  
  (def-op=  or= orf)
  (def-op= and= andf)
  (def-op= not= not)
  (def-op= xor= xorf))

(def ^ (a b) (expt a b))
(def ^2 (x) (* x x))
(def ^3 (x) (* x x x))
(def ^4 (x) (let ((x^2 (* x x)))      (* x^2 x^2)))
(def ^5 (x) (let ((x^2 (* x x))) (* x (* x^2 x^2))))

(def ^6 (x) (let* ((x^2 (* x x)) (x^4 (* x^2 x^2)))      (* x^4 x^2)))
(def ^7 (x) (let* ((x^2 (* x x)) (x^4 (* x^2 x^2))) (* x (* x^4 x^2))))
(def ^8 (x) (let* ((x^2 (* x x)) (x^4 (* x^2 x^2)))      (* x^4 x^4)))

#|

(macrolet ((def-synonyms-2+ (&body pairs)
  (let ((x  (gensym))
	(y  (gensym))
	(ys (gensym)))
    `(progn
      ,@(loop for pair in pairs
	      collect `(progn
			(defun ,(car pair) (,x ,y &rest ,ys)
			  (declare (inline ,(cadr pair)))
			  (apply #',(cadr pair) ,x ,y ,ys))
			(export ',(car pair))))))))
  (def-synonyms-2+
      (is-char-greater char-greaterp  ) (not-char-greater  char-not-greaterp   )
    (is-char-less      char-lessp     ) (not-char-less      char-not-lessp     )
    (is-string-greater string-greaterp) (not-string-greater string-not-greaterp)
    (is-string-less    string-lessp   ) (not-string-less    string-not-lessp   )
    (issubset subsetp)
    ))
|#