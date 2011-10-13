;; standard-cl:  a standard libary for Common Lisp
#|
-------------------------------------------------------------------------
This software is Copyright (c) 2008 Daniel S. Bensen.
Permission is granted to use, copy, modify, and distribute this software,
provided that this copyright and permission notice is included in full
in all copies and supporting documentation.
This software is provided "as is" with no express or implied warranty.
-------------------------------------------------------------------------
|#

(cl:declaim (optimize debug))

(cl:in-package :standard-cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro eval-always (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      ,@body))

  (defmacro xport (&rest symbols)
    `(eval-always (export ',symbols)))

  (defmacro macx (macro args &body body)
    `(progn
      (defmacro ,macro ,args ,@body)
      (xport ,macro)))

  (defmacro defmac (macro args &body body) `(defmacro ,macro ,args ,@body))

  (defmac in-defpackage (name &rest options)
    `(progn
      (defpackage ,name ,@options)
      (in-package ,name)))
  
;;; GENSYMS

  (defmac with-gensyms (items &body body)
    `(let ,(mapcar (lambda (x) (if (atom x)
				   `(,x (gensym))
				   `(,(car x) (gensym ,(cadr x)))))
		   items)
      ,@body))
  
  (defmac with-gensym (item &body body) `(with-gensyms (,item) ,@body))

;;; looping

  (defmac    while (test &body body) `(loop (unless ,test (return)) ,@body))
  (defmac    until (test &body body) `(loop ( when  ,test (return)) ,@body))
  (defmac do-while (test &body body) `(loop ,@body (unless ,test (return))))
  (defmac do-until (test &body body) `(loop ,@body ( when  ,test (return))))

;;; BINDING

  (defmac lett (x expr &body body)
    (if (atom x)
	`(let ((,x ,expr)) ,@body)
	`(destructuring-bind ,x ,expr ,@body)))

  (defmac letvals (vars expr &body body)
    `(multiple-value-bind ,vars ,expr ,@body))

;;(defsetf if (test if-true if-false) (expr)
;;  (with-gensym val
;;    `(let ((,val ,expr))
;;      (if ,test (setf ,if-true ,val) (setf ,if-false ,val)))))

;;; DEFs

(defmac def (func args &body defn)
  (let (preamble (body defn))
    (when (listp body)
      (when (stringp (car body)) (push (pop body) preamble))
      (while (and (listp (car body))
		  (eq (caar body) 'declare)) (push (pop body) preamble)))
    (with-gensym (val "GRETURN-VAL")
      (setf body
	    (list `(lett gfuncname (symbol-name ',func)
		    (declare (ignorable gfuncname))
		    (macrolet ((greturn (&optional (,val (values)))
				 `(return-from ,',func ,,val))) ;; C-style return
		      ,@body)))))
    (when preamble
      (setf body (revappend preamble body)))
    `(defun ,func ,args ,@body)))

(defmac   defx (f xs &body body) `(progn                       (def ,f ,xs ,@body) (export ',f)))
(defmac indef  (f xs &body body) `(progn (declaim (inline ,f)) (def ,f ,xs ,@body)))
(defmac indefx (f xs &body body) `(progn (declaim (inline ,f)) (def ,f ,xs ,@body) (export ',f)))
);end eval-when
