;;; standard-cl:  partial application
#|
-------------------------------------------------------------------------
This software is Copyright (c) 2008 Daniel S. Bensen.
Permission is granted to use, copy, modify, and distribute this software,
provided that this copyright and permission notice is included in full
in all copies and supporting documentation.
This software is provided "as is" with no express or implied warranty.
-------------------------------------------------------------------------
|#

(cl:in-package :standard-cl)

;; #f(func _ arg _)
;; #f[_ op _]

;;from GOO
;;http://people.csail.mit.edu/jrb/goo/manual.46/goomanual_15.html#17
;;creates an anonymous function with implicitly defined arguments,
;; where ,op-arg is either an implicit required parameter "_" or rest parameter "..."
;; or an s-expression potentially containing further op-args.
;; The required parameters are found ordered according to a depth-first walk of the op-args.
;;((op _) 1) ==> 1 
;;((op 2) 1) ==> 2 
;;((op + _ 1) 3) ==> 4 
;;((op lst ... 1) 3 2) ==> (3 2 1) 
;;((op tail (tail _)) '(1 2 3)) ==> (3) 

;;TODO: composition: [f @ g]
;; (defun @ (f &rest fs) ...)

(defun |read-#func| (stream char n)
  (declare (ignore char)) 
  (let (vars outforms (informs (read stream t :eof t)))
    (flet ((pushvar ()
	     (let ((var (gensym)))
	       (push var vars)
	       (push var outforms))))
      (dolist (x informs)
	(if (and (symbolp x) (string= (symbol-name x) "_"))
	    (pushvar)
	    (push x outforms)))
      (dotimes (_ (or n 0))
	(pushvar)))
    `(lambda ,(nreverse vars) (,@(nreverse outforms)))))

