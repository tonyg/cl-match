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

(def gensyms (n) (loop repeat n collect (gensym)))

(defmac eif ;Elisp-style IF (if* conflicts w/allegro)
 (test then &body elses) `(if ,test ,then (progn ,@elses)))

;;; bindings

(eval-always
  (def 2list<-plist (plist)
    (loop for var in plist
	  and expr in (cdr plist)
	  collect (list var expr)))
)
(defmac letts (plist &body body)
  (eif (no plist)
      `(progn ,@body)
    (lett bindings (nreverse (2list<-plist plist))
      (assert bindings (bindings) "LETTS: malformed binding list")
      (lett binding (car bindings)
	(setf body `(lett ,(car binding) ,(cadr binding) ,@body))
	(dolist (binding (cdr bindings) body)
	  (setf body `(lett ,(car binding) ,(cadr binding) ,body)))))))

(def external-symbols (&optional (package *package*))
  (lett symbols nil
    (do-external-symbols (symbol package (nreverse symbols))
      (push symbol symbols))))

;;; abbrevs

(def no (x) (declare (inline null))  (null x))
(defmac concat   (&rest args) `(concatenate  ,@args))
(def concstr (&rest strings) (apply #'concatenate 'string strings))
(def ask-yn     (str) (declare (inline  y-or-n-p  )) ( y-or-n-p   str))
(def ask-yes-no (str) (declare (inline yes-or-no-p)) (yes-or-no-p str))
(defmac defparam (&rest args) `(defparameter ,@args))

;;(defmac fn (args &body body) `(lambda  ,args ,@body))
;;(defmac fc (func &rest args) `(funcall ,func ,@args))

(def debug-out (mesg) mesg);; (write-line mesg))

(def  echo  (&rest args) (apply #'format t args) (terpri))
(def prompt (&rest args) (apply #'format t args) (finish-output))

(def sethash (key table val) (setf (gethash key table) val))

(defmac for ((var init test incr) &body body)
      `(lett ,var ,init
	(while ,test
	  ,@body
	  (setf ,var ,incr))))

(defmac docells ((cell list &optional rzult) &body body)
      `(do ((,cell ,list (cdr ,cell)))
	(,cell ,rzult)
      ,@body))

(defmac dopairs ((x y list) &body body)
      (with-gensym cell
	`(lett ,x (car ,list)
	  (docells (,cell (cdr ,list))
	   (dolist (,y (cdr ,cell))
	     ,@body
	     (setf ,x (car ,cell)))))))

(defmac do-hashtable ((key val table &optional (rtrnval nil)) &body body)
  (with-gensyms (get-keyval got-one do-hashtable)
    `(block ,do-hashtable
      (with-hash-table-iterator
	  (,get-keyval ,table) 
	(loop
	 (multival-bind (,got-one ,key ,val) (,get-keyval)
			(when (not ,got-one) (return-from ,do-hashtable ,rtrnval))
			,@body))))))

(def pause (&rest args)
  (when args (apply #'prompt args))
  (read-line))

(defmac dotimes1 ((n nmax &optional result) &body body)
  `(do ((,n 1 (1+ ,n)))
       ((> ,n ,nmax) ,result)
       ,@body))

(defmac dorange ((n min top &optional result) &body body)
  `(do ((,n ,min (1+ ,n)))
       ((>= ,n ,top) ,result)
	,@body))

(def range (n-min n-upr) "[n-min...n-upr]"
  (let (vals)
    (do ((n (1- n-upr) (1- n)))
	((< n n-min) vals)
      (push n vals))))

(def range* (min max) (range min (1+ max)))

(def times  (n) "[0...n]" (range  0 n))
(def times1 (n) "[1..n]"  (range* 1 n))

(defmac count-list ((n val list &optional result) &body body)
  (with-gensym cell
    `(do* ((,n 0 (+ ,n 1))
	   (,cell      ,list  (cdr ,cell))
	   (,val  (car ,cell) (car ,cell)))
	  ((null ,cell) ,result)
	  ,@body)))

(def split-list (pred items)
  (let (goods bads)
    (dolist (item items)
      (if (funcall pred item)
	  (push item goods)
	  (push item bads)))
    (values (nreverse goods)
	    (nreverse bads))))

(def xorf (a b) (if a (not b) b))

(def  orf (&rest vals) (dolist (val vals nil) (when   val (return val))))
(def andf (&rest vals)
  (lett rzult nil
    (dolist (val vals rzult) (if val (setf rzult val) (return nil)))))

(def /_ (x y) (floor x y))
(def /. (x y) (float (/ x y)))

(defmac def-length-op (func op)
  (with-gensyms (a b)
    `(def ,func (,a ,b) (,op (length ,a) (length ,b)))))

(def-length-op length< <)
(def-length-op length= =)
(def-length-op length> >)

(def sum  (vals) (reduce #'+ vals))
(def prod (vals) (reduce #'* vals))

(def e^  (x) (exp     x))
(def 2^  (x) (expt  2 x))
(def 10^ (x) (expt 10 x))

(def avg (xs)
  (assert (and (listp xs) xs) (xs) "AVG:  Need at least one number.") 
  (/. (sum xs) (length xs)))

(def variance (xs)
  (assert (and (listp xs) (cdr xs)) (xs) "VARIANCE:  Need at least two numbers.") 
  (let* ((N (length xs))
	 (avg (/. (sum xs) N))
	 (sum (sum (mapcar (lambda (x) (expt (- x avg) 2))
			       xs))))
    (values (/. sum (1- N)) avg)))

(def rms (xs) (sqrt (variance xs)))

(defmacro def-defthings (defthings defthing)
  `(defmac ,defthings (&rest things)
    (let ((defs (mapcar (lambda (thing) (if (atom thing)
					`(,',defthing  ,thing)
					`(,',defthing ,@thing)))
			things)))
      `(progn ,@defs))))

(def-defthings defvars      defvar     )
(def-defthings defparams    defparam   )
(def-defthings defconstants defconstant)

(let ((lcs #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	     #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
      (ucs #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	     #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)))
  (defun is-case (letr letrs)
    (dotimes (n 26 nil)
      (when (char= letr (aref letrs n))
	(return t))))
  (def is-lc (letr) (is-case letr lcs))
  (def is-uc (letr) (is-case letr ucs))
  (def is-letr (char) (or (is-lc char) (is-uc char)))
  (def lc<-int (n) (aref lcs n))
  (def uc<-int (n) (aref ucs n))
  )

(def int<-letr (c)
  (case c
	((#\A #\a)  0) ((#\B #\b)  1) ((#\C #\c)  2) ((#\D #\d)  3) ((#\E #\e)  4)
	((#\F #\f)  5) ((#\G #\g)  6) ((#\H #\h)  7) ((#\I #\i)  8) ((#\J #\j)  9)
	((#\K #\k) 10) ((#\L #\l) 11) ((#\M #\m) 12) ((#\N #\n) 13) ((#\O #\o) 14)
	((#\P #\p) 15) ((#\Q #\q) 16) ((#\R #\r) 17) ((#\S #\s) 18) ((#\T #\t) 19)
	((#\U #\u) 20) ((#\V #\v) 21) ((#\W #\w) 22) ((#\X #\x) 23) ((#\Y #\y) 24)
	((#\Z #\z) 25)))

(defun make-? (forms)
  (and forms `(if ,(car forms) ,(cadr forms) ,(make-? (cddr forms)))))

(defmac ? (forms) (make-? forms))

(def flatten (list) (apply #'nconc list))

(defmac npushlist   (list place) `(setf ,place (   nconc  ,list ,place)))
(defmac pushlist    (list place) `(setf ,place (   append ,list ,place)))
(defmac pushrevlist (list place) `(setf ,place (revappend ,list ,place)))

(def trim-whitespace (string) (string-trim '(#\Space #\Tab #\Newline) string))
(def trim-linespace  (string) (string-trim '(#\Space #\Tab)           string))

(defmac pushwhen (expr place)
  (with-gensym val
    `(lett ,val ,expr
      (when ,val (push ,val ,place)))))

(defmac letwhen (var pred &body body)
  `(lett ,var ,pred
     (when ,var ,@body)))

(defmac setwhen (oldval place newval)
  `(when (eq ,place ,oldval) (setf ,place ,newval)))

(defmac cond-eql (val &rest tests)
  (flet ((make-case (test)
	   (let ((case (car test))
		 (prog (cdr test))) `((eql ,val ,case) ,@prog))))
    (let ((cases
	   (mapcar #'make-case tests)))
      `(cond
	,@cases))))

(defmac strcase (expr &rest clauses)
  (with-gensym val
  `(lett ,val ,expr
    (cond
      ,@(mapcar (lambda (clause) (if (eq (car clause) t)        `(t ,@(cdr clause))
				 `((string= ,val ,(car clause)) ,@(cdr clause))))
		clauses)))))

(defmac dovector ((n val vectr &optional rzult) &body body)
  `(dotimes
    (,n (length ,vectr) ,rzult)
    (let ((,val (aref ,vectr ,n))) ,@body)))

(def string-or-symbol-name (x)
  (cond ((stringp x) x)
	((symbolp x) (symbol-name x))
	(t (error "string-or-symbol-name: Not a string or symbol"))))

(defun letstruct-testable (testing type- slots expr body)
  "Deconstruct a struct.  Testing for struct existance is optional."
  (lett prefix (string-or-symbol-name type-)
    (with-gensym val
      (flet ((slotbinding (slot)
	       (let* ((is-atom (atom slot))
		      (struct-slot 
		       (find-symbol (concstr prefix
					     (symbol-name (if is-atom slot
							      (cadr slot))))))
		      (slotname	(if is-atom slot (car slot)))
		      (slotexpr1 (list struct-slot val))
		      (slot-expr (if testing
				     `(and ,expr ,slotexpr1)
				     slotexpr1)))
		 (list slotname slot-expr))))
	(if slots
	    `(lett ,val ,expr
	      (let ,(mapcar #'slotbinding slots) ,@body))
	    `(lett ,val ,expr                    ,@body))))))

(defmac letstruct    ((type- . slots) x &body body) (letstruct-testable nil type- slots x body))
(defmac letstruct-if ((type- . slots) x &body body) (letstruct-testable  t  type- slots x body))

(def |read_case| (case)
  (lett readtable (copy-readtable nil)
    (setf (readtable-case readtable) case)
    (setf *readtable* readtable)))

(def cwd () (truename "."))

(def cd (pathstr)
;;  (lett char0 (char dir 0)
    (setf *default-pathname-defaults*
	  (pathname pathstr)))
	;;  (if [char0 char= #\/]
	  ;;    dir
	    ;;  (concatenate 'string
		;;	   *default-pathname-defaults*
			;;   dir)))))

(def quit ()
  #+allegro (excl:exit)
  #+sbcl  (sb-ext:quit)
  #+clisp    (ext:quit)
  #-(or allegro sbcl clisp) (error "Don't know how to quit."))
