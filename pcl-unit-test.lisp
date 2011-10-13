;; Based on the test framework in
;; Practical Common Lisp
;; by Peter Siebel

(std:in-defpackage :pcl-unit-test (:use :cl :std)
		   (:export :deftest :all-okay :test-forms :errs))

(defvar *test-name* nil)

(use-std-readtable)

(defmac deftest (name parameters &body body)
  `(defun ,name ,parameters
    (lett *test-name* (cons ',name *test-name*)
      ,@body)))

(def pass-fail (rzult form)
  (echo "~:[***FAIL***~;   pass   ~] ~a ~a" rzult form *test-name*)
  rzult)

(defmac all-okay (&body forms)
  (with-gensym okay
    `(lett ,okay t
      ,@(mapcar (lambda (form) `(unless ,form (setf ,okay nil)))
		forms)
      ,okay)))

(defmac test-forms (&body forms)
  `(all-okay
     ,@(mapcar (lambda (form) `(pass-fail ,form ',form))
	       forms)))

(defmac errs (&body body) `(not (ignore-errors ,@body t)))

;;(deftest test-fail () (check nil))

(deftest test-pcl ()
  (test-forms
    [[2 + 2] = 4]
    ;;(not (test-fail))
    ))
