;;; standard-cl:  syntax
#|
-------------------------------------------------------------------------
This software is Copyright (c) 2008 Daniel S. Bensen.
Permission is granted to use, copy, modify, and distribute this software,
provided that this copyright and permission notice is included in full
in all copies and supporting documentation.
This software is provided "as is" with no express or implied warranty.
-------------------------------------------------------------------------
|#

(in-package :standard-cl)

(def make-std-readtable ()
  (lett table (copy-readtable nil)
    (set-macro-character #\[      #'|read-[2-fix]|         nil table) 
    (set-macro-character #\] (get-macro-character #\) nil) nil table)
    (set-dispatch-macro-character #\# #\f #'|read-#func|       table)
    table))

(defmac use-std-readtable ()
  '(eval-when (:execute :compile-toplevel)
    (setf *readtable* (make-std-readtable))))
