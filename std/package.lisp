;; standard-cl package
#|
---------------------------------------------------------------
;; This software is Copyright (c) 2008 Daniel S. Bensen.
;; You are hereby granted permission to distribute and use this software
;; as governed by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
---------------------------------------------------------------
|#

(cl:defpackage
    :standard-cl (:use :cl) (:nicknames :std)
    (:export 
     ;;syntax
     :make-std-readtable :use-std-readtable
     ;;base
     :in-defpackage :eval-always :xport
     :defmac :macx :with-gensyms :with-gensym
     :def :defx :indef :indefx :greturn :gfuncname
     :while :until :do-while :do-until
     :lett :letvals
     ;;standard
     :gensyms :eif :2list<-plist :letts :external-symbols
     :no :concat :concstr :ask-yn :ask-yes-no :defparam
     :debug-out :echo :prompt
     :sethash :docells :dopairs :do-hashtable
     :pause :dotimes1 :dorange :range :range* :times :times1
     :count-list :split-list
     :xorf :orf :andf
     :/_ :/. 
     :length< :length= :length>
     :sum :prod :e^ :2^ :10^
     :avg :variance :rms
     :defvars :defparams :defconstants
     :is-lc :is-uc :is-letr :lc<-int :uc<-int :int<-letr
     :? :flatten :npushlit :pushlist :pushrevlist
     :trim-whitespace :trim-linespace
     :pushwhen :letwhen :setwhen :cond-eql :strcase
     :dovector :string-or-symbol-name
     :letstruct :letstruct-if
     :cwd :cd :quit
     ;;2fix
     :isnum :isint :isfloat
     :islist :isstring :isvector :isarray :has-length
     :=0 :>0 :<0 :!=0 :<=0 :>=0 
     :+= :*= :or= :and= :not= :xor=
     :^ :^2 :^3 :^4 :^5 :^6 :^7 :^8  
     :is-tail-of :is-subtype-of :has-type
     :is-adjustable-array      :is-alpha-char    :isalphanum    :has-array-fill-pointer
     :is-array-in-bounds
     :isatom    :is-bit-vector    :is-both-case    :isbound    :ischar
     :is-compiled-func    :iscomplex    :iscons    :isconstant
     :isdigchar    :isend    :iseven    :isfbound     :isfunc    :is-hashtable    :iskeyword
     :is-lower-case    :isodd    :ispackage    :ispathname
     :isrational    :isreadtable    :isreal    :is-simple-string    :is-simple-vector
     :isstream    :is-symbol    :is-upper-case    :is-wild-pathname
     
     ))
