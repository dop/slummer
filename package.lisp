;;;; package.lisp

(in-package :cl)
(named-readtables:in-readtable :parenscript)

(defpackage #:slummer
  (:use #:cl #:parenscript)
  (:export
   ;; convenience parenscript macros
   #:@>
   #:{}
   #:setf+
   #:let-slots
   #:with-methods
   #:with-object
   #:defstruct

   ;; defines functions for semantic virtual dom element creation, should this
   ;; even be exported?
   #:defelems

   ;; modularity macros
   #:defmodule
   #:export
   #:export-struct
   #:import-from
   #:import-struct-from

   ;; reactive making applications
   #:defview
   #:defstate
   #:defroute

   ;; making sites & pages
   #:with-site-context
   #:fresh-site
   #:defpage
   #:defscript
   #:defstyle
   #:include
   #:build-site
   ))

