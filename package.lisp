;;;; package.lisp

(in-package :cl)
(named-readtables:in-readtable :parenscript)

(defpackage #:slummer
  (:use #:cl #:parenscript)
  (:export
   ;; convenience macros
   #:@>
   #:{}
   #:let-slots

   ;; defines functions for semantic virtual dom element creation, should this
   ;; even be exported?
   #:defelems

   ;; modularity macros
   #:defmodule
   #:export
   #:import-from

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

