;;;; package.lisp

(in-package :cl)
(named-readtables:in-readtable :parenscript)

(defpackage #:slummer
  (:use #:cl #:parenscript)
  (:export
   ;; convenience macros
   #:@>
   #:{}

   ;; defines functions for semantic virtual dom element creation
   #:defelems

   ;; modularity macros
   #:defmodule
   #:export
   #:import-from

   ;; reactive making applications
   #:defapp
   #:defactive
   #:defview

   ;; making pages
   #:*js-root*
   #:*css-root*
   #:*resource-root*
   #:defpage
   ))

