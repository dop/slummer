;;;; package.lisp

(in-package :cl)
(named-readtables:in-readtable :parenscript)

(defpackage #:slummer
  (:use #:cl #:parenscript)
  (:export
   #:{}
   #:defapp
   #:defelems
   #:defmodule
   ))

;; (defpackage #:slummer.paren
;;   (:use #:parenscript #:slummer))
