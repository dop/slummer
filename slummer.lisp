;;;; slummer.lisp

(named-readtables:in-readtable :parenscript)
(in-package #:slummer)

(defpsmacro {} (&rest args)
  `(ps:create ,@args))

(defpsmacro @> (&rest args)
  `(ps:chain ,@args))

(defpsmacro defelems (&rest names)
  (unless (null names)
    `(progn
      (defun ,(car names) (props &rest children)
        (elem ,(string-downcase (symbol-name (car names))) props children))
      (defelems ,@(cdr names)))))


(defpsmacro defapp (name &rest setup)
  `(defun ,name (attachment)
     (let* ((*state* nil)
            (*view* nil)
            (*virutal* nil)
            (*attachment* attachment)
            (*render* (lambda ()
                        (let ((new-virtual (*view*)))
                          (chain *slummer* (update-elem *attachment* *virtual* new-virtual))
                          (setf *virtual* new-virtual)))))
       (progn ,@setup)
       (setf *virtual* (*view*))
       (chain *slummer* (update-elem *attachment* nil *virtual*)))))

(defpsmacro defactive (name lambda-list &rest body)
  `(defun ,name ,lambda-list
     (progn ,@body)
     (*render*)))

(defpsmacro defview (name view-form)
  `(defun ,name () ,view-form))

(defpsmacro defmodule (name &rest body)
  (cond
    ((symbolp name)
     `(defvar ,name
        ((lambda ()
           (let ((*exports* ({})))
             (progn ,@body)
             *exports*)))))

    ((listp name)
     `(setf (@ ,@name)
            ((lambda ()
               (let ((*exports* ({})))
                 (progn ,@body)
                 *exports*)))))))



(defpsmacro export (&rest names)
  `(progn
     ,@(mapcar (lambda (name)
                 (list 'setf (list '@ '*exports* name) name))
               names)))

(defpsmacro import-from (module-name &rest symbs)
  `(progn ,@(mapcar (lambda (s)
                      (if (symbolp module-name)
                          (list 'defvar s (list '@ module-name s))
                          (list 'defvar s (append (cons '@ module-name) (list s)))))
                    symbs)))
