;;;; slummer.lisp

(named-readtables:in-readtable :parenscript)
(in-package #:slummer)

(defpsmacro {} (&rest args)
  `(ps:create ,@args))

(defpsmacro defelems (&rest names)
  (unless (null names)
    `(progn
      (defun ,(car names) (props &rest children)
        (elem ,(string-downcase (symbol-name (car names))) props children))
      (defelems ,@(cdr names)))))




(defun defs->pairs (defs)
    (mapcar (lambda (def)
              (case (car def)
                ;; (:handler name (event) ... forms ...) -> (name (lambda (event) .. forms ..))
                (:handler (destructuring-bind (name lambda-list . body) (cdr def)
                            (list name (append (list 'lambda lambda-list) body))))
                ;; (:view name view-form) -> (name (lambda () view-form))
                (:view (destructuring-bind (name view-form) (cdr def)
                         (list name (list 'lambda (list) view-form))))))
            defs))


(defpsmacro defapp (name definitions &rest setup)
  `(defun ,name (attachment)
     (let* ((*state* nil)
            (*view* nil)
            (*virutal* nil)
            (*attachment* attachment)
            (*render* (lambda ()
                        (let ((new-virtual (*view*)))
                          (update-elem *attachment* *virtual* new-virtual)
                          (setf *virtual* new-virtual))))
            (get (lambda (prop) (getprop *state* prop)))
            (set (lambda (prop val)
                   (setf (getprop *state* prop) val)
                   (*render*)))))
     (let* ,(defs->pairs definitions)
       (progn ,@setup)
       (setf *virtual* (*view*))
       (update-elem *attachment* nil *virtual))))


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

(defpsmacro import-from (module-name &rest symbols)
  `(progn ,@(mapcar (lambda (s) (list 'defvar s (list '@ module-name s))) symbols)))
