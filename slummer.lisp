;;;; slummer.lisp

(named-readtables:in-readtable :parenscript)
(in-package #:slummer)

;;; Parenscript Macros

;; NB: The following macro exists b/c ps:{} wasn't working for some reason
(defpsmacro {} (&rest args)
  "A convenience macro for building object literals."
  `(ps:create ,@args))

(defpsmacro @> (&rest args)
  "A convenience macro aliasing ps:chain."
  `(ps:chain ,@args))

(defpsmacro defelems (&rest names)
  "Used to define virtual DOM elements constructors en masse."
  (unless (null names)
    `(progn
      (defun ,(car names) (props &rest children)
        (elem ,(string-downcase (symbol-name (car names))) props children))
      (defelems ,@(cdr names)))))


(defpsmacro defapp (name &rest setup)
  "Define an application, returns a function that accepts a DOM node. In the
body of your definition you must (SETF *VIEW* <something>) in order for your
app to work properly."
  `(defun ,name (attachment)
     (let* ((*state* nil)
            (*view* nil)
            (*virutal* nil)
            (*attachment* (if (stringp attachment)
                              (@> document (get-element-by-id attachment))
                              attachment))
            (*render* (lambda ()
                        (let ((new-virtual (*view*)))
                          (chain *slummer* (update-elem *attachment* *virtual* new-virtual))
                          (setf *virtual* new-virtual)))))
       (progn ,@setup)
       (setf *virtual* (*view*))
       (chain *slummer* (update-elem *attachment* nil *virtual*)))))

(defpsmacro defactive (name lambda-list &rest body)
  "A macro to be called within the body of a DEFAPP. Creates a function that,
after having been called, will re-render the DOM."
  `(defun ,name ,lambda-list
     (progn ,@body)
     (*render*)))

(defpsmacro defview (name view-form)
  "Defines a thunk that returns a virtual DOM element."
  `(defun ,name () ,view-form))

(defpsmacro defmodule (name &rest body)
  "Defines a unit of code, meant to encapsulate hidden state and functions. NAME
can be either a symbol or a list of symbols. In the latter case, the list
represents a module path to the module being defined."
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
  "To be called within the body a DEFMODULE. Exports a NAMES from the containing module."
  `(progn
     ,@(mapcar (lambda (name)
                 (list 'setf (list '@ '*exports* name) name))
               names)))

(defpsmacro import-from (module-name &rest symbs)
  "To be called from within the body of DEFMODULE. Imports names into the
current module. Each member of SYMBS can be either a symbol or a pair of
symbols. In the case of the example pair (EXTERNAL LOCAL) the EXTERNAL symbol
is bound to the LOCAL symbol.  This lets you avoid name conflicts."
  `(progn ,@(mapcar (lambda (s)
                      (let ((local (if (symbolp s) s (cadr s)))
                            (foreign (if (symbolp s) s (car s))))
                        (if (symbolp module-name)
                            (list 'defvar local (list '@ module-name foreign))
                            (list 'defvar local (append (cons '@ module-name) (list foreign))))))
                      symbs)))


;;; Spinneret Macros & Functions

(defvar *js-root* ""
  "This special variable designates a URL root prepended to javascript file
  names passed to DEFPAGE.")

(defvar *css-root* ""
  "This special variable designates a URL root prepended to stylsheet file names
  that are passed to DEFPAGE")

(defvar *resource-root* "/" )

(defvar *site-data* '())

(defvar *site-file-root* ""
  "The root directory, from the perspective of the browser, used in generating URLs.")

(defvar *source-root* ""
  "The root directory, relative to to current working directory, where project code is located")


(defun make-scripts (&optional source-names)
  (mapcar (lambda (s)
            (list :tag :name "script"
                       :attrs `(list :src (concatenate 'string *js-root* ,s))))
          (append '("ps-prelude.js" "slummer.js") source-names)))



(defun make-styles (&optional source-names)
  (mapcar (lambda (s)
            (list :tag :name "link"
                       :attrs `(list :rel "stylesheet" :type "text/css"
                                     :href (concatenate 'string *css-root* ,s))))
          source-names))


(defun make-resource (path)
  (concatenate 'string *resource-root* resource))

(defun add-to-site (name thing)
  (push (cons name thing) *site-data*))


(defmacro defpage (path (&key (title "Slumming It") styles scripts)  &body body)
  `(add-to-site
    ,path
    (spinneret:with-html-string
      (:doctype)
      (:html
       (:head
        (:title ,title)
        ,@(make-styles styles))
       (:body
        (:div ,@body)
        ,@(make-scripts scripts))))))



(defmacro define-site-in-context ((&key js css resource) &body body)
  `(let ((*site-data* nil)
         (*js-root* (if ,js ,js *js-root*))
         (*css-root* (if ,css ,css *css-root*))
         (*resource-root* (if ,resource ,resource *resource-root*)))
     (progn ,@body)
     *site-data*))


(defun change-filename-ext (name ext)
  (cl-strings:join
   (reverse
    (cons ext
          (cdr
           (reverse (cl-strings:split name ".")))))
   :separator "."))


(defun include-script (name)
  (cond ((cl-strings:ends-with name ".js")
         (add-to-site  name (alexandria:read-file-into-string name)))

        ((cl-strings:ends-with ".paren")
         (add-to-site (change-filename-ext name "js")
                      (ps:ps-compile-file name)))
        (t
         (error "~s is neither a Javascript nor Parenscript file." name ))))


(defmacro defscript (name &body body)
  `(add-to-site ,name
                (ps:ps ,@body)))

(defun include-style (name)
  (cond ((cl-strings:ends-with ".css")
         (add-to-site name (alexandria:read-file-into-string name)))
        ((cl-strings:ends-with ".lass")
         (error "Inclusion of .lass files not yet implemented"))
        (t
         (error "~s is neither a CSS nor LASS file." name))))

;; TODO
;; defmacro defstyle

(defun include-page (name)
  (cond ((cl-strings:ends-with ".html")
         (add-to-site name (alexandria:read-file-into-string name)))
        ((cl-strings:ends-with ".lisp")
         (cl-add-to-site
          (change-fielname-ext name "html")
          (eval `(spinneret:with-html-string
                   ,(read-from-string
                     (alexandria:read-file-into-string name))))))
        (t
         (error "~s is neither an HTML nor LISP file." name))))


(defun build-site (site-data)
  (loop for (path . content) in site-data
        do (progn
             (let ((filename (concatenate 'string "build/" path)))
               (ensure-directories-exist (directory-namestring filename))
               (alexandria:write-string-into-file content filename :if-exists :supersede)))))

