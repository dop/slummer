(in-package :slummer)

(defvar *slummer-ps-lib*
'(progn

;;; The core virtual DOM type and some utility functions
(defmodule *slummer*


    (defun elem (tag &optional (properties ({})) (children ([])))
      "TAG is an html tag, PROPS is an object and CHILDREN is an array of elements
 The whole structure represents a DOM tree fragment."
      ({} tag tag
          properties properties
          ;; if children are not elems they are implicitly converted to strings
          children (mapcar (lambda (child)
                             (if (not (@ child tag))
                                 (+ "" child)
                                 child))
                           children)))

  (defun elem-prop (el prop)
    "A handy function to retrieve the property named PROP from the PROPERTIES
field of EL."
    (getprop el 'properties prop))

  (defun elems-diff? (el1 el2)
  "Returns T if EL1 and EL2 are different enough to require re-rendering."
    (or (not (equal (typeof el1) (typeof el2)))
        (and (stringp el1) (not (equal el1 el2)))
        (not (equal (@ el1 tag) (@ el2 tag)))))


  ;;; Checking Virtual DOM state and chainging the real DOM.

  (defun event-property? (prop)
    "Returns T if PROP is the name of an event handler property.
E.g. 'onclick' or 'onkeydown'."
    (chain prop (starts-with "on")))

  (defun property->event (prop)
    "Turns an event property name PROP into an event name.
E.g. (property->event :onclick) should return 'click'. Parenscript doesn't seem
to distinguish between keywords and strings."
    (chain prop (slice 2)))

  (defun remove-property (node prop value)
    "NODE is a real DOM element, PROP and VALUE are what is being removed."
    (cond ((booleanp value)
           (chain node (remove-attribute prop))
           (setf (getprop node prop) false))
          ((functionp value)
           (chain node (remove-event-listener (property->event prop) value)))
          (t
           (chain node (remove-attribute prop)))))

  (defun realize-property (node prop value)
    "NODE is a real DOM element, and PROP and VALUE are being added."
    (cond ((event-property? prop)
           (chain node (add-event-listener (property->event prop) value)))
          ((booleanp value)
           (when value (chain node (set-attribute prop value)))
           (setf (getprop node prop) value))
          (t
           (chain node (set-attribute prop value)))))

  ;; TODO figure out a good way to compare lambda values
  (defun update-property (node prop old-val new-val)
    "Handles changes in PROP's value on the real DOM element NODE."
    (if (not new-val)
        (remove-property node prop old-val)
        (when (not (equal old-val new-val))
          (realize-property node prop new-val))))

  (defun keys-for (&rest objects)
    "A utility function for combining the keys of OBJECTS and returning them as
an array."
    (let ((index ({}))
          (keys (list)))
      (dolist (ob objects)
        (for-in (key ob)
                (unless (getprop index key)
                  (setf (getprop index key) t)
                  (chain keys (push key)))))
      keys))


  (defun realize-elem (el)
    "The main DOM node builder. Takes an ELEM called El and returns a new DOM
node."
    (if (stringp el)
        (chain document (create-text-node el))
        (let ((new-node (chain document (create-element (@ el tag)))))
          (for-in (prop (@ el properties))
                  (realize-property new-node prop (elem-prop el prop)))
          (dolist (child (@ el children))
            (chain new-node (append-child (realize-elem child))))
          new-node)))


 (defun update-properties (node old-props new-props)
    (dolist (prop (keys-for old-props new-props))
      (update-property node
                       prop
                       (getprop old-props prop)
                       (getprop new-props prop))))


  (defun update-elem (parent-node old-elem new-elem &optional (child-index 0))

    (let ((child-node (getprop parent-node 'child-nodes child-index)))
      (cond ((not old-elem)
             ;; if there is no old element we just append a new one
             (chain parent-node (append-child (realize-elem new-elem))))

            ((not new-elem)
             ;; if there is no new element we remove the node from the DOM
             (chain parent-node (remove-child child-node)))

            ((elems-diff? new-elem old-elem)
             ;; if the elements differ, we replace the child-node with fresh node
             (chain parent-node
                    (replace-child (realize-elem new-elem) child-node)))

            ((not (stringp new-elem)) ; if we have a non-string node
             ;; first we update the child node's properties
             (update-properties child-node
                                (@ old-elem properties)
                                (@ new-elem properties))
             ;; then we recursively  update the child node's own children
             (let* ((new-length (@ new-elem children length))
                    (old-length (@ old-elem children length))

                    (max-len (max new-length old-length)))
               (dotimes (idx max-len)
                 (update-elem child-node
                              (getprop old-elem 'children (- max-len 1 idx))
                              (getprop new-elem 'children (- max-len 1 idx))
                              (- max-len 1 idx))))))))


 (defun query (arg)
   "Query the DOM. If the query is in CSS id attribute notation, then a single
element is returned, otherwise an array of matches is returned."
   (if (equal (elt arg 0) "#")
       (@> document (query-selector arg))
       (@> -Array (from (@> document (query-selector-all arg))))))

 (defun on (ob evt handler)
   (let ((ob (if (stringp ob) (query ob) ob)))
     (@> ob (add-event-listener evt handler))))

 (defun attach-view (view attachment)
   (setf (@> view attachment)
         (if (stringp attachment)
             (@> document (get-element-by-id attachment))
             attachment))
   (render-view view))

 (defun render-view (view)
   (let ((new-virtual (@> view (render))))
     (update-elem (@> view attachment) (@> view virtual) new-virtual)
     (setf (@> view virtual) new-virtual)))

  (export elem render-view attach-view on query)) ; end defmodule *slummer*


;;; HTML builders for virtual DOM elements
;;; see https://developer.mozilla.org/en-US/docs/Web/HTML/Element
(defmodule (*slummer* *html*)

  (import-from *slummer* elem)

  (defelems
      footer header h1 h2 h3 h4 h5 h6 nav section)

  ;; text content
  (defelems
      blockquote dd div dl dt figcaption figure hr li ol p pre ul)

  ;; inline text semantics
  (defelems
      a b br code em i q  s small span strong sub sup time )

  ;; multimedia
  (defelems
      audio img track video)

  ;; canvas
  (defelems canvas)

  ;; forms
  (defelems
      button datalist fieldset form input label legend meter
    optgroup option select textarea)

  (export
   footer header h1 h2 h3 h4 h5 h6 nav section
   blockquote dd div dl dt figcaption figure hr li ol p pre ul
   a b br code em i q  s small span strong sub sup time
   audio img track video
   canvas
   button datalist fieldset form input label legend meter
   optgroup option select textarea )

  ;;; Some Handy Utilities

  (defun list->ul (props ls &optional map-fn)
    "Takes a PROPS object and a LS and produces a UL element. Optionally,
accepts a MAP-FN argument that should turn the members of LS into ELEMs"
    (if (not map-fn)
        (elem "ul" props ls)
        (elem "ul" props (mapcar map-fn ls))))

  (defun list->ol (props ls &optional map-fn)
    (if (not map-fn)
        (elem "ol" props ls)
        (elem "ol" props (mapcar map-fn ls))))


  (export list->ul list->ol)) ; ends SLUMMER.HTML


;;; Odds and ends - Utilities.
(defmodule (*slummer* *util*)
 "utility library"
 (defun ->string (arg)
   (+ "" arg))

 (defun cons (x xs)
   "XS is assumed to be a javascript array"
   (@> xs (unshift x))
   xs)

 (defun list (&rest args)
   args)

  (export ->string cons list)) ; end of SLUMMER.UTIL

;;; Creating and selecting random values.
(defmodule (*slummer* *random*)
 "Random operations"

 (defun pick (ary)
   (elt ary (random (length ary))))

 (defun pick-pop (ary)
   (let* ((idx (random (length ary)))
          (val (elt ary idx)))
     (@> ary (slice idx 1))
     val))

 (defun rand (&optional lo hi)
   (cond ((not lo) (random))
         ((not hi) (random lo))
         (true
          (+ lo (random (- hi lo))))))

 (export pick pick-pop rand)) ; end of SLUMMER.RANDOM


;;; JSON Serialization
(defmodule (*slummer* *json*)

 (defun ->json (ob)
   (@> *json* (stringify ob)))

 (defun parse (str)
   (@> *json* (parse str)))

 (export ->json parse)) ; end of SLUMMER.JSON


;;; Basic graphics utilities
(defmodule (*slummer* *graphics*)

 (defstruct color (red 0) (green 0) (blue 0) (alpha 1.0))

 (defun random-color ()
   (make-color :red (random 256)
               :green (random 256)
               :blue (random 256)))

 (defun color->string (color)
   (with-slots (red blue green alpha) color
     (+ "rgba(" red "," green "," blue "," alpha  ")")))


 (export random-color color->string
  make-color color-red color-green color-blue color-alpha)) ; end of SLUMMER.GRAPHICS


;; Working with html5 canvas as a drawing surface, low level
(defmodule (*slummer* *graphics* *surface*)
 "Canvas primitives"

 (defstruct surface context canvas)

 (defun new-surface (&optional dom-elem)
   (let ((canvas
           (cond ((null dom-elem) (@> document (create-element "canvas")))
                 ((stringp dom-elem) (@> *slummer* (query dom-elem)))
                 (t from-dom))))
     (make-surface :canvas canvas
                   :context (@> canvas (get-context "2d")))))

 (macrolet
     ((defaccessors (&rest specs)
        (let ((expanded
                (mapcan (lambda (spec)
                          (destructuring-bind (prop sub-prop) spec
                            (let ((dname (read-from-string (format nil "surface-~a" sub-prop))))
                              (list
                               `(defun ,dname (surface)
                                  (@> surface ,prop ,sub-prop))
                               `(defun (setf ,dname) (newval surface)
                                  (setf (@> surface ,prop ,sub-prop) newval))
                               `(export ,dname)))))
                        specs)))
          `(progn ,@expanded))))
   (defaccessors
     (canvas height)
     (canvas width)
     (context fill-style)
     (context line-style)
     (context line-width)
     (context line-cap)
     (context line-join)
     (context miter-limit)
     (context font)
     (context text-align)
     (context text-baseline)
     (context direction)))

 (macrolet
     ((ctx-proxy (&rest names)
        (cons 'progn
              (mapcan (lambda (name)
                        (list `(defun ,name (surface &rest args)
                                 (apply (ps:@ surface context ,name) args))
                              `(export ,name)))
                      names))))
   (ctx-proxy fill-rect stroke-rect clear-rect
              begin-path close-path stroke fill
              move-to line-to arc arc-to
              quadratic-curve-to bezier-curve-to
              fill-text stroke-text
              draw-image
              save restore
              translate rotate scale-float))

 (defun clear-surface (surface &optional color)
   (if color
       (progn
         (save surface)
         (setf+ (surface-fill-style surface) color)
         (fill-rect surface 0 0 (surface-width surface) (surface-height surface))
         (restore surface))
       (clear-rect surface 0 0 (surface-width surface) (surface-height surface))))

 (export new-surface clear-surface)) ;; end of (*slummer* *graphics* *surface*)

(defmodule (*slummer* *net*)
  "Some networking tools."

 (defun xhr (url &key
                   (method "GET")
                   (response-type "text")
                   payload
                   on-error
                   on-load)
    "Make an XHR request to URL calling ON-LOAD on the response. The default
    METHOD is the string \"GET\", and the default PAYLOAD is NIL."
   (let ((req (ps:new (-X-M-L-Http-Request))))
      (setf (@> req response-type) response-type)
      (when on-load
        (let ((handler (lambda () (funcall on-load (@> req response)))))
          (@> req (add-event-listener "load" handler))))
      (when on-error
        (@> req (add-event-listener "error" on-error)))
      (@> req (open method url))
      (@> req (send payload))))

  (defun ws (url on-message &key hello)
    "Creates a new WebSocket connection to URL and attaches the ON-MESSAGE
    handler to handle incoming messages. Optionally send the HELLO message on
    opening the connection. The URL should look like ws:://addr[:PORT]/other-stuff"
    (let ((con (ps:new (-web-socket url))))
      (@> con (add-event-listener "message" on-message))
      (when hello
        (@> con (add-event-listener "open" (lambda () (@> con (send hello))))))
      con))

 (export xhr ws)) ; end defmodule slummer.net

;; the following two lines close the top-level defvar
))

