;;;; slummer.asd

(asdf:defsystem #:slummer
  :description "Lisp visits the Browser."
  :author "Boutade <thegoofist@protonmail.com>"
  :license  "AGPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:parenscript #:lass #:spinneret)
  :components ((:file "package")
               (:file "slummer")))
