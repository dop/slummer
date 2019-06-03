;;;; slummer.asd

(asdf:defsystem #:slummer
  :description "Describe slummer here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:parenscript #:lass #:spinneret)
  :components ((:file "package")
               (:file "slummer")))
