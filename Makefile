build: slummer.lisp package.lisp slummer-ps-lib.lisp slummer.asd
	sbcl --load slummer.asd \
         --eval '(ql:quickload :slummer)' \
         --eval '(in-package :slummer)' \
         --eval "(sb-ext:save-lisp-and-die #p\"slummer\" :toplevel #'slum-it :executable t)"

