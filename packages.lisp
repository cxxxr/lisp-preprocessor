(defpackage :lisp-preprocessor
  (:use :cl :alexandria :split-sequence)
  (:export :*in-template-package*
           :compile-template
           :run-template-into-stream
           :run-template-into-string
           :run-template-into-file))

(defpackage :lisp-preprocessor.stream
  (:use :cl :trivial-gray-streams)
  (:export :emitter
           :with-indent))

(defpackage :lisp-preprocessor.in-template
  (:use :cl :alexandria :split-sequence)
  (:export :$request
           :with-indent))
