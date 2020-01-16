(defpackage :lisp-preprocessor-tests.in-template
  (:use :cl)
  (:export :object
           :object-reader))
(in-package :lisp-preprocessor-tests.in-template)

(defclass object ()
  ((reader
    :initarg :reader
    :reader object-reader
    :initform 0)))
