(defpackage :lisp-preprocessor-tests
  (:use :cl :rove :lisp-preprocessor))
(in-package :lisp-preprocessor-tests)

(defparameter *sample-file* (asdf:system-relative-pathname :lisp-preprocessor-tests "./tests/sample"))

(defun load-sample-file (file)
  (with-open-file (in file)
    (let ((output (make-string-output-stream))
          (count 0)
          (input-samples '())
          (output-samples '()))
      (flet ((f ()
               (let ((text (get-output-stream-string output)))
                 (if (evenp (incf count))
                     (push text output-samples)
                     (push text input-samples)))))
        (loop :for line := (read-line in nil)
              :while line
              :do (cond ((string= line (string #\Page))
                         (f))
                        (t
                         (write-line line output))))
        (f)
        (mapcar #'list
                (nreverse input-samples)
                (nreverse output-samples))))))

(deftest tests
  (let ((samples (load-sample-file *sample-file*)))
    (loop :for (input-text output-text) :in samples
          :do (ok (string= (run-template-into-string input-text)
                           output-text)))))

(deftest argument-test
  (ok (string= "10" (run-template-into-string (compile-template "#{ (princ ?) #}"
                                                                :arguments (list '?))
                                              10))))
