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
    (loop :for (input-text expected-text) :in samples
          :for actual-text := (run-template-into-string input-text)
          :do (ok (string= actual-text expected-text)
                  #+(or)(format nil "input:~%~A~%expected:~%~A~%actual:~%~A~%" input-text expected-text actual-text)))))

(deftest argument-test
  (ok (string= "10" (run-template-into-string (compile-template "#{ (princ ?) #}"
                                                                :arguments (list '?))
                                              10))))

(deftest chop-test
  (ok (string= "foobar" (run-template-into-string "foo

#{ :chop #}

bar"))))

(deftest change-template-marker-test
  (ok (string= "123"
               (run-template-into-string (compile-template "{{{ (princ 123) }}}"
                                                           :template-begin "{{{"
                                                           :template-end "}}}")))))

(deftest change-template-package-test
  (ok (string= "test"
               (run-template-into-string
                (compile-template "#{ (princ (object-reader $arg)) #}"
                                  :in-template-package (find-package :lisp-preprocessor-tests.in-template)
                                  :arguments '($arg))
                (make-instance 'lisp-preprocessor-tests.in-template:object
                               :reader "test")))))
