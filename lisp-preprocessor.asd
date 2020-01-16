(defsystem "lisp-preprocessor"
  :license "MIT"
  :author "cxxxr"
  :description "Common Lisp embedded template engine"
  :depends-on ("alexandria"
               "split-sequence"
               "trivial-gray-streams"
               "trivia"
               "cl-ppcre")
  :serial t
  :components ((:file "packages")
               (:file "stream")
               (:file "lisp-preprocessor")))

(defsystem "lisp-preprocessor-tests"
  :depends-on ("lisp-preprocessor"
               "rove")
  :serial t
  :pathname "tests"
  :components ((:file "in-template")
               (:file "tests"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
