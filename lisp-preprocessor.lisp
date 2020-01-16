(in-package :lisp-preprocessor)

(defparameter +whitespaces+ '(#\newline #\space #\tab))

(defparameter *in-template-package* (find-package :lisp-preprocessor.in-template))

(defparameter *template-begin* "#{")
(defparameter *template-end* "#}")

(defun begin+ (pos)
  (+ pos (length *template-begin*)))

(defun end+ (pos)
  (+ pos (length *template-end*)))

(defun blank-line-p (text pos)
  (let ((start (if-let (pos (position #\newline text :end pos :from-end t))
                 (1+ pos)
                 0))
        (end (position #\newline text :start pos)))
    (when (string= (string-trim +whitespaces+ (subseq text start end))
                   *template-end*)
      (1+ end))))

(defun end-of-line-p (text pos)
  (multiple-value-bind (start end)
      (ppcre:scan "^\\s*\\n" text :start pos)
    (when start
      end)))

(defun read-template-form (stream text column)
  (labels ((tweak (template-form)
             (trivia:match template-form
               ((list :chop)
                template-form)
               ((list* :indent (trivia:guard indent (integerp indent)) template-form)
                (list* :indent (+ column indent) template-form))
               ((cons :indent template-form)
                (list* :indent column template-form))
               (otherwise
                (cons :form template-form)))))
    (loop :with template-form := '()
          :for form := (read stream)
          :do (peek-char t stream)
              (let ((pos (file-position stream)))
                (push form template-form)
                (when (string= text *template-end* :start1 pos :end1 (end+ pos))
                  (return (values (tweak (nreverse template-form))
                                  (if-let (next-line-pos (or (blank-line-p text (end+ pos))
                                                             (end-of-line-p text (end+ pos))))
                                    next-line-pos
                                    (end+ pos)))))))))

(defun compute-column (text pos)
  (loop :for i :downfrom pos :to 0
        :for count :from 0
        :do (when (char= #\newline (aref text i))
              (return (1- count)))
        :finally (return pos)))

(defun load-template (string-or-pathname)
  (let ((text (etypecase string-or-pathname
                (pathname (read-file-into-string string-or-pathname))
                (string string-or-pathname)))
        (forms '())
        (*package* *in-template-package*))
    (loop :with end
          :for start := 0 :then end
          :for pos := (search *template-begin* text :start2 start)
          :while pos
          :do (unless (= start pos) (push (subseq text start pos) forms))
              (with-input-from-string (in text :start (begin+ pos))
                (multiple-value-bind (template-form next)
                    (read-template-form in text (compute-column text pos))
                  (setq end next)
                  (push template-form forms)))
          :finally (push (subseq text start) forms))
    (nreverse forms)))

(defun reintern-symbol (symbol)
  (intern (string symbol) *in-template-package*))

(defun compile-template (string-or-pathname
                         &key ((:template-begin *template-begin*) *template-begin*)
                              ((:template-end *template-end*) *template-end*)
                              ((:in-template-package *in-template-package*) *in-template-package*)
                              arguments)
  (let ((*package* *in-template-package*)
        (forms (load-template string-or-pathname))
        (output "")
        (chopping nil)
        (compiled-forms '()))
    (labels ((chop-left (string)
               (if chopping
                   (string-left-trim +whitespaces+ string)
                   string))
             (chop-right (string)
               (string-right-trim +whitespaces+ string))
             (emit (string)
               (setq output (concatenate 'string output (chop-left string)))))
      (dolist (form forms)
        (trivia:ematch form
          ((type string)
           (emit form)
           (setq chopping nil))
          ((list* :indent indent lisp-forms)
           (push `(write-string ,output) compiled-forms)
           (setq output "")
           (push `(lisp-preprocessor.in-template:with-indent ,indent . ,lisp-forms)
                 compiled-forms)
           (setq chopping nil))
          ((cons :form lisp-forms)
           (push `(write-string ,output) compiled-forms)
           (setq output "")
           (trivia:match lisp-forms
             ((list (type symbol))
              (push `(princ ,(first lisp-forms)) compiled-forms))
             (otherwise
              (dolist (form lisp-forms) (push form compiled-forms))))
           (setq chopping nil))
          ((list :chop)
           (setq chopping t)
           (setq output (chop-right output)))))
      (when output
        (push `(write-string ,output) compiled-forms)))
    (setq compiled-forms (nreverse compiled-forms))
    (let ((lambda-form
            (let ((g-stream (gensym "STREAM")))
              `(lambda (,g-stream ,@(mapcar #'reintern-symbol arguments))
                 (with-open-stream (*standard-output* (make-instance 'lisp-preprocessor.stream:emitter
                                                                     :stream ,g-stream))
                   ,@compiled-forms
                   (values))))))
      (let ((*error-output* (make-broadcast-stream)))
        (compile nil lambda-form)))))

(defun ensure-compiled-template (compiland)
  (etypecase compiland
    (function compiland)
    (pathname (compile-template (alexandria:read-file-into-string compiland)))
    (string (compile-template compiland))))

(defun run-template-into-stream (compiland stream &rest arguments)
  (handler-bind ((warning #'muffle-warning))
    (apply (ensure-compiled-template compiland)
           stream
           arguments)))

(defun run-template-into-string (compiland &rest arguments)
  (with-output-to-string (output)
    (apply #'run-template-into-stream compiland output arguments)))

(defun run-template-into-file (compiland file &rest arguments)
  (with-open-file (*standard-output* file
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
    (apply #'run-template-into-stream compiland *standard-output* arguments)))

(in-package :lisp-preprocessor.in-template)

(defun invoke-with-indent (indent function)
  (lisp-preprocessor.stream:with-indent *standard-output* indent
    (funcall function)))

(defmacro with-indent (indent &body body)
  `(Invoke-with-indent ,indent (lambda () ,@body)))
