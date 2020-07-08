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
                  (return
                    (let (next-line-pos)
                      (cond ((setq next-line-pos (blank-line-p text (end+ pos)))
                             (values (tweak (nreverse template-form))
                                     next-line-pos))
                            ((setq next-line-pos (end-of-line-p text (end+ pos)))
                             (values (tweak (nreverse template-form))
                                     next-line-pos
                                     t))
                            (t
                             (values (tweak (nreverse template-form))
                                     (end+ pos)))))))))))

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
          :do (unless (= start pos) (push (list :string (subseq text start pos)) forms))
              (with-input-from-string (in text)
                (file-position in (begin+ pos))
                (multiple-value-bind (template-form next fresh-line-p)
                    (read-template-form in text (compute-column text pos))
                  (setq end next)
                  (push template-form forms)
                  (when fresh-line-p
                    (push '(:fresh-line) forms))))
          :finally (push (list :string (subseq text start)) forms))
    (nreverse forms)))

(defun reintern-symbol (symbol)
  (intern (string symbol) *in-template-package*))

(defun empty-string-p (string)
  (not (find-if (lambda (c)
                  (not (find c +whitespaces+)))
                string)))

(defun chop-in-forms (forms)
  (loop :with temporary-forms := '()
        :and new-forms := '()
        :and chop-flag := nil
        :for form :in forms
        :do (trivia:match form
              ((list :string string)
               (if chop-flag
                   (push (list :string (string-left-trim +whitespaces+ string)) temporary-forms)
                   (push form temporary-forms)))
              ((list :fresh-line)
               (unless chop-flag
                 (push form temporary-forms)))
              ((list :chop)
               (let ((acc '()))
                 (loop :for form :in temporary-forms
                       :do (trivia:match form
                             ((list :string string)
                              (let ((string (string-right-trim +whitespaces+ string)))
                                (if (zerop (length string))
                                    nil
                                    (progn
                                      (push (list :string string) acc)
                                      (return)))))
                             ((list :fresh-line)
                              nil)
                             (otherwise
                              (return))))
                 (setq temporary-forms '())
                 (setq new-forms (append acc new-forms)))
               (setq chop-flag t))
              (otherwise
               (setq chop-flag nil)
               (setq new-forms
                     (append temporary-forms
                             new-forms))
               (push form new-forms)
               (setq temporary-forms '())))
        :finally (return (nreverse (append temporary-forms new-forms)))))

(defun compile-template (string-or-pathname
                         &key ((:template-begin *template-begin*) *template-begin*)
                              ((:template-end *template-end*) *template-end*)
                              ((:in-template-package *in-template-package*) *in-template-package*)
                              arguments)
  (let ((*package* *in-template-package*)
        (forms (load-template string-or-pathname))
        (output "")
        (fresh-line-reservation nil)
        (compiled-forms '()))
    (labels ((emit (string)
               (setq output (concatenate 'string output string)))
             (fresh-line-if-reserved ()
               (when fresh-line-reservation
                 (push '(fresh-line) compiled-forms)
                 (setq fresh-line-reservation nil))))
      (loop :for form :in (chop-in-forms forms)
            :do (trivia:ematch form
                  ((list :string string)
                   (fresh-line-if-reserved)
                   (emit string))
                  ((list* :indent indent lisp-forms)
                   (fresh-line-if-reserved)
                   (push `(write-string ,output) compiled-forms)
                   (setq output "")
                   (push `(lisp-preprocessor.in-template:with-indent ,indent . ,lisp-forms)
                         compiled-forms))
                  ((cons :form lisp-forms)
                   (fresh-line-if-reserved)
                   (push `(write-string ,output) compiled-forms)
                   (setq output "")
                   (trivia:match lisp-forms
                     ((list (type symbol))
                      (push `(princ ,(first lisp-forms)) compiled-forms))
                     (otherwise
                      (dolist (form lisp-forms) (push form compiled-forms)))))
                  ((list :fresh-line)
                   (setq fresh-line-reservation t))))
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
