(in-package :lisp-preprocessor.stream)

(defparameter +tab-width+ 8)

(defun add-character-width (character column)
  (cond ((char= character #\tab)
         (+ (* (floor column +tab-width+) +tab-width+) +tab-width+))
        (t
         (1+ column))))

(defclass emitter (fundamental-output-stream)
  ((column
    :initform 0
    :accessor emitter-column)
   (indent
    :initform 0
    :accessor emitter-indent)
   (stream
    :initarg :stream
    :initform *standard-output*
    :reader emitter-stream)))

(defmethod stream-element-type ((stream emitter))
  'character)

(defmethod stream-write-char ((stream emitter) char)
  (cond ((char= char #\newline)
         (setf (emitter-column stream) 0))
        (t
         (when (zerop (emitter-column stream))
           (loop :repeat (emitter-indent stream)
                 :do (write-char #\space (emitter-stream stream))))
         (setf (emitter-column stream) (add-character-width char (emitter-column stream)))))
  (write-char char (emitter-stream stream))
  char)

(defmethod stream-write-string ((stream emitter) string &optional start end)
  (loop :for c :across (subseq string start end)
        :do (write-char c stream))
  string)

(defmethod stream-line-column ((stream emitter))
  (emitter-column stream))

(defun call-with-indent (emitter indent function)
  (let ((old-indent (emitter-indent emitter)))
    (setf (emitter-indent emitter) indent)
    (unwind-protect (funcall function)
      (setf (emitter-indent emitter) old-indent))))

(defmacro with-indent (emitter indent &body body)
  `(call-with-indent ,emitter ,indent (lambda () ,@body)))
