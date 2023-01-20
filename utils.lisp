(in-package #:stantler)

(defclass children-mixin ()
  ((children%
    :reader children
    :initarg :children)))

(defclass child-mixin ()
  ((child%
    :reader child
    :initarg :child)))

(defun slurp-file (path)
  "Reads a text file into string."
  (with-open-file (stream path
			  :direction :input
			  :element-type 'character)
    (let ((chars (make-array (file-length stream)
			      :element-type 'character)))
      (read-sequence chars stream)
      chars)))

;; Compatibility because #\Newline is platform-dependent

(defconstant +return+ (code-char #xA)
  "Carriage Return character")

(defconstant +newline+ (code-char #xD)
  "Newline character")

;;; End of File ================================================================

(define-condition eof-error (error)
  ((index%
    :reader index
    :initarg :index))
  (:documentation "Error for reading past the end of a character or token stream."))

(defun look-ahead (input start &optional (count 0))
  "Utility function to read (aref input (+ start count)).
Signals eof-error if upper bounds check fails."
  (let ((index (+ start count)))
    (if (>= index (array-total-size input))
	(error 'eof-error :index index)
	(aref input index))))

(defmacro with-no-eof-match (&body body)
  "Evaluates to NIL when an eof-error is signaled."
  `(handler-case (progn ,@body)
     (eof-error ()
       nil)))
