(in-package #:stantler)

(defmacro with-gensyms (names &body body)
  `(let ,(mapcar (lambda (name) `(,name (gensym ,(symbol-name name)))) names)
     ,@body))

(defun filter-type (type sequence)
  (loop for element in sequence
	when (typep element type)
	  collect element))

(defclass children-mixin ()
  ((children%
    :accessor children
    :initarg :children)))

(defclass child-mixin ()
  ((child%
    :accessor child
    :initarg :child)))

(defun slurp-file (path)
  "Reads a text file into string."
  (with-open-file (stream path :direction :input  :element-type 'character)
    (let ((chars (make-array (file-length stream) :element-type 'character)))
      (read-sequence chars stream)
      chars)))

;;; Platform compatible #\Newline replacement ==================================

(defconstant +return+ (code-char #xA)
  "Carriage Return character")

(defconstant +newline+ (code-char #xD)
  "Newline character")

;;; End of File Handlers =======================================================

(define-condition eof-error (error)
  ((index%
    :reader index
    :initarg :index))
  (:documentation "Error for reading past the end of a character or token stream."))

(defgeneric look-ahead (input start &optional count)
  (:documentation "Utility function to read the `count'th element of `input'.
Signals `eof-error' when reading past the end of the input.")
  (:method ((input array) start &optional (count 0))
    (let ((index (+ start count)))
      (if (>= index (array-total-size input))
	  (error 'eof-error :index index)
	  (aref input index))))
  (:method ((input list) start &optional (count 0))
    (let* ((index (+ start count))
	   (input* (nthcdr index input)))
      (if input*
	  (first input*)
	  (error 'eof-error :index index)))))

(defmacro with-no-eof-match (&body body)
  "Evaluates body. If an `eof-error' is signaled, the error is captured and this form evaluates to NIL."
  `(handler-case (progn ,@body)
     (eof-error ()
       nil)))
