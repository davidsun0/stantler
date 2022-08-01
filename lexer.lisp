(in-package #:stantler)

(defun slurp-file (path)
  (with-open-file (stream path
			  :direction :input
			  :element-type 'character)
    (let ((chars (make-array (file-length stream)
			      :element-type 'character)))
      (read-sequence chars stream)
      chars)))

(defgeneric match (input rule start)
  (:documentation "Matches `rule` on `input` at index `start`.
Returns the number of items matched or NIL if the rule fails to match.
Rules that sucessfully match no items return 0."))

(define-condition eof-error (error)
  ((index%
    :reader index
    :initarg :index)))

(defun look-ahead (input start &optional (count 0))
  (let ((index (+ start count)))
    (if (> index (array-total-size input))
	(error 'eof-error :index index)
	(aref input index))))

(defclass literal-rule ()
  ((value%
    :reader value
    :initarg :value)
   (comparison%
    :reader comparison
    :initarg :comparison))
  (:documentation "Matches a literal or array of literals against the input."))

(defclass object-literal-rule (literal-rule) ())

(defmethod match (input (rule object-literal-rule) (start integer))
  (if (funcall (comparison rule) (value rule) (look-ahead input start))
      1
      nil))

(defclass array-literal-rule (literal-rule) ())

(defmethod match (input (rule array-literal-rule) (start integer))
  (loop for needle across (value rule)
	    for offset from 0
	    unless (funcall (comparison rule)
			    needle
			    (look-ahead input start offset))
	      return nil
	    finally (return (length (value rule)))))

(defclass char-range-rule ()
  ((low%
    :reader low
    :initarg :low)
   (high%
    :reader high
    :initarg :high)))

(defmethod match (input (rule char-range-rule) (start integer))
  (if (char< (low rule) (look-ahead input start) (high rule))
      1
      nil))

(defclass token ()
  ((content%
    :reader content
    :initarg :content)
   (name%
    :reader name
    :initarg :name)))

(defclass named-rule (child-mixin) ())

(defmethod match (input (rule named-rule) start)
  (let ((count (match input (child rule) start)))
    (make-instance 'token
		   :name (name rule)
		   :content (cons start count))))
