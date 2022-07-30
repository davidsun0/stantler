(in-package #:stantler)

(defclass input-stream ()
  ((content%
    :reader content
    :initarg :content)
   (pointer%
    :accessor pointer
    :initarg :pointer)))

(defun slurp-file (path)
  (with-open-file (stream path
			  :direction :input
			  :element-type 'character)
    (let ((chars (make-array (file-length stream)
			      :element-type 'character)))
      (read-sequence chars stream)
      (make-instance 'input-stream :content chars :pointer 0))))

(defun take (input-stream &optional (count 1))
  (incf (pointer input-stream) count))

(defun look-ahead (input-stream count)
  (aref (content input-stream)
	(+ (pointer input-stream) count)))

(defmethod eof-p ((input input-stream))
  (>= (pointer input) (length (content input))))

(defclass literal-rule ()
  ((value%
    :reader value
    :initarg :value)))

(defclass char-literal-rule (literal-rule) ())

(defmethod match ((rule char-literal-rule) input)
  ( (char= (value rule) (look-ahead input 0))

(defclass string-literal-rule (literal-rule) ())

(defmethod match ((rule string-literal-rule) input)
  (let ((needle (value rule)))
    (cond
      ((string= needle (content input)
		:start2 (pointer input)
		:end2 (+ (pointer input) (length needle)))
       (take input (length needle))
       needle)
      (t nil))))

(defclass char-range-rule ()
  ((low%
    :reader low
    :initarg :low)
   (high%
    :reader high
    :initarg :high)))

(defmethod match ((rule char-range-rule) input)
  (let ((head (aref (content input) (pointer input))))
    (if (char< (low rule) head (high rule))
	(prog1 head
	  (take input))
	nil)))

(defclass token ()
  ((content%
    :reader content
    :initarg :content)
   (name%
    :reader name
    :initarg :name)))

(defclass named-rule (child-mixin) ())

(defmethod match ((rule named-rule) input)
  (match (child rule) input))

(defun stringify (string-designators)
  "Concatentates a list of string designators into a single string."
  (apply #'concatenate 'string (mapcar #'string string-designators)))
