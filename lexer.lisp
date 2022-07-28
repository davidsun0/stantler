(defpackage #:stantler
  (:use :cl))
(in-package #:stantler)

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("cl-unicode")))
|#

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

(defun restore (input-stream &optional (count 1))
  (decf (pointer input-stream) count))

(defun look-ahead (input-stream count)
  (aref (content input-stream)
	(+ (pointer input-stream) count)))

(defclass string-literal-rule ()
  ((value%
    :reader value
    :initarg :value)))

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

(defmethod match ((rule char-ragne-rule) input)
  (let ((head (aref (content input) (pointer input))))
    (if (char< (low rule) head (high rule))
	(prog1 head
	  (take input))
	nil)))
