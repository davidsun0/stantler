(in-package #:stantler)

;;; Special Rules

(defclass wildcard-rule () ())

(defmethod match ((rule wildcard-rule) input)
  (prog1
      (aref (content input) (pointer input))
    (take input)))

(defclass eof-rule () ())

(defmethod match ((rule eof-rule) input)
  (< (length (content input)) (pointer input)))

;;; Compound Rules

(defclass compound-rule () ())

(defmethod match :around ((rule compound-rule) (input input-stream))
  (let* ((pointer (pointer input))
	 (match-results (call-next-method rule input)))
    ;; Reset input to before match if matching fails
    (unless match-results
      (setf (pointer input) pointer))
    match-results))

(defclass or-rule (compound-rule children-mixin)
  ()
  (:documentation "Matches the first child rule from the left."))

(defmethod match ((rule or-rule) input)
  (loop for child in (children rule)
	do (let ((token (match child input)))
	     (when token
	       (return token)))
	finally (return nil)))

(defclass and-rule (compound-rule children-mixin)
  ()
  (:documentation "Matches if all children match consecutively, from left to right."))

(defmethod match ((rule and-rule) input)
  (let ((tokens '()))
    (loop for child in (children rule)
	  do (let ((token (match child input)))
	       (if token
		   (push token tokens)
		   (return nil)))
	  finally (return tokens))))

(defclass maybe-rule (child-mixin)
  ((default%
    :reader default
    :initarg :default
    :initform t))
  (:documentation "Matches the child rule or returns the default value."))

(defmethod match ((rule maybe-rule) input)
  (let ((token (match (child rule) input)))
    (if token
	token
	(default rule))))

(defclass repeat-rule (child-mixin)
  ()
  (:documentation "Matches the child multiple times."))

(defmethod match ((rule repeat-rule) input)
  (loop with token = (match (child rule) input)
	if token
	  collect token into tokens
	else
	  ;; Repeat for zero times is still a sucessful match
	  return (if tokens tokens t)))

(defclass lazy-repeat-rule (repeat-rule)
  ((stop%
    :reader stop
    :initarg :stop))
  (:documentation "Matches child rule repeatedly until the stop rule matches."))

(defmethod match ((rule lazy-repeat-rule) input)
  (let ((tokens '()))
    (loop (let ((stop (match (stop rule) input)))
	    (when stop
	      (return (nreverse (cons stop tokens)))))
	  (let ((token (match (child rule) input)))
	    (if token
		(push token tokens)
		(return nil))))))
