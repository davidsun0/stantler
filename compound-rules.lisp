(in-package #:stantler)

(defclass children-mixin ()
  ((children%
    :reader children
    :initarg :children)))

(defclass child-mixin ()
  ((child%
    :reader child
    :initarg :child)))

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

(defclass or-rule (children-mixin)
  ()
  (:documentation "Matches the first child rule from the left."))

(defmethod match ((rule or-rule) input)
  (loop for child in (children rule)
	do (let ((token (match child input)))
	     (when token
	       (take input)
	       (return token)))
	finally (return nil)))

(defclass and-rule (children-mixin)
  ()
  (:documentation "Matches if all children match consecutively, from left to right."))

(defmethod match ((rule and-rule) input)
  (let ((tokens '()))
    (loop for child in (children rule)
	  do (let ((token (match child input)))
	       (cond
		 (token
		  (take input)
		  (push token tokens))
		 (t
		  (restore input (length tokens))
		  (return nil))))
	  finally (return nil))))

(defclass maybe-rule (child-mixin)
  ((default%
    :reader default
    :initarg :default
    :initform t))
  (:documentation "Matches the child rule or returns the default value."))

(defmethod match ((rule maybe-rule) input)
  (let ((token (match (child rule) input)))
    (cond
      (token
       (take input)
       token)
      (t
       (default rule)))))

(defclass repeat-rule (child-mixin)
  ()
  (:documentation "Matches the child multiple times."))

(defmethod match ((rule repeat-rule) input)
  (loop with token = (match (child rule) input)
	if token
	  do (take input)
	  and collect token into tokens
	else
	  return tokens))

(defclass lazy-repeat-rule (repeat-rule)
  ((stop%
    :reader stop
    :initarg :stop))
  (:documentation "Matches child rule repeatedly until the stop rule matches."))

(defmethod match ((rule lazy-repeat-rule) input)
  (let ((tokens '()))
    (loop (let ((token (match (child rule) input)))
	    (cond
	      (token
	       (take input)
	       (push token tokens))
	      (t
	       (let ((stop (match (stop rule) input)))
		 (cond
		   (stop
		    (take input)
		    (push stop tokens)
		    (return (nreverse tokens)))
		   (t
		    (restore (length tokens))
		    (return nil))))))))))

(defmethod match ((rule lazy-repeat-rule) input)
  (let ((tokens '()))
    (loop (let ((stop (match (stop rule) input)))
	    (when stop
	      (return (nreverse (cons stop tokens)))))
	  (let ((token (match (child rule) input)))
	    (cond
	      (token
	       (push token tokens))
	      (t
	       (restore (length tokens))
	       (return nil)))))))
