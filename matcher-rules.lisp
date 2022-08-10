(in-package #:stantler)

;;; General Rules =====================================================

(defmacro with-no-eof-match (&body body)
  `(handler-case (progn ,@body)
     (eof-error ()
       nil)))

(defclass literal-rule ()
  ((value%
    :reader value
    :initarg :value)
   (comparison%
    :reader comparison
    :initarg :comparison)))

(defclass object-literal-rule (literal-rule)
  ()
  (:documentation "Matches a single object literal."))

(defmethod match (input (rule object-literal-rule) start)
  (with-no-eof-match
    (if (funcall (comparison rule)
		 (value rule)
		 (look-ahead input start))
	1
	nil)))

(defclass array-literal-rule (literal-rule)
  ()
  (:documentation "Matches an array of literals."))

(defmethod match (input (rule array-literal-rule) start)
  (with-no-eof-match
    (loop for needle across (value rule)
	  for offset from 0
	  unless (funcall (comparison rule)
			  needle
			  (look-ahead input start offset))
	    return nil
	  finally (return (length (value rule))))))

(defclass named-rule (child-mixin)
  ((name%
    :reader name
    :initarg :name)))

(defmethod match (input (rule named-rule) start)
  (match input (child rule) start))

;;; Special Rules =====================================================

(defclass wildcard-rule () ())

(defmethod match (input (rule wildcard-rule) start)
  (if (look-ahead input start)
      1
      nil))

(defclass eof-rule () ())

(defmethod match ((input array) (rule eof-rule) (start integer))
  (if (> start (array-total-size input))
      0
      nil))

;;; Compound Rules ====================================================

(defclass or-rule (children-mixin)
  ()
  (:documentation "Matches the first applicable child rule."))

(defmethod match (input (rule or-rule) start)
  (loop for child in (children rule)
	do (let ((count (match input child start)))
	     (when count
	       (return count)))
	finally (return nil)))

(defclass and-rule (children-mixin)
  ()
  (:documentation "Matches if all children match consecutively, from left to right."))

(defmethod match (input (rule and-rule) start)
  (let ((total 0))
    (loop for child in (children rule)
	  do (let ((count (match input child (+ start total))))
	       (if count
		   (incf total count)
		   (return nil)))
	  finally (return total))))

(defclass not-rule (child-mixin)
  ()
  (:documentation "Matches if the child rule doesn't match."))

(defmethod match (input (rule not-rule) start)
  (if (match input (child rule) start)
      nil
      1))

(defclass maybe-rule (child-mixin)
  ()
  (:documentation "Matches the child rule or nothing."))

(defmethod match (input (rule maybe-rule) start)
  (let ((count (match input (child rule) start)))
    (if token
	token
	0)))

(defclass repeat-rule (child-mixin)
  ()
  (:documentation "Matches the child multiple times."))

(defmethod match (input (rule repeat-rule) start)
  (let ((total 0))
    (loop (let ((count (match input (child rule) (+ start total))))
	    (if count
		(incf total count)
		;; Return zero when matched zero times
		(return total))))))

(defclass lazy-repeat-rule (repeat-rule)
  ((stop%
    :reader stop
    :initarg :stop))
  (:documentation "Matches child rule repeatedly until the stop rule matches."))

(defmethod match (input (rule lazy-repeat-rule) start)
  (let ((total 0))
    (loop (let ((stop (match input (stop rule) (+ start total))))
	    (when stop
	      (return (+ total stop))))
	  (let ((count (match input (child rule) (+ start total))))
	    (if count
		(incf total count)
		(return nil))))))
