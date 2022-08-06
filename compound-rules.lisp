(in-package #:stantler)

;;; Special Rules

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

;;; Compound Rules

(defclass compound-rule () ())

(defclass or-rule (compound-rule children-mixin)
  ()
  (:documentation "Matches the first child rule from the left."))

(defmethod match (input (rule or-rule) start)
  (loop for child in (children rule)
	do (let ((count (match input child start)))
	     (when count
	       (return count)))
	finally (return nil)))

(defclass and-rule (compound-rule children-mixin)
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

(defclass not-rule (compound-rule child-mixin)
  ()
  (:documentation "Matches if the child rule doesn't match."))

(defmethod match (input (rule not-rule) start)
  (if (match input (child rule) start)
      nil
      1))

(defclass maybe-rule (compound-rule child-mixin)
  ()
  (:documentation "Matches the child rule or nothing."))

(defmethod match (input (rule maybe-rule) start)
  (let ((count (match input (child rule) start)))
    (if token
	token
	0)))

(defclass repeat-rule (compound-rule child-mixin)
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
