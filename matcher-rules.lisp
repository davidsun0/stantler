(in-package #:stantler)

(defgeneric match (input rule start)
  (:documentation "Matches `rule` on `input` at index `start`.
Returns the number of items matched or NIL if the rule fails to match.
Rules that sucessfully match no items return 0."))

;;; 'NIL represents the no-match rule. It never matches anything.
;;; Useful as a default or dummy rule.
(defmethod match (input (rule null) start)
  nil)

;;; Literal Rules =====================================================

(defclass literal-rule ()
  ((value%
    :reader value
    :initarg :value)
   (comparison%
    :reader comparison
    :initarg :comparison)
   (key%
    :reader key
    :initform 'identity
    :initarg :key)))

(defclass object-literal-rule (literal-rule)
  ()
  (:documentation "Matches a single object literal."))

(defmethod match (input (rule object-literal-rule) start)
  (with-accessors ((comparison comparison) (value value) (key key)) rule
    (with-no-eof-match
      (if (funcall comparison value (funcall key (look-ahead input start)))
	  1
	  nil))))

;; TODO: funcall key w/ array-literal-rule
(defclass array-literal-rule (literal-rule)
  ()
  (:documentation "Matches an array of literals in sequence."))

(defmethod match (input (rule array-literal-rule) start)
  (with-no-eof-match
    (loop for needle across (value rule)
	  for offset from 0
	  unless (funcall (comparison rule)
			  needle
			  (look-ahead input start offset))
	    return nil
	  finally (return (length (value rule))))))

;;; Special Rules =====================================================

;; Ideally the wildcard-rule should be a singleton
(defclass wildcard-rule ()
  ()
  (:documentation "Matches any one object."))

(defmethod match (input (rule wildcard-rule) start)
  (if (look-ahead input start)
      1
      nil))

(defclass eof-rule ()
  ()
  (:documentation "Matches the end of the input."))

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
	for count = (match input child start)
	when count
	  return count
	finally (return nil)))

(defclass and-rule (children-mixin)
  ()
  (:documentation "Matches if all children match consecutively, from left to right."))

(defmethod match (input (rule and-rule) start)
  (loop with total = 0
	for child in (children rule)
	for count = (match input child (+ start total))
	if count
	  do (incf total count)
	else
	  return nil
	finally (return total)))

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
    (if count
	count
	nil)))

(defclass repeat-rule (child-mixin)
  ()
  (:documentation "Matches the child multiple times."))

(defmethod match (input (rule repeat-rule) start)
  (loop with total = 0
	for count = (match input (child rule) (+ start total))
	if count
	  do (incf total count)
	else
	  ;; Returns zero when child repeats zero times
	  return total))

(defclass lazy-repeat-rule (repeat-rule)
  ((stop%
    :reader stop
    :initarg :stop))
  (:documentation "Matches the child rule the fewest number of times before the stop rule matches."))

(defmethod match (input (rule lazy-repeat-rule) start)
  (with-accessors ((child child) (stop stop)) rule
    (loop with total = 0
	  ;; Stop rule
	  for count = (match input stop (+ start total))
	  when count
	    return (+ total count)
	  ;; Child rule
	  do (setf count (match input child (+ start total)))
	     (when count
	       (incf total count)))))
