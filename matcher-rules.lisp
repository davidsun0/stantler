(in-package #:stantler)

(defgeneric match (input rule start)
  (:documentation "Matches `rule` on `input` at index `start`.
Returns the number of items matched or NIL if the rule fails to match.
Rules that sucessfully match no items return 0."))

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

(defclass string-literal-rule ()
  ((value%
    :reader value
    :initarg :value)))

(defmethod match ((input string) (rule string-literal-rule) start)
  (if (string= (value rule) input :start2 start)
      (length (value rule))
      nil))

(defclass char-range-rule ()
  ((low%
    :reader low
    :initarg :low)
   (high%
    :reader high
    :initarg :high))
  (:documentation "Matches a character with char-code between low and high, inclusive."))

(defmethod match (input (rule char-range-rule) start)
  (with-no-eof-match
    (if (char<= (low rule)
		(look-ahead input start)
		(high rule))
	1
	nil)))

;;; Special Rules =====================================================

(defclass null-rule ()
  ()
  (:documentation "Never matches. Useful as a placeholder."))

(defmethod match (input (rule null-rule) start)
  (declare (ignore input rule start))
  nil)

(defclass wildcard-rule ()
  ()
  (:documentation "Matches any one object."))

(defmethod match (input (rule wildcard-rule) start)
  (if (look-ahead input start)
      1
      nil))

;;; ANTLR has language actions, which allows for executing arbitrary code upon
;;; lexing a pattern. Since this is Lisp, we use the built-in reader to parse Lisp
;;; code.

(defclass eof-rule () ())

(defclass lisp-form-rule ()
  ((read-eval
    :reader read-eval
    :initarg :read-eval
    :initform nil))
  (:documentation "Matches one Lisp form."))

(defmethod match ((input string) (rule lisp-form-rule) (start integer))
  (let ((*read-eval* (read-eval rule))
	(*package* (find-package "CL-USER")))
    (nth-value 1 (read-from-string input nil nil :start start))))

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
	0)))

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

;; The one-or-more-rule is not strictly necessary as it can be composed from by
;; the pattern (a a*). However, it simplifies parse tree walkers as it removes
;; some list manipulation.

(defclass one-or-more-rule (child-mixin)
  ()
  (:documentation "Matches the child rule one or more times."))

(defmethod match (input (rule one-or-more-rule) start)
  (loop with total = 0
	for count = (match input (child rule) (+ start total))
	if count
	  do (incf total count)
	else
	  return (if (plusp total)
		     total
		     nil)))

(defclass lazy-rule ()
  ((stop%
    :reader stop
    :initarg :stop)))

(defclass lazy-maybe-rule (lazy-rule child-mixin) ())

(defclass lazy-repeat-rule (lazy-rule child-mixin)
  ()
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

(defclass lazy-one-or-more-rule (lazy-rule child-mixin) ())
