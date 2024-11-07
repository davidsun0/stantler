(in-package #:stantler)

(defclass rule () ())

;;; Literal Rules =====================================================

;; TODO: split into char-rule and token-rule
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

(defclass object-literal-rule (literal-rule) ()
  (:documentation "Matches a single object literal."))

(defclass string-rule ()
  ((value%
    :reader value
    :initarg :value)))

(defclass character-rule ()
  ((value%
    :reader value
    :initarg :value)))

(defclass char-range-rule ()
  ((low%
    :reader low
    :initarg :low)
   (high%
    :reader high
    :initarg :high))
  (:documentation "Matches a character with char-code between low and high, inclusive."))

;;; Special Rules =====================================================

(defclass null-rule () ()
  (:documentation "Never matches. Useful as a placeholder."))

(defclass wildcard-rule () ()
  (:documentation "Matches any one object."))

;;; ANTLR has language actions, which allows for executing arbitrary code upon
;;; lexing a pattern. Since this is Lisp, we use the built-in reader to parse Lisp
;;; code.

(defclass eof-rule () ())

;; Does this need a rule? Are language actions actually lexed?

(defclass lisp-form-rule ()
  ((read-eval
    :reader read-eval
    :initarg :read-eval
    :initform nil))
  (:documentation "Matches one Lisp form."))

;;; Compound Rules ====================================================

(defclass or-rule (children-mixin) ()
  (:documentation "Matches the first applicable child rule."))

(defclass and-rule (children-mixin) ()
  (:documentation "Matches if all children match consecutively, from left to right."))

(defclass not-rule (child-mixin) ()
  (:documentation "Matches if the child rule doesn't match."))

(defclass maybe-rule (child-mixin) ()
  (:documentation "Matches the child rule or nothing."))

(defclass repeat-rule (child-mixin) ()
  (:documentation "Matches the child multiple times."))

;; The one-or-more-rule is not strictly necessary as it can be composed from by
;; the pattern (a a*). However, it simplifies parse tree walkers as it removes
;; some list manipulation.

(defclass one-or-more-rule (child-mixin) ()
  (:documentation "Matches the child rule one or more times."))

(defclass lazy-rule ()
  ((stop%
    :accessor stop
    :initarg :stop)))

(defclass lazy-maybe-rule (lazy-rule child-mixin) ())

(defclass lazy-repeat-rule (lazy-rule child-mixin) ()
  (:documentation "Matches the child rule the fewest number of times before the stop rule matches."))

(defclass lazy-one-or-more-rule (lazy-rule child-mixin) ())

(defgeneric match (rule input start)
  (:documentation "Matches `rule` on `input` at index `start`.
Returns the number of items matched or NIL if the rule fails to match.
Rules that sucessfully match no items return 0."))

(defmethod match ((rule object-literal-rule) input start)
  (with-accessors ((comparison comparison) (value value) (key key)) rule
    (with-no-eof-match
      (if (funcall comparison value (funcall key (look-ahead input start)))
	  1
	  nil))))

(defmethod match ((rule character-rule) (input string) start)
  (with-no-eof-match
    (if (char= (value rule) (look-ahead input start))
	1
	nil)))

(defmethod match ((rule string-rule) (input string) start)
  (let ((end (+ start (length (value rule)))))
    (with-no-eof-match
      (if (and (< end (length input))
	       (string= (value rule) input :start2 start :end2 end))
	  (length (value rule))
	  nil))))

(defmethod match ((rule char-range-rule) input start)
  (with-no-eof-match
    (if (char<= (low rule)
		(look-ahead input start)
		(high rule))
	1
	nil)))

(defmethod match ((rule null-rule) input start)
  (declare (ignore rule input start))
  nil)

(defmethod match ((rule wildcard-rule) input start)
  (if (look-ahead input start)
      1
      nil))

(defmethod match ((rule lisp-form-rule) (input string) (start integer))
  (let ((*read-eval* (read-eval rule))
	(*package* (find-package "CL-USER")))
    (nth-value 1 (read-from-string input nil nil :start start))))

(defmethod match ((rule or-rule) input start)
  (loop for child in (children rule)
	for count = (match child input start)
	when count
	  return count
	finally (return nil)))

(defmethod match ((rule and-rule) input start)
  (loop with total = 0
	for child in (children rule)
	for count = (match child input (+ start total))
	if count
	  do (incf total count)
	else
	  return nil
	finally (return total)))

(defmethod match ((rule not-rule) input start)
  (if (match (child rule) input start)
      nil
      1))

(defmethod match ((rule maybe-rule) input start)
  (let ((count (match (child rule) input start)))
    (if count
	count
	0)))

(defmethod match ((rule repeat-rule) input start)
  (loop with total = 0
	for count = (match (child rule) input (+ start total))
	if count
	  do (incf total count)
	else
	  ;; Returns zero when child repeats zero times
	  return total))

(defmethod match ((rule one-or-more-rule) input start)
  (loop with total = 0
	for count = (match (child rule) input (+ start total))
	if count
	  do (incf total count)
	else
	  return (if (plusp total)
		     total
		     nil)))

(defmethod match ((rule lazy-repeat-rule) input start)
  (with-accessors ((child child) (stop stop)) rule
    (loop with total = 0
	  ;; Stop rule
	  for count = (match stop input (+ start total))
	  when count
	    return (+ total count)
	  ;; Child rule
	  do (setf count (match child input (+ start total)))
	     (when count
	       (incf total count)))))

