(in-package #:stantler)

(defgeneric match (rule input start)
  (:documentation "Matches `rule` on `input` at index `start`.
Returns the number of items matched or NIL if the rule fails to match.
Rules that sucessfully match no items return 0."))

(defmacro with-no-eof-match (&body body)
  "Evaluates body. If an `eof-error' is signaled, the error is captured and this form evaluates to NIL."
  `(handler-case (progn ,@body)
     (eof-error ()
       nil)))

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

(defgeneric parse-tree (rule input start)
  (:documentation "Matches parser rule `rule` on `input`, an array of tokens
starting at index `start`."))

(defmethod parse-tree ((rule object-literal-rule) input start)
  (when (match rule input start)
    (look-ahead input start)))

(defmethod parse-tree ((and-rule and-rule) input start)
  (loop with length = 0
	for child in (children and-rule)
	for count = (match child input (+ start length))
	if count
	  collect (parse-tree child input (+ start length)) into nodes
	  and do (incf length count)
	else
	  return nil
	finally (return nodes)))

(defmethod parse-tree ((or-rule or-rule) input start)
  (loop for child in (children or-rule)
	for count = (match child input start)
	when count
	  return (parse-tree child input start)
	finally (return nil)))

(defmethod parse-tree ((rule parser-subrule) input start)
  (let ((subrule (gethash (name rule) (rules *antlr-parser*))))
    (when (match subrule input start)
      (let ((children (parse-tree subrule input start)))
	(unless (listp children)
	  (setf children (list children)))
	(make-instance 'parse-node
		       :children children
		       :rule rule)))))

(defmethod parse-tree ((rule repeat-rule) input start)
  (loop with length = 0
	for count = (match (child rule) input (+ start length))
	if count
	  collect (parse-tree (child rule) input (+ start length))
	    into results
	  and do (incf length count)
	else
	  return results))

(defmethod parse-tree ((rule one-or-more-rule) input start)
  (loop with length = 0
	for count = (match (child rule) input (+ start length))
	if count
	  collect (parse-tree (child rule) input (+ start length))
	    into results
	    and do (incf length count)
	else
	  return results))

(defmethod parse-tree ((rule maybe-rule) input start)
  (if (plusp (match rule input start))
      (parse-tree (child rule) input start)
      '()))
