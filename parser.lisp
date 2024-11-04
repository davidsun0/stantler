(in-package #:stantler)

(defclass parser ()
  ((rules%
    :reader rules
    :initarg :rules
    ;; TODO: remove after bootstrapping
    :initform (make-hash-table :size 16 :test 'equal))))

;; TODO: rename to parser-node?
(defclass parser-subrule ()
  ((name%
    :initarg :name
    :reader name)))

(defmethod print-object ((parser-subrule parser-subrule) stream)
  (print-unreadable-object (parser-subrule stream :type t :identity nil)
    (princ (name parser-subrule) stream)))

(defclass parse-node (children-mixin)
  ((rule%
    :accessor rule
    :initarg :rule)))

(defmethod print-object ((parse-node parse-node) stream)
  (print-unreadable-object (parse-node stream :type t :identity t)
    (princ (rule parse-node) stream)))

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

;; TODO: compile parse-tree
