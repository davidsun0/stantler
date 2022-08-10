(in-package #:stantler)

(defun slurp-file (path)
  (with-open-file (stream path
			  :direction :input
			  :element-type 'character)
    (let ((chars (make-array (file-length stream)
			      :element-type 'character)))
      (read-sequence chars stream)
      chars)))

(defgeneric match (input rule start)
  (:documentation "Matches `rule` on `input` at index `start`.
Returns the number of items matched or NIL if the rule fails to match.
Rules that sucessfully match no items return 0."))

(defun look-ahead (input start &optional (count 0))
  (let ((index (+ start count)))
    (if (>= index (array-total-size input))
	(error 'eof-error :index index)
	(aref input index))))

(defun char-rule (char &optional (comparison 'char=))
  (make-instance 'literal-rule :value char :comparison comparison))

(defun string-rule (string &optional (comparison 'char=))
  (make-instance 'literal-rule :value string :comparison comparison))

(defclass char-range-rule ()
  ((low%
    :reader low
    :initarg :low)
   (high%
    :reader high
    :initarg :high)))

(defmethod match (input (rule char-range-rule) start)
  (with-no-eof-match
    (if (char<= (low rule)
		(look-ahead input start)
		(high rule))
	1
	nil)))

(defclass lexer ()
  ((rules%
    :accessor rules
    :initform (make-array 16 :adjustable t :fill-pointer 0))))

(defmethod match (input (lexer lexer) start)
  "Matches first applicable rule in `lexer`."
  (loop for rule across (rules lexer)
	with match = (match input rule start)
	when match
	  return match
	finally (return nil)))

(defclass token ()
  ((content%
    :reader content
    :initarg :content)
   (name%
    :reader name
    :initarg :name)))

(defun tokenize (input lexer start)
  (loop for rule across (rules lexer)
	for match = (match input rule start)
	when match
	  return (make-instance 'token
				:content (cons start match)
				:name rule)
	finally (return nil)))

(defun lex (input lexer start)
  (loop for token = (tokenize input lexer start)
	if token
	  collect token into tokens
	  and do (incf start (cdr (content token)))
	else
	  return (values tokens start)))
