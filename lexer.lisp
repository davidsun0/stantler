(in-package #:stantler)

(defun slurp-file (path)
  (with-open-file (stream path
			  :direction :input
			  :element-type 'character)
    (let ((chars (make-array (file-length stream)
			      :element-type 'character)))
      (read-sequence chars stream)
      chars)))

(defun look-ahead (input start &optional (count 0))
  (let ((index (+ start count)))
    (if (>= index (array-total-size input))
	(error 'eof-error :index index)
	(aref input index))))

(defun char-rule (char &optional (comparison 'char=))
  (make-instance 'object-literal-rule :value char :comparison comparison))

(defun string-rule (string &optional (comparison 'char=))
  (make-instance 'array-literal-rule :value string :comparison comparison))

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

(defclass lisp-form-rule ()
  ((read-eval
    :reader read-eval
    :initarg :read-eval
    :initform t))
  (:documentation "Matches one Lisp form."))

(defmethod match ((input string) (rule lisp-form-rule) (start integer))
  (let ((*read-eval* (read-eval rule))
	(*package* (find-package "CL-USER")))
    (nth-value 1 (read-from-string input nil nil :start start))))

(defclass lexer ()
  ((name%
    :reader name
    :initarg :name)
   (mode%
    :accessor mode
    :initform :default)
   (modes%
    :accessor modes
    :initform nil)))

(defclass lexer-mode ()
  ((name%
    :reader name
    :initarg :name)
   (rules%
    :reader rules
    :initform (make-array 16 :adjustable t :fill-pointer 0))))

(defmethod mode-rules ((lexer lexer) name)
  (let ((mode (find name (modes lexer) :key 'name :test 'equal)))
    (if mode
	(rules mode)
	(let ((new-mode (make-instance 'lexer-mode :name name)))
	  (push new-mode (modes lexer))
	  (rules new-mode)))))

(defclass token ()
  ((content%
    :reader content
    :initarg :content)
   (rule%
    :reader rule
    :initarg :rule)
   (channel%
    :reader channel
    :initarg :channel
    :initform :default)))

(defclass lexer-rule (child-mixin)
  ((name%
    :reader name
    :initarg :name)
   (skip-p%
    :reader skip-p
    :initarg :skip-p
    :initform nil)
   (more-p%
    :reader more-p
    :initarg :more-p
    :initform nil)
   (token-type%
    :reader token-type
    :initarg :token-type
    :initform nil)
   (channel%
    :reader channel
    :initarg :channel
    :initform :default)
   (lexer-mode%
    :reader lexer-mode
    :initarg :lexer-mode
    :initform nil))
  (:documentation "Top level lexer rule"))

(defmethod match (input (rule lexer-rule) start)
  (match input (child rule) start))

(defun lex% (input lexer token-start)
  (let ((length 0)
	(rule-start token-start)
	rule
	match
	type
	channel)
    (tagbody
     loop
       (loop for rule* across (mode-rules lexer (mode lexer)) 
	     for match* = (match input rule* rule-start)
	     when match*
	       return (setf rule rule* match match*)
	     finally (return-from lex% nil))
       ;; Can't be bothered to write a tail call macro
       (when (skip-p rule)
	 (incf rule-start match)
	 (setf token-start rule-start)
	 (setf length 0)
	 (go loop))
       (when (more-p rule)
	 (incf rule-start match)
	 (setf length match)
	 (go loop))
       (setf type (or (token-type rule) rule))
       (setf channel (or (channel rule) :default))
       (setf (mode lexer) (or (lexer-mode rule) :default)))
    (make-instance 'token
		   :rule type
		   :channel channel
		   :content (cons token-start (+ match length)))))

(defun lex (input lexer start)
  (loop for token = (lex% input lexer start)
	do (print token)
	when token
	  collect token into tokens
	  and do (incf start (cdr (content token)))
	else
	  return (values tokens start)))
