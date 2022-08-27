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
    :accessor content
    :initarg :content)
   (offset%
    :accessor offset
    :initarg :offset)
   (rule%
    :reader rule
    :initarg :rule)
   (channel%
    :reader channel
    :initarg :channel
    :initform :default)))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t :identity nil)
    ;; If the top-level rule's child is a literal, just print the name.
    ;; Otherwise, print the rule name and the matched content.
    (if (typep (child (rule token)) 'literal-rule)
	(format stream "~A" (name (rule token)))
	(format stream "~A ~S" (name (rule token)) (content token)))))

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

(defun construct-token (rule channel input offset length)
  (make-instance
   'token
   :rule rule
   :channel channel
   :offset (cons offset length)
   :content (make-array
	     length
	     :element-type (array-element-type input)
	     :displaced-index-offset offset
	     :displaced-to input)))

(defun next-token (input lexer token-start)
  "Lexes the next token from the input at the offset `token-start`."
  (let ((length 0)
	(rule-start token-start)
	rule
	match)
    (tagbody
     loop
       ;; Search for an applicable rule
       (loop for rule* across (mode-rules lexer (mode lexer)) 
	     for match* = (match input rule* rule-start)
	     when match*
	       return (setf rule rule* match match*)
	     ;; Return if no rules match
	     finally (return-from next-token nil))
       ;; Match may trigger additional rules
       ;; Tail recurse to the next rule for `skip' and `more'
       (when (skip-p rule)
	 (incf rule-start match)
	 (setf token-start rule-start)
	 (setf length 0)
	 (go loop))
       (when (more-p rule)
	 (incf rule-start match)
	 (setf length match)
	 (go loop))
       (setf (mode lexer) (or (lexer-mode rule) :default)))
    (construct-token
     (or (token-type rule) rule)
     (or (channel rule) :default)
     input
     token-start
     (+ length match))))

(defun lex (input lexer start)
  (loop for token = (next-token input lexer start)
	when token
	  collect token into tokens
	  and do (incf start (cdr (offset token)))
	else
	  return (values tokens start)))
