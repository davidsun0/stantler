(in-package #:stantler)

;;;; Text-specific matching rules ==============================================

(defun char-rule (char &optional (comparison 'char=))
  "Matches one character."
  (make-instance 'object-literal-rule :value char :comparison comparison))

(defun string-rule (string &optional (comparison 'char=))
  "Matches a string literal."
  (make-instance 'array-literal-rule :value string :comparison comparison))

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

;;; ANTLR has language actions, which allows for executing arbitrary code upon
;;; lexing a pattern. Since this is Lisp, we use the built-in reader to parse Lisp
;;; code.

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

;;;; Lexer =====================================================================

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

;;; Each lexer is composed of one or more lexer modes.
;;; Each mode contains a set of matcher rules; rules can switch the lexer's
;;; current mode.

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

(defclass lexer-rule (child-mixin)
  ((name%
    :reader name
    :initarg :name)
   (skip-p%
    :reader skip-p
    :initarg :skip-p
    :initform nil)
   ;; True when a rule continues to consume input
   (more-p%
    :reader more-p
    :initarg :more-p
    :initform nil)
   ;; Is token-type necessary? It references the rule that created the token (i.e. this object)
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
  (:documentation "Top level lexer rule."))

(defmethod match (input (rule lexer-rule) start)
  (match input (child rule) start))

;;; When a rule successfully matches, the text it matches produces a token.

(defclass token ()
  ((content%
    :accessor content
    :initarg :content)
   (offset%
    :accessor offset
    :initarg :offset)
   ;; Rule may be overwritten by lexer (why, ANTLR!?)
   (rule%
    :accessor rule
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

(defun construct-token (rule channel input offset length)
  "Utility function for generating lexer tokens.
Token content is a offset array into the input, which should be a string."
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

;; These functions are weird. Should they be methods?

;; rule-start is incremented in next-token
(defun match-rule (mode-rules input rule-start)
  (loop for rule across mode-rules
	for match = (match input rule rule-start)
	when match
	  collect (list rule match) into matches
	  and maximize match into max-match
	finally
	   (return
	     (values-list
	      (cond
		;; Zero matches
		((null matches)
		 nil)
		;; One match
		((= (length matches) 1)
		 (first matches))
		;; From The Definitive ANTLR4 Reference 15.6 (p. 285):
		;; When multiple rules match, return the longest match
		;; If there is a tie, the rule defined first wins.
		(t
		 (find-if (lambda (x) (= x max-match))
			  matches
			  :key 'second
			  :from-end nil)))))))

(defun next-token (input lexer token-start)
  "Lexes the next token from the input at the offset `token-start'."
  (let ((length 0)
	(rule-start token-start)
	rule
	match)
    (tagbody
     loop
       ;; Search for an applicable rule
       (setf (values rule match)
	     (match-rule
	      (mode-rules lexer (mode lexer))
	      input
	      rule-start))
       (unless rule
	 (return-from next-token))
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
