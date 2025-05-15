(in-package #:stantler)

(defclass rule ()
  ((name%
    :accessor name
    :initarg :name
    :initform nil)
   (channel%
    :accessor channel
    :initarg :channel
    :initform :default)
   (skip-p%
    :accessor skip-p
    :initarg :skip-p
    :initform nil)
   (lexer-mode%
    :reader lexer-mode
    :initarg :lexer-mode
    :initform nil)
   ))

;;; Literal Rules =====================================================

(defclass object-literal-rule (rule)
  ((value%
    :reader value
    :initarg :value)
   (comparison%
    :reader comparison
    :initform 'eq
    :initarg :comparison)
   (key%
    :reader key
    :initform 'identity
    :initarg :key))
  (:documentation "Matches a single object literal."))

(defclass string-rule (rule)
  ((value%
    :reader value
    :initarg :value)))

(defclass character-rule (rule)
  ((value%
    :reader value
    :initarg :value)))

(defclass char-range-rule (rule)
  ((low%
    :reader low
    :initarg :low)
   (high%
    :reader high
    :initarg :high))
  (:documentation "Matches a character with char-code between low and high, inclusive."))

;;; Special Rules =====================================================

(defclass null-rule (rule) ()
  (:documentation "Never matches. Useful as a placeholder."))

(defclass fragment-reference-rule (rule child-mixin) ()
  (:documentation "Placeholder for rule compilation."))

(defclass wildcard-rule (rule) ()
  (:documentation "Matches any one object."))

;;; ANTLR has language actions, which allows for executing arbitrary code upon
;;; lexing a pattern. Since this is Lisp, we use the built-in reader to parse Lisp
;;; code.

(defclass eof-rule (rule) ())

;; Does this need a rule? Are language actions actually lexed?

(defclass lisp-form-rule (rule)
  ((read-eval
    :reader read-eval
    :initarg :read-eval
    :initform nil))
  (:documentation "Matches one Lisp form."))

;;; Compound Rules ====================================================

(defclass or-rule (rule children-mixin) ()
  (:documentation "Matches the first applicable child rule."))

(defclass and-rule (rule children-mixin) ()
  (:documentation "Matches if all children match consecutively, from left to right."))

(defclass not-rule (rule child-mixin) ()
  (:documentation "Matches if the child rule doesn't match."))

(defclass maybe-rule (rule child-mixin) ()
  (:documentation "Matches the child rule or nothing."))

(defclass repeat-rule (rule child-mixin) ()
  (:documentation "Matches the child multiple times."))

;; The one-or-more-rule is not strictly necessary as it can be composed from by
;; the pattern (a a*). However, it simplifies parse tree walkers as it removes
;; some list manipulation.

(defclass one-or-more-rule (rule child-mixin) ()
  (:documentation "Matches the child rule one or more times."))

(defclass lazy-rule (rule)
  ((stop%
    :accessor stop
    :initarg :stop)))

(defclass lazy-maybe-rule (lazy-rule child-mixin) ())

(defclass lazy-repeat-rule (lazy-rule child-mixin) ()
  (:documentation "Matches the child rule the fewest number of times before the stop rule matches."))

(defclass lazy-one-or-more-rule (lazy-rule child-mixin) ())

;;;; Lexer =====================================================================

(defclass lexer ()
  ((name%
    :reader name
    :initarg :name)
   (mode%
    :accessor mode
    :initform (list :default))
   (modes%
    :accessor modes
    :initarg :modes
    :initform nil)
   (fragments%
    :reader fragments
    :initarg :fragments
    :initform '())))

;;; Each lexer is composed of one or more lexer modes.
;;; Each mode contains a set of matcher rules; rules can switch the lexer's
;;; current mode.

(defclass lexer-mode (children-mixin)
  ((name%
    :reader name
    :initarg :name)))

(defmethod print-object ((mode lexer-mode) stream)
  (print-unreadable-object (mode stream :type t :identity t)
    (format stream "~A" (name mode))))

(defclass fragment (child-mixin)
  ((name%
    :reader name
    :initarg :name)))

(defmethod print-object ((fragment fragment) stream)
  (print-unreadable-object (fragment stream :type t)
    (format stream "~A" (name fragment))))

#|
;; TODO: remove and merge into rule
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
    :initform nil)
   (action%
    :reader action
    :initarg :action
    :initform nil))
  (:documentation "Top level lexer rule."))

(defmethod print-object ((rule lexer-rule) stream)
  (print-unreadable-object (rule stream :type t :identity t)
    (format stream "~A" (name rule))))

(defmethod match ((rule lexer-rule) input start)
  (match (child rule) input start))
|#

;;; When a rule successfully matches, the text it matches produces a token.

(defclass token ()
  ((content%
    :accessor content
    :initarg :content)
   (offset%
    :accessor offset
    :initarg :offset)
   ;; Rule may be overwritten by lexer
   ;; Tokens' rule objects are EQ if they are equal
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
    (if (and (typep (rule token) 'child-mixin)
	     (typep (child (rule token)) 'object-literal-rule))
	(format stream "~A" (name (rule token)))
	(format stream "~A ~S" (name (rule token)) (content token)))))

(defun next-token (lexer input start)
  (loop with length = 0
	with match = nil
	with rule-start = start
	with token-start = start
	for rule = nil
	do (loop with max-match = -1
		 with best-rule = nil
		 ;; Search for an applicable rule.
		 ;; (mode lexer) is a stack of mode names - the car is the current mode.
		 for r across (children (find (car (mode lexer)) (modes lexer) :key 'name))
		 for m = (match r input rule-start)
		 ;; When multiple rules match, return the longest match.
		 ;; If there is a tie, the rule defined first wins.
		 when (and m (> m max-match))
		   do (setf max-match m
			    best-rule r)
		 finally (when best-rule
			   (setf match max-match rule best-rule)))
	   ;; Lexer actions happen here?
	   ;; (when (and rule (action rule))
	   ;;  (funcall (action rule) lexer))
	   (cond
	     ;; No match
	     ((null match) (return nil))
	     ;; When there is a match, additional rules may be triggered.
	     ;; Tail recurse to continue building the token.
	     ((skip-p rule)
	      (break)
	      ;; Skip: The rule matches, but does not produce a token.
	      ;; Continue matching at the new location with the next rule.
	      (setf rule-start (+ rule-start match)
		    token-start rule-start
		    length 0))
	     #|
	     ((more-p rule)
	      (break)
	      ;; More: continue matching.
	      ;; TODO: verify `more` semantics
	      (setf rule-start (+ rule-start match)
	     length match))
	     |#
	     ;; Successful match: exit the tagbody and produce a token.
	     (t
	      (setf (car (mode lexer)) (or (lexer-mode rule) :default))
	      (incf length match)
	      (let ((contents (make-array length
					  :element-type (array-element-type input)
					  :displaced-index-offset token-start
					  :displaced-to input)))
		(return (make-instance 'token
				       :rule    rule
				       :channel (or (channel rule) :default)
				       :offset  (cons token-start length)
				       :content contents)))))))

(defun lex (lexer input start)
  (setf (mode lexer) (list :default))
  (loop for token = (next-token lexer input start)
	when token
	  collect token into tokens
	  and do (incf start (cdr (offset token)))
	else
	  return (values tokens start)))

;;;; Parser ====================================================================

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
