;;; Bootstrap Lexer
;;; Used for lexing ANTLR in ANTLR
;;; No
;;; - Unicode
;;; - Recursive Rules, Fragments
;;; - Lexer actions, Modes, Channels

(in-package #:stantler)

(defclass lexer ()
  ((rules%
    :accessor rules
    :initform (make-array 16 :adjustable t :fill-pointer 0))))

(defmethod match (input (lexer lexer) start)
  (loop for rule across (rules lexer)
	do (let ((m (match input rule start)))
	     (when m (return m)))
	finally (return nil)))

(defun lex (input lexer start)
  (flet ((lex-once (index)
	   (print index)
	   (loop for rule across (rules lexer)
		 do (let ((m (match input rule index)))
		      (when m (return m)))
		 finally (return nil))))
    (let ((count 0))
      (loop while (lex-once (+ start count))
	      do (incf count it)
    ))))

(let ((count 0))
      (loop with match = (lex-once (+ start count))
	    when match
	      ;; collect (cons (+ start count) match) into tokens
	    ;; and
	      do (incf count match)
		     (print (+ start count))
		     (print match)
	    else
	      return nil))

(defparameter *bootstrap-lexer*
  (make-instance 'lexer))

(let ((wildcard (make-instance 'wildcard-rule)))
  (flet ((literal (literal)
	   (cond
	     ((characterp literal)
	      (make-instance 'object-literal-rule :comparison 'char= :value literal))
	     ((stringp literal)
	      (make-instance 'array-literal-rule :comparison 'char= :value literal))))
	 (range (low high)
	   (make-instance 'char-range-rule :low low :high high))
	 (and-rule (&rest rules)
	   (make-instance 'and-rule :children rules))
	 (or-rule (&rest rules)
	   (make-instance 'or-rule :children rules))
	 (lazy-repeat (child stop)
	   (make-instance 'lazy-repeat-rule :child child :stop stop))
	 (repeat (child)
	   (make-instance 'repeat-rule :child child))
	 (lexer-rule (name rule)
	   (declare (ignore name))
	   (vector-push-extend rule (rules *bootstrap-lexer*)))
	 (not-rule (child)
	   (make-instance 'not-rule :child child)))

    (lexer-rule "DOC_COMMENT"
	(and-rule (literal "/**") (lazy-repeat wildcard (literal "*/"))))
    (lexer-rule "BLOCK_COMMENT"
	(and-rule (literal "/*") (lazy-repeat wildcard (literal "*/"))))
    (lexer-rule "LINE_COMMENT"
	(and-rule (literal "//")
		  (lazy-repeat wildcard
			       (or-rule (literal +return+)
					(literal +newline+)))))
    (lexer-rule "INT"
	(or-rule (literal #\0)
		 (and-rule (range #\1 #\9) (repeat (range #\0 #\9)))))
    (lexer-rule "STRING_LITERAL"
	(and-rule (literal #\')
		  (repeat (or-rule (and-rule (literal #\\)
					     (or
					      (literal #\b)
					      (literal #\t)
					      (literal #\n)
					      (literal #\f)
					      (literal #\r)
					      (literal #\")
					      (literal #\')
					      (literal #\\)))
				   (not-rule (or
					      (literal #\')
					      (literal +return+)
					      (literal +newline+)
					      (literal #\\)))))
		  (literal #\')))

    (let ((string-tokens
	    (list
	     "import"
	     "fragment"
	     "lexer"
	     "parser"
	     "grammar"
	     "protected"
	     "public"
	     "private"
	     "returns"
	     "locals"
	     "throws"
	     "catch"
	     "finally"
	     "mode")))
      (loop for st in string-tokens
	    do (lexer-rule (string-upcase st)
		   (literal (string-downcase st)))))

    (lexer-rule "COLONCOLON" (literal "::"))
    (lexer-rule "COLON" (literal #\:))
    (lexer-rule "COMMA" (literal #\,))
    (lexer-rule "SEMI" (literal #\;))
    (lexer-rule "LPAREN" (literal #\())
    (lexer-rule "RPAREN" (literal #\)))
    (lexer-rule "LBRACE" (literal #\{))
    (lexer-rule "RBRACE" (literal #\}))
    (lexer-rule "RARROW" (literal "->"))
    (lexer-rule "LT" (literal #\<))
    (lexer-rule "GT" (literal #\>))
    (lexer-rule "ASSIGN" (literal #\=))
    (lexer-rule "QUESTION" (literal #\?))
    (lexer-rule "STAR" (literal #\*))
    (lexer-rule "PLUS_ASSIGN" (literal "+="))
    (lexer-rule "PLUS" (literal #\+))
    (lexer-rule "OR" (literal #\|))
    (lexer-rule "DOLLAR" (literal #\$))
    (lexer-rule "RANGE" (literal ".."))
    (lexer-rule "DOT" (literal #\.))
    (lexer-rule "AT" (literal #\@))
    (lexer-rule "POUND" (literal #\#))
    (lexer-rule "NOT" (literal #\~))

    (lexer-rule "WS"
      (and-rule (or-rule
		 (literal #\Space)
		 (literal #\Tab)
		 (literal +return+)
		 (literal +newline+)
		 (literal #\Page))
		(repeat (or-rule
			 (literal #\Space)
			 (literal #\Tab)
			 (literal +return+)
			 (literal +newline+)
			 (literal #\Page)))))

    )) ; end let
