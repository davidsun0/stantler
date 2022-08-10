;;; Bootstrap Lexer
;;; Used for lexing ANTLR in ANTLR
;;; No
;;; - Unicode
;;; - Recursive Rules, Fragments
;;; - Lexer actions, Modes, Channels

(in-package #:stantler)

(defparameter *bootstrap-lexer*
  (make-instance 'lexer))

(let ((wildcard (make-instance 'wildcard-rule)))
  (flet ((literal (literal)
	   (cond
	     ((characterp literal)
	      (char-rule literal))
	     ((stringp literal)
	      (string-rule literal))))
	 (range (low high)
	   (make-instance 'char-range-rule :low low :high high))
	 (and-rule (&rest rules)
	   (make-instance 'and-rule :children rules))
	 (or-rule (&rest rules)
	   (make-instance 'or-rule :children rules))
	 (lazy-repeat (child stop)
	   (make-instance 'lazy-repeat-rule :child child :stop stop))
	 (eager-repeat (child)
	   (make-instance 'repeat-rule :child child))
	 (repeat (child)
	   (make-instance 'repeat-rule :child child))
	 (lexer-rule (name rule)
	   (let ((named-rule (make-instance 'named-rule
					    :child rule
					    :name name)))
	     (vector-push-extend named-rule (rules *bootstrap-lexer*))))
	 (not-rule (child)
	   (make-instance 'not-rule :child child)))

    (setf *bootstrap-lexer* (make-instance 'lexer))
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

    ;; Taking shortcuts to avoid language actions
    (lexer-rule "LEXER_CHAR_SET"
		(and-rule (literal #\[)
			  (repeat
			   (or-rule (and-rule (literal #\\) wildcard)
				    (not-rule (or-rule
					       (literal #\])
					       (literal #\\)))))
			  (literal #\])))

    (let ((wsnl-chars (repeat (or-rule
			       (literal #\Space)
			       (literal #\Tab)
			       (literal #\Page)
			       (literal +return+)
			       (literal +newline+)))))
      (lexer-rule "OPTIONS"
		  (and-rule (literal "options") wsnl-chars (literal #\{)))
      (lexer-rule "TOKENS"
		  (and-rule (literal "tokens") wsnl-chars (literal #\{)))
      (lexer-rule "CHANNELS"
		  (and-rule (literal "channels") wsnl-chars (literal #\{))))

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

    (lexer-rule "ID"
		(and-rule (or-rule (range #\A #\Z) (range #\a #\z))
			  (eager-repeat (or-rule (range #\A #\Z)
						 (range #\a #\z)
						 (range #\0 #\9)
						 (literal #\_)))))

    (let ((white-space (or-rule
			(literal #\Space)
			(literal #\Tab)
			(literal +return+)
			(literal +newline+)
			(literal #\Page))))
      (lexer-rule "WS"
		  (and-rule white-space (repeat white-space))))

    )) ; end let
