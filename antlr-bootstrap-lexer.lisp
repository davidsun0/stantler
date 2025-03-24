(in-package #:stantler)

(defvar *antlr-lexer*
  (make-instance 'lexer))

(let ((wildcard (make-instance 'wildcard-rule)))
  (setf *antlr-lexer* (make-instance 'lexer))
  (lexer-rule "DOC_COMMENT"
	      (and-rule (literal "/**") (lazy-repeat wildcard (literal "*/")))
	      :channel "COMMENT")
  (lexer-rule "BLOCK_COMMENT"
	      (and-rule (literal "/*") (lazy-repeat wildcard (literal "*/")))
	      :channel "COMMENT")
  (lexer-rule "LINE_COMMENT"
	      (and-rule (literal "//")
			(lazy-repeat wildcard
				     (or-rule (literal +return+)
					      (literal +newline+))))
	      :channel "COMMENT")

  (lexer-rule "INT"
	      (or-rule (literal #\0)
		       (and-rule (range #\1 #\9) (repeat (range #\0 #\9)))))

  (lexer-rule "STRING_LITERAL"
	      (and-rule (literal #\')
			(repeat (or-rule (and-rule (literal #\\)
						   wildcard)
					 (not-rule (or-rule
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
			(repeat (or-rule (range #\A #\Z)
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
		(and-rule white-space (repeat white-space))
		:channel "OFF_CHANNEL"))

;;; ANTLR Lexer converts ID tokens to either TOKEN_REF or RULE_REF based on text case
;;; Dummy rules are used because token-rule compares rules by eq
  (lexer-rule "TOKEN_REF" (make-instance 'null-rule))
  (lexer-rule "RULE_REF" (make-instance 'null-rule))

  ) ; end let

