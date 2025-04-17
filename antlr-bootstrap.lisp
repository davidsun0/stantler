(in-package #:stantler)

(defvar *antlr-lexer*
  (make-instance 'lexer))

(defun literal (literal)
  (ctypecase literal
    (character
     (make-instance 'character-rule :value literal))
    (string
     (make-instance 'string-rule :value literal))))

(defun range (low high)
  (make-instance 'char-range-rule :low low :high high))

(defun and-rule (&rest rules)
  (make-instance 'and-rule :children rules))

(defun or-rule (&rest rules)
  (make-instance 'or-rule :children rules))

(defun lazy-repeat (child stop)
  (make-instance 'lazy-repeat-rule :child child :stop stop))

(defun repeat (child)
  (make-instance 'repeat-rule :child child))

(defun one-or-more (child)
  (make-instance 'one-or-more-rule :child child))

(defun not-rule (child)
  (make-instance 'not-rule :child child))

(defun convert-id (token)
  (when (string= "ID" (name (rule token)))
    (setf (rule token)
	  (find-rule
	   (if (upper-case-p (char (content token) 0))
	       "TOKEN_REF"
	       "RULE_REF")
	   *antlr-lexer*)))
  token)

(defun find-rule (name lexer)
  (loop for mode in (modes lexer)
	do (loop for rule across (children mode)
		 when (string= name (name rule))
		   do (return-from find-rule rule))))

(defun token-rule (rule-name)
  (make-instance 'object-literal-rule
		 :value (find-rule rule-name *antlr-lexer*)
		 :comparison 'eq
		 :key 'rule))

(defun maybe (child)
  (make-instance 'maybe-rule :child child))

(defun lexer-rule (name rule &rest args &key (mode :default) &allow-other-keys)
  (vector-push-extend
   (apply 'make-instance 'lexer-rule :child rule :name name args)
   (mode-rules *antlr-lexer* mode)))

(defun mode-rules (lexer name)
  (let ((mode (find name (modes lexer) :key 'name :test 'equal)))
    (cond
      (mode
       (children mode))
      (t
       (let ((new-mode (make-instance 'lexer-mode :name name)))
	 (push new-mode (modes lexer))
	 (setf (children new-mode) (make-array 8  :fill-pointer 0 :adjustable t))
	 (children new-mode))))))

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


(defvar *antlr-parser*
  (make-instance 'parser))

(defun parser-rule (name rule)
  (setf (gethash name (rules *antlr-parser*)) rule))

(defmethod match ((rule parser-subrule) input start)
  (let ((subrule (gethash (name rule) (rules *antlr-parser*))))
    (match subrule input start)))

(defun parser-subrule (name)
  (make-instance 'parser-subrule :name name))

(eval-when (:load-toplevel :execute)
  (parser-rule
   "grammarSpec"
   (and-rule (parser-subrule "grammarDecl")
	     (repeat (parser-subrule "prequelConstruct"))
	     (parser-subrule "rules")
	     (repeat (parser-subrule "modeSpec"))))

  (parser-rule
   "grammarDecl"
   (and-rule (parser-subrule "grammarType")
	     (parser-subrule "identifier")
	     (token-rule "SEMI")))

  (parser-rule
   "grammarType"
   (or-rule (and-rule (token-rule "LEXER") (token-rule "GRAMMAR"))
	    (and-rule (token-rule "PARSER") (token-rule "GRAMMAR"))
	    (token-rule "GRAMMAR")))

  (parser-rule
   "prequelConstruct"
   (or-rule (parser-subrule "optionsSpec")
	    (parser-subrule "delegateGrammars")
	    (parser-subrule "tokensSpec")
	    (parser-subrule "channelsSpec")
	    (parser-subrule "action_")))

  (parser-rule
   "optionsSpec"
   (and-rule (token-rule "OPTIONS")
	     (repeat (and-rule (parser-subrule "option")
			       (token-rule "SEMI")))
	     (token-rule "RBRACE")))

  (parser-rule
   "option"
   (and-rule (parser-subrule "identifier")
	     (token-rule "ASSIGN")
	     (parser-subrule "optionValue")))

  (parser-rule
   "optionValue"
   (or-rule
    (and-rule (parser-subrule "identifier")
	      (repeat (and-rule (token-rule "DOT")
				(parser-subrule "identifier"))))
    (token-rule "STRING_LITERAL")
    (parser-subrule "actionBlock")
    (token-rule "INT")))

  (parser-rule
   "delegateGrammars"
   (and-rule (token-rule "IMPORT")
	     (parser-subrule "delegateGrammar")
	     (repeat (and-rule (token-rule "COMMA") (parser-subrule "delegateGrammar")))
	     (token-rule "SEMI")))

  (parser-rule
   "delegateGrammar"
   (and-rule (parser-subrule "identifier")
	     (maybe (and-rule (token-rule "ASSIGN")
			      (parser-subrule "identifier")))))

  (parser-rule
   "tokensSpec"
   (and-rule (token-rule "TOKENS")
	     (maybe (parser-subrule "idList"))
	     (token-rule "RBRACE")))

  (parser-rule
   "channelsSpec"
   (and-rule (token-rule "CHANNELS")
	     (maybe (parser-subrule "idList"))
	     (token-rule "RBRACE")))

  (parser-rule
   "idList"
   (and-rule (parser-subrule "identifier")
	     (repeat (and-rule (token-rule "COMMA")
			       (parser-subrule "identifier")))
	     (maybe (token-rule "COMMA"))))

  (parser-rule
   "action_"
   (and-rule (token-rule "AT")
	     (maybe (and-rule (parser-subrule "actionScopeName")
			      (token-rule "COLONCOLON")))
	     (parser-subrule "identifier")
	     (parser-subrule "actionBlock")))

  (parser-rule
   "actionScopeName"
   (or-rule (parser-subrule "identifier")
	    (token-rule "LEXER")
	    (token-rule "PARSER")))

  (parser-rule
   "actionBlock"
   (and-rule (token-rule "BEGIN_ACTION")
	     (repeat (token-rule "ACTION_CONTENT"))
	     (token-rule "END_ACTION")))

  (parser-rule
   "argActionBlock"
   (and-rule (token-rule "BEGIN_ARGUMENT")
	     (repeat (token-rule "ACTION_CONTENT"))
	     (token-rule "END_ARGUMENT")))

  (parser-rule
   "modeSpec"
   (and-rule (token-rule "MODE")
	     (parser-subrule "identifier")
	     (token-rule "SEMI")
	     (repeat (parser-subrule "lexerRuleSpec"))))

  (parser-rule
   "rules"
   (repeat (parser-subrule "ruleSpec")))

  (parser-rule
   "ruleSpec"
   (or-rule (parser-subrule "parserRuleSpec")
	    (parser-subrule "lexerRuleSpec")))

  (parser-rule
   "parserRuleSpec"
   (and-rule
    (maybe (parser-subrule "ruleModifiers"))
    (token-rule "RULE_REF")
    (maybe (parser-subrule "argActionBlock"))
    (maybe (parser-subrule "ruleReturns"))
    (maybe (parser-subrule "throwsSpec"))
    (maybe (parser-subrule "localsSpec"))
    (repeat (parser-subrule "rulePrequel"))
    (token-rule "COLON")
    (parser-subrule "ruleBlock")
    (token-rule "SEMI")
    (parser-subrule "exceptionGroup")))

  (parser-rule
   "exceptionGroup"
   (and-rule
    (repeat (parser-subrule "exceptionHandler"))
    (maybe (parser-subrule "finallyClause"))))

  (parser-rule
   "exceptionHandler"
   (and-rule
    (token-rule "CATCH")
    (parser-subrule "argActionBlock")
    (parser-subrule "actionBlock")))

  (parser-rule
   "finallyClause"
   (and-rule
    (token-rule "FINALLY")
    (parser-subrule "actionBlock")))

  (parser-rule
   "rulePrequel"
   (or-rule (parser-subrule "optionsSpec") (parser-subrule "ruleAction")))

  (parser-rule
   "ruleReturns"
   (and-rule (token-rule "RETURNS") (parser-subrule "argActionBlock")))

  (parser-rule
   "throwsSpec"
   (and-rule
    (token-rule "THROWS")
    (parser-subrule "identifier")
    (repeat (and-rule (token-rule "COMMA") (parser-subrule "identifier")))))

  (parser-rule
   "localsSpec"
   (and-rule (token-rule "LOCALS") (parser-subrule "argActionBlock")))

  (parser-rule
   "ruleAction"
   (and-rule
    (token-rule "AT")
    (parser-subrule "identifier")
    (parser-subrule "actionBlock")))

  (parser-rule
   "ruleModifiers"
   (and-rule
    (parser-subrule "ruleModifier")
    (repeat (parser-subrule "ruleModifier"))))

  (parser-rule
   "ruleModifier"
   (or-rule
    (token-rule "PUBLIC")
    (token-rule "PRIVATE")
    (token-rule "PROTECTED")
    (token-rule "FRAGMENT")))

  (parser-rule "ruleBlock" (parser-subrule "ruleAltList"))

  (parser-rule
   "ruleAltList"
   (and-rule
    (parser-subrule "labeledAlt")
    (repeat (and-rule (token-rule "OR") (parser-subrule "labeledAlt")))))

  (parser-rule
   "labeledAlt"
   (and-rule
    (parser-subrule "alternative")
    (maybe (and-rule (token-rule "POUND") (parser-subrule "identifier")))))

  (parser-rule
   "lexerRuleSpec"
   (and-rule
    (maybe (token-rule "FRAGMENT"))
    (token-rule "TOKEN_REF")
    (maybe (parser-subrule "optionsSpec"))
    (token-rule "COLON")
    (parser-subrule "lexerRuleBlock")
    (token-rule "SEMI")))

  (parser-rule "lexerRuleBlock" (parser-subrule "lexerAltList"))

  (parser-rule
   "lexerAltList"
   (and-rule
    (parser-subrule "lexerAlt")
    (repeat (and-rule (token-rule "OR") (parser-subrule "lexerAlt")))))

  (parser-rule
   "lexerAlt"
   (maybe
    (and-rule (parser-subrule "lexerElements")
	      (maybe (parser-subrule "lexerCommands")))))

  (parser-rule
   "lexerElements"
   (maybe (one-or-more (parser-subrule "lexerElement"))))

  (parser-rule
   "lexerElement"
   (or-rule
    (and-rule (parser-subrule "labeledLexerElement") (maybe (parser-subrule "ebnfSuffix")))
    (and-rule (parser-subrule "lexerAtom") (maybe (parser-subrule "ebnfSuffix")))
    (and-rule (parser-subrule "lexerBlock") (maybe (parser-subrule "ebnfSuffix")))
    (and-rule (parser-subrule "actionBlock") (maybe (token-rule "QUESTION")))))

  (parser-rule
   "labeledLexerElement"
   (and-rule
    (parser-subrule "identifier")
    (or-rule (token-rule "ASSIGN") (token-rule "PLUS_ASSIGN"))
    (or-rule (parser-subrule "lexerAtom") (parser-subrule "lexerBlock"))))

  (parser-rule
   "lexerBlock"
   (and-rule (token-rule "LPAREN") (parser-subrule "lexerAltList") (token-rule "RPAREN")))

  (parser-rule
   "lexerCommands"
   (and-rule
    (token-rule "RARROW")
    (parser-subrule "lexerCommand")
    (repeat (and-rule (token-rule "COMMA") (parser-subrule "lexerCommand")))))

  (parser-rule
   "lexerCommand"
   (or-rule
    (and-rule
     (parser-subrule "lexerCommandName")
     (token-rule "LPAREN")
     (parser-subrule "lexerCommandExpr")
     (token-rule "RPAREN"))
    (parser-subrule "lexerCommandName")))

  (parser-rule
   "lexerCommandName"
   (or-rule (parser-subrule "identifier") (token-rule "MODE")))

  (parser-rule
   "lexerCommandExpr"
   (or-rule (parser-subrule "identifier") (token-rule "INT")))

  (parser-rule
   "altList"
   (and-rule (parser-subrule "alternative")
	     (repeat (and-rule (token-rule "OR") (parser-subrule "alternative")))))

  (parser-rule
   "alternative"
   (maybe (and-rule (maybe (parser-subrule "elementOptions"))
		    (one-or-more (parser-subrule "element")))))

  (parser-rule
   "element"
   (or-rule
    (and-rule (parser-subrule "labeledElement") (maybe (parser-subrule "ebnfSuffix")))
    (and-rule (parser-subrule "atom") (maybe (parser-subrule "ebnfSuffix")))
    (parser-subrule "ebnf")
    (and-rule (parser-subrule "actionBlock") (maybe (token-rule "QUESTION")))))

  (parser-rule
   "labeledElement"
   (and-rule
    (parser-subrule "identifier")
    (or-rule (token-rule "ASSIGN") (token-rule "PLUS_ASSIGN"))
    (or-rule (parser-subrule "atom") (parser-subrule "block"))))

  (parser-rule
   "ebnf"
   (and-rule (parser-subrule "block") (maybe (parser-subrule "blockSuffix"))))

  (parser-rule "blockSuffix" (parser-subrule "ebnfSuffix"))

  (parser-rule
   "ebnfSuffix"
   (or-rule
    (and-rule (token-rule "QUESTION") (maybe (token-rule "QUESTION")))
    (and-rule (token-rule "STAR") (maybe (token-rule "QUESTION")))
    (and-rule (token-rule "PLUS") (maybe (token-rule "QUESTION")))))

  (parser-rule
   "lexerAtom"
   (or-rule
    (parser-subrule "characterRange")
    (parser-subrule "terminal")
    (parser-subrule "notSet")
    (token-rule "LEXER_CHAR_SET")
    (and-rule (token-rule "DOT") (maybe (parser-subrule "elementOptions")))))

  (parser-rule
   "atom"
   (or-rule
    (parser-subrule "terminal")
    (parser-subrule "ruleref")
    (parser-subrule "notSet")
    (and-rule (token-rule "DOT") (maybe (parser-subrule "elementOptions")))))

  (parser-rule
   "notSet"
   (or-rule
    (and-rule (token-rule "NOT") (parser-subrule "setElement"))
    (and-rule (token-rule "NOT") (parser-subrule "blockSet"))))

  (parser-rule
   "blockSet"
   (and-rule
    (token-rule "LPAREN")
    (parser-subrule "setElement")
    (repeat (and-rule (or-rule (token-rule "OR") (parser-subrule "setElement"))))
    (token-rule "RPAREN")))

  (parser-rule
   "setElement"
   (or-rule
    (and-rule (token-rule "TOKEN_REF") (maybe (parser-subrule "elementOptions")))
    (and-rule (token-rule "STRING_LITERAL") (maybe (parser-subrule "elementOptions")))
    (parser-subrule "characterRange")
    (token-rule "LEXER_CHAR_SET")))

  (parser-rule
   "block"
   (and-rule (token-rule "LPAREN")
	     (maybe (and-rule (maybe (parser-subrule "optionsSpec"))
			      (repeat (parser-subrule "ruleAction"))
			      (token-rule "COLON")))
	     (parser-subrule "altList")
	     (token-rule "RPAREN")))

  (parser-rule
   "ruleref"
   (and-rule (token-rule "RULE_REF")
	     (maybe (parser-subrule "argActionBlock"))
	     (maybe (parser-subrule "elementOptions"))))

  (parser-rule
   "characterRange"
   (and-rule (token-rule "STRING_LITERAL") (token-rule "RANGE") (token-rule "STRING_LITERAL")))

  (parser-rule
   "terminal"
   (or-rule
    (and-rule (token-rule "TOKEN_REF") (maybe (parser-subrule "elementOptions")))
    (and-rule (token-rule "STRING_LITERAL") (maybe (parser-subrule "elementOptions")))))

  (parser-rule
   "elementOptions"
   (and-rule (token-rule "LT")
	     (parser-subrule "elementOption")
	     (repeat (and-rule (token-rule "COMMA") (parser-subrule "elementOption")))
	     (token-rule "GT")))

  (parser-rule
   "elementOption"
   (or-rule
    (parser-subrule "identifier")
    (and-rule (parser-subrule "identifier")
	      (token-rule "ASSIGN")
	      (or-rule (parser-subrule "identifier") (token-rule "STRING_LITERAL")))))

  (parser-rule "identifier" (or-rule (token-rule "RULE_REF") (token-rule "TOKEN_REF")))

  ) ; end eval-when

#|
;; Top level command for lexing the lexer
(let* ((parser-text (slurp-file (asdf:system-relative-pathname
				 'stantler
				 "StantlerLexer.g4")))
       (tokens (lex *antlr-lexer* parser-text 0))
       (tokens* (remove-if (lambda (x) (not (eq :default x)))
			   tokens
			   :key 'channel))
       (tokens** (mapcar #'convert-id tokens*)))
  (defparameter *tokens* (apply #'vector tokens**)))

(let ((root-node (parse-tree (parser-subrule "grammarSpec") *tokens* 0)))
	    (node-walk *ast-transform* root-node))
|#

