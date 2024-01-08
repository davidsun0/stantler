#|
ANTLR4 Lexer / Parser
|#

(in-package #:stantler)

(defparameter *antlr-lexer*
  (make-instance 'lexer))


#|
Bootstrap Lexer / Parser
|#

(defun literal (literal)
  (cond
    ((characterp literal)
     (char-rule literal))
    ((stringp literal)
     (string-rule literal))))

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

(defun not-rule (child)
  (make-instance 'not-rule :child child))

(defun lexer-rule (name rule &rest args &key (mode :default) &allow-other-keys)
  (vector-push-extend
   (apply 'make-instance 'lexer-rule :child rule :name name args)
   (mode-rules *antlr-lexer* mode)))

(let ((wildcard (make-instance 'wildcard-rule)))
  (setf *antlr-lexer* (make-instance 'lexer))
  (lexer-rule "DOC_COMMENT"
	      (and-rule (literal "/**") (lazy-repeat wildcard (literal "*/")))
	      :channel "COMMENT")
  (lexer-rule "BLOCK_COMMENT"
	      (And-rule (literal "/*") (lazy-repeat wildcard (literal "*/")))
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
						   (or-rule
						    (literal #\b)
						    (literal #\t)
						    (literal #\n)
						    (literal #\f)
						    (literal #\r)
						    (literal #\")
						    (literal #\')
						    (literal #\\)))
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

  ) ; end let

(defun find-rule (name lexer)
  (loop for mode in (modes lexer)
	do (loop for rule across (rules mode)
		 when (string= name (name rule))
		   do (return-from find-rule rule))))

(defun token-rule (rule-name)
  (make-instance 'object-literal-rule
		 :value (find-rule rule-name *antlr-lexer*)
		 :comparison 'eq
		 :key 'rule))

(defun maybe (child)
  (make-instance 'maybe-rule :child child))

(defclass parser-rule-reference ()
  ((name
    :initarg :name
    :reader name)))

(defun parser-ref (rule-name)
  (make-instance 'parser-rule-reference :name rule-name))


;;; ANTLR Lexer converts ID tokens to either TOKEN_REF or RULE_REF based on text case
;;; Dummy rules are used because token-rule compares rules by eq
(lexer-rule "TOKEN_REF" nil)
(lexer-rule "RULE_REF" nil)

(defun convert-id (token)
  (when (string= "ID" (name (rule token)))
    (setf (rule token)
	  (find-rule
	   (if (upper-case-p (aref (content token) 0))
	       "TOKEN_REF"
	       "RULE_REF")
	   *antlr-lexer*)))
  token)

(defclass parser ()
  ((rules%
    :reader rules
    :initform (make-hash-table :size 16 :test 'equal))))

(defparameter *antlr-parser*
  (make-instance 'parser))

(defun parser-rule (name rule)
  (setf (gethash name (rules *antlr-parser*)) rule))

(defclass parser-subrule ()
  ((name%
    :initarg :name
    :reader name)))

(defmethod match (input (rule parser-subrule) start)
  (let ((subrule (gethash (name rule) (rules *antlr-parser*))))
    (format t "~A ~A~%" (name rule) start)
    (match input subrule start)))

(defgeneric replace-parser-refs (rule)
  (:method ((rule t)) t)
  (:method ((child-rule child-mixin))
    (let ((child (child child-rule)))
      (if (typep child 'parser-rule-reference)
	  (setf (child child-rule)
		(make-instance 'parser-subrule :name (name child)))
	  (replace-parser-refs child))))
  (:method ((children-rule children-mixin))
    (loop for children on (children children-rule)
	  for child = (car children)
	  if (typep child 'parser-rule-reference)
	    do (setf (car children)
		     (make-instance 'parser-subrule :name (name child)))
	  else
	    do (replace-parser-refs child)))
  (:method ((parser parser))
    (loop for value being the hash-values of (rules parser)
	  do (replace-parser-refs value))))

(parser-rule
 "grammarSpec"
 (and-rule (parser-ref "grammarDecl")
	   (repeat (parser-ref "prequelConstruct"))
	   (parser-ref "rules")
	   (repeat (parser-ref "modeSpec"))
	   (token-rule "EOF")))

(parser-rule
 "grammarDecl"
 (and-rule (parser-ref "grammarType")
	   (parser-ref "identifier")
	   (token-rule "SEMI")))

(parser-rule
 "grammarType"
 (or-rule (and-rule (token-rule "LEXER") (token-rule "GRAMMAR"))
	  (and-rule (token-rule "PARSER") (token-rule "GRAMMAR"))
	  (token-rule "GRAMMAR")))

(parser-rule
 "prequelConstruct"
 (or-rule (parser-ref "optionsSpec")
	  (parser-ref "delegateGrammars")
	  (parser-ref "tokensSpec")
	  (parser-ref "channelsSpec")
	  (parser-ref "action_")))

(parser-rule
 "optionsSpec"
 (and-rule (token-rule "OPTIONS")
	   (repeat (and-rule (parser-ref "option")
				   (token-rule "SEMI")))
	   (token-rule "RBRACE")))

(parser-rule
 "option"
 (and-rule (parser-ref "identifier")
	   (token-rule "ASSIGN")
	   (parser-ref "optionValue")))

(parser-rule
 "optionValue"
 (or-rule
  (and-rule (parser-ref "identifier")
	    (repeat (and-rule (token-rule "DOT")
				    (parser-ref "identifier"))))
  (token-rule "STRING_LITERAL")
  (parser-ref "actionBlock")
  (token-rule "INT")))

(parser-rule
 "delegateGrammar"
 (and-rule (parser-ref "identifier")
	   (maybe (and-rule (token-rule "ASSIGN")
				 (parser-ref "identifier")))))

(parser-rule
 "tokensSpec"
 (and-rule (token-rule "TOKENS")
	   (maybe (parser-ref "idList"))
	   (token-rule "RBRACE")))

(parser-rule
 "channelsSpec"
 (and-rule (token-rule "CHANNELS")
	   (maybe (parser-ref "idList"))
	   (token-rule "RBRACE")))

(parser-rule
 "idList"
 (and-rule (parser-ref "identifier")
	   (repeat (and-rule (token-rule "COMMA")
				   (parser-ref "identifier")))
	   (maybe (token-rule "COMMA"))))

(parser-rule
 "action_"
 (and-rule (token-rule "AT")
	   (maybe (and-rule (parser-ref "actionScopeName")
				 (token-rule "COLONCOLON")))
	   (parser-ref "identifier")
	   (parser-ref "actionBlock")))

(parser-rule
 "actionScopeName"
 (or-rule (parser-ref "identifier")
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
	   (parser-ref "identifier")
	   (token-rule "SEMI")
	   (repeat (parser-ref "lexerRuleSpec"))))

(parser-rule
 "rules"
 (repeat (parser-ref "ruleSpec")))

(parser-rule
 "ruleSpec"
 (or-rule (parser-ref "parserRuleSpec")
	  (parser-ref "lexerRuleSpec")))

(parser-rule
 "parserRuleSpec"
 (and-rule
  (maybe (parser-ref "ruleModifiers"))
  (token-rule "RULE_REF")
  (maybe (parser-ref "argActionBlock"))
  (maybe (parser-ref "ruleReturns"))
  (maybe (parser-ref "throwsSpec"))
  (maybe (parser-ref "localsSpec"))
  (repeat (parser-ref "rulePrequel"))
  (token-rule "COLON")
  (parser-ref "ruleBlock")
  (token-rule "SEMI")
  (parser-ref "exceptionGroup")))

(parser-rule
 "exceptionGroup"
 (and-rule
  (repeat (parser-ref "exceptionHandler"))
  (maybe (parser-ref "finlalyClause"))))

(parser-rule
 "exceptionHandler"
 (and-rule
  (token-rule "CATCH")
  (parser-ref "argActionBlock")
  (parser-ref "actionBlock")))

(parser-rule
 "finallyClause"
 (and-rule
  (token-rule "FINALLY")
  (parser-ref "actionBlock")))

(parser-rule
 "rulePrequel"
 (or-rule (parser-ref "optionsSpec") (parser-ref "ruleAction")))

(parser-rule
 "ruleReturns"
 (and-rule (token-rule "RETURNS") (parser-ref "argActionBlock")))

(parser-rule
 "throwsSpec"
 (and-rule
  (token-rule "THROWS")
  (parser-ref "identifier")
  (repeat (and-rule (token-rule "COMMA") (parser-ref "identifier")))))

(parser-rule
 "localsSpec"
 (and-rule (token-rule "LOCALS") (parser-ref "argAtionBlock")))

(parser-rule
 "ruleAction"
 (and-rule
  (token-rule "AT")
  (parser-ref "identifier")
  (parser-ref "actionBlock")))

(parser-rule
 "ruleModifiers"
 (and-rule
  (parser-ref "ruleModifier")
  (repeat (parser-ref "ruleModifier"))))

(parser-rule
 "ruleModifier"
 (or-rule
  (token-rule "PUBLIC")
  (token-rule "PRIVATE")
  (token-rule "PROTECTED")
  (token-rule "FRAGMENT")))

(parser-rule "ruleBlock" (parser-ref "ruleAltList"))

(parser-rule
 "ruleAltList"
 (and-rule
  (parser-ref "labeledAlt")
  (repeat (and-rule (token-rule "OR") (parser-ref "labeledAlt")))))

(parser-rule
 "labeledAlt"
 (and-rule
  (parser-ref "alternative")
  (maybe (and-rule (token-rule "POUND") (parser-ref "identifier")))))

(parser-rule
 "lexerRuleSpec"
 (and-rule
  (maybe (token-rule "FRAGMENT"))
  (token-rule "TOKEN_REF")
  (maybe (parser-ref "optionsSpec"))
  (token-rule "COLON")
  (parser-ref "lexerRuleBlock")
  (token-rule "SEMI")))

(parser-rule "lexerRuleBlock" (parser-ref "lexerAltList"))

(parser-rule
 "lexerAltList"
 (and-rule
  (parser-ref "lexerAlt")
  (repeat (and-rule (token-rule "OR") (parser-ref "lexerAlt")))))

(parser-rule
 "lexerAlt"
 (maybe
  (and-rule (parser-ref "lexerElements")
	    (maybe (parser-ref "lexerCommands")))))

(parser-rule
 "lexerElements"
 (maybe (and-rule (token-rule "lexerElement")
		  (repeat (token-rule "lexerElement")))))

(parser-rule
 "lexerElement"
 (or-rule
  (and-rule (parser-ref "labeledLexerElement") (maybe (parser-ref "ebnfSuffix")))
  (and-rule (parser-ref "lexerAtom") (maybe (parser-ref "ebnfSuffix")))
  (and-rule (parser-ref "lexerBlock") (maybe (parser-ref "ebnfSuffix")))
  (and-rule (parser-ref "actionBlock") (maybe (token-rule "QUESTION")))))

(parser-rule
 "labeledLexerElement"
 (and-rule
  (parser-ref "identifier")
  (or-rule (token-rule "ASSIGN") (token-rule "PLUS_ASSIGN"))
  (or-rule (parser-ref "lexerAtom") (parser-ref "lexerBlock"))))

(parser-rule
 "lexerBlock"
 (and-rule (token-rule "LPAREN") (parser-ref "lexerAltList") (token-rule "RPAREN")))

(parser-rule
 "lexerCommands"
 (and-rule
  (token-rule "RARROW")
  (parser-ref "lexerCommand")
  (repeat (and-rule (token-rule "COMMA") (parser-ref "lexerCommand")))))

(parser-rule
 "lexerCommand"
 (or-rule
  (and-rule
   (parser-ref "lexerCommandName")
   (token-rule "LPAREN")
   (parser-ref "lexerCommandExpr")
   (token-rule "RPAREN"))
  (parser-ref "lexerCommandName")))

(parser-rule
 "lexerCommandName"
 (or-rule (parser-ref "identifier") (token-rule "MODE")))

(parser-rule
 "lexerCommandExpr"
 (or-rule (parser-ref "identifier") (token-rule "INT")))

(parser-rule
 "altList"
 (and-rule (parser-ref "alternative")
	   (repeat (and-rule (token-rule "OR") (parser-ref "alternative")))))

(parser-rule
 "alternative"
 (and-rule (maybe (parser-ref "elementOptions"))
	   (parser-ref "element")
	   (repeat (parser-ref "element"))))

(parser-rule
 "element"
 (or-rule
  (and-rule (parser-ref "labeledElement") (maybe (parser-ref "ebnfSuffix")))
  (and-rule (parser-ref "atom") (maybe (parser-ref "ebnfSuffix")))
  (parser-ref "ebnf")
  (and-rule (parser-ref "actionBlock") (maybe (token-rule "QUESTION")))))

(parser-rule
 "labeledElement"
 (and-rule
  (parser-ref "identifier")
  (or-rule (token-rule "ASSIGN") (token-rule "PLUS_ASSIGN"))
  (or-rule (parser-ref "atom") (parser-ref "block"))))

(parser-rule
 "ebnf"
 (and-rule (parser-ref "block") (maybe (parser-ref "blockSuffix"))))

(parser-rule "blockSuffix" (parser-ref "ebnfSuffix"))

(parser-rule
 "ebnfSuffix"
 (or-rule
  (and-rule (token-rule "QUESTION") (maybe (token-rule "QUESTION")))
  (and-rule (token-rule "STAR") (maybe (token-rule "QUESTION")))
  (and-rule (token-rule "PLUS") (maybe (token-rule "QUESTION")))))

(parser-rule
 "lexerAtom"
 (or-rule
  (parser-ref "characterRange")
  (parser-ref "terminal")
  (parser-ref "notSet")
  (token-rule "LEXER_CHAR_SET")
  (and-rule (token-rule "DOT") (maybe (parser-ref "elementOptions")))))

(parser-rule
 "atom"
 (or-rule
  (parser-ref "terminal")
  (parser-ref "ruleref")
  (parser-ref "notSet")
  (and-rule (token-rule "DOT") (maybe (parser-ref "elementOptions")))))

(parser-rule
 "notSet"
 (or-rule
  (and-rule (token-rule "NOT") (parser-ref "setElement"))
  (and-rule (token-rule "NOT") (parser-ref "blockSet"))))

(parser-rule
 "blockSet"
 (and-rule
  (token-rule "LPAREN")
  (parser-ref "setElement")
  (repeat (and-rule (or-rule (token-rule "OR") (parser-ref "setElement"))))
  (token-rule "RPAREN")))

(parser-rule
 "setElement"
 (or-rule
  (and-rule (token-rule "TOKEN_REF") (maybe (parser-ref "elementOptions")))
  (and-rule (token-rule "STRING_LITERAL") (maybe (parser-ref "elementOptions")))
  (parser-ref "characterRange")
  (token-rule "LEXER_CHAR_SET")))

(parser-rule
 "block"
 (and-rule (token-rule "LPAREN")
	   (maybe (and-rule (maybe (parser-ref "optionsSpec"))
			   (repeat (parser-ref "ruleAction"))
			   (token-rule "COLON")))
	   (parser-ref "altList")
	   (token-rule "RPAREN")))

(parser-rule
 "ruleref"
 (and-rule (token-rule "RULE_REF")
	   (maybe (parser-ref "argActionBlock"))
	   (maybe (parser-ref "elementOptions"))))

(parser-rule
 "characterRange"
 (and-rule (token-rule "STRING_LITERAL") (parser-ref "range") (token-rule "STRING_LITERAL")))

(parser-rule
 "terminal"
 (or-rule
  (and-rule (token-rule "TOKEN_REF") (maybe (parser-ref "elementOptions")))
  (and-rule (token-rule "STRING_LITERAL") (maybe (parser-ref "elementOptions")))))

(parser-rule
 "elementOptions"
 (and-rule (token-rule "LT")
	   (parser-ref "elementOption")
	   (repeat (and-rule (token-rule "COMMA") (parser-ref "elementOption")))
	   (token-rule "GT")))

(parser-rule
 "elementOption"
 (or-rule
  (parser-ref "identifier")
  (and-rule (parser-ref "identifier")
	    (token-rule "ASSIGN")
	    (or-rule (parser-ref "identifier") (token-rule "STRING_LITERAL")))))

(parser-rule "identifier" (or-rule (token-rule "RULE_REF") (token-rule "TOKEN_REF")))

;;(replace-parser-refs *antlr-parser*)

;; Top level command for lexing the lexer
(let* ((parser-text (slurp-file (asdf:system-relative-pathname
				 'stantler
				 "ANTLRv4Parser.g4")))
       (tokens (lex parser-text *antlr-lexer* 0))
       (tokens* (remove-if (lambda (x) (not (eq :default x)))
			   tokens
			   :key 'channel))
       (tokens** (mapcar #'convert-id tokens*)))
  (defparameter *tokens* (apply #'vector tokens**)))

