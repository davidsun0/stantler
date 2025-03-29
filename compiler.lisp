(in-package #:stantler)

(defun transform-and (children)
  ;; Set stop rule for child lazy-repeat rules
  (loop for child in children
	for i from 0
	when (typep child 'lazy-repeat-rule)
	  do (setf (stop child) (nth (1+ i) children))
	finally (return (if (= 1 (length children))
			    (first children)
			    (make-instance 'and-rule :children children)))))

(defun transform-or (children)
  (make-instance 'or-rule :children children))

(defun apply-ebnf-suffix (rule suffix)
  "Modifies a rule with an EBNF suffix (one of ?, *, +, ??, *?, +?)."
  (cond
    ((null suffix) rule)
    ((string= suffix "?")
     (make-instance 'maybe-rule :child rule))
    ((string= suffix "*")
     (make-instance 'repeat-rule :child rule))
    ((string= suffix "+")
     (make-instance 'one-or-more-rule :child rule))
    ;; The context for the stop rule is not available at this level so NIL is
    ;; used as a placeholder. The stop rule is calculated when constructing
    ;; the parent and-rule.
    ((string= suffix "??")
     (make-instance 'lazy-maybe-rule :child rule :stop nil))
    ((string= suffix "*?")
     (make-instance 'lazy-repeat-rule :child rule :stop nil))
    ((string= suffix "+?")
     (make-instance 'lazy-one-or-more-rule :child rule :stop nil))))

;; Node Tree to Parser Rule Conversion ==========================================

(defmacro define-node-walker ((function-table name) lambda-list &body body)
  (with-gensyms (node)
    (let ((fn-name (make-symbol name))
	  (fn-body (if (lower-case-p (char name 0))
		       ;; Parser rule - destructure children
		       `(destructuring-bind ,lambda-list (children ,node)
			  ,@body)
		       ;; Lexer rule - access token slots
		       `(with-accessors ,(mapcar (lambda (x) (list x x))
					  lambda-list)
			    ,node
			  ,@body))))
      `(labels ((,fn-name (,node)
		  ,fn-body))
	 (setf (gethash ,name ,function-table)
	       #',fn-name)))))

(defun node-walk (function-table node)
  (let* ((name (name (rule node)))
	 (fn (gethash name function-table)))
    (if fn
	(funcall fn node)
	(error "No node walk function defined for ~S" name))))

(defvar *ast-transform*
  (make-hash-table :test 'equal))

(defun ast-transform (parse-node)
  (node-walk *ast-transform* parse-node))

(define-node-walker (*ast-transform* "grammarSpec")
		    (grammar-decl prequel-constructs rules mode-specs &optional eof)
  (declare (ignore prequel-constructs eof))
  (let* (*lexer-name*
	 (rules (ast-transform rules))
	 (parser-rules      (filter-type 'parser-subrule rules))
	 (lexer-rules       (filter-type 'lexer-rule     rules))
	 (default-fragments (filter-type 'fragment       rules))
	 (default-mode (make-instance 'lexer-mode
				      :children (coerce lexer-rules 'vector)
				      :name :default)))
    (declare (special *lexer-name*))
    (ast-transform grammar-decl)
    (list
     (loop for (mode fragments) in (mapcar 'ast-transform mode-specs)
	   collect mode into modes
	   nconc fragments into fragment-list
	   finally (return (make-instance 'lexer
					  :name *lexer-name*
					  :modes (list* default-mode modes)
					  :fragments (nconc default-fragments fragment-list))))
     (make-instance 'parser :rules parser-rules))))

(define-node-walker (*ast-transform* "grammarDecl") (type identifier semi)
  (declare (special *lexer-name*) (ignore semi))
  (let ((type-string (ast-transform type)))
    (when (string= type-string "lexer grammar")
      (setf *lexer-name* (value (ast-transform identifier))))
    (when (string= type-string "parser grammar")
      (error "Unimplemented"))
    (when (string= type-string "grammar")
      (error "Unimplemented"))))

(define-node-walker (*ast-transform* "grammarType") (first &optional second)
  ;; grammarType is one of "LEXER GRAMMAR", "PARSER GRAMMAR", or "GRAMMAR"
  ;; grammarType is interpreted in grammarDecl
  (format nil "~A~A~A" (content first) (if second " " "") (if second (content second) "")))

(define-node-walker (*ast-transform* "modeSpec") (mode identifier semi lexer-rule-specs)
  (declare (ignore mode semi))
  ;; Only lexers have modeSpecs. Therefore all child rules are lexer rules.
  (let* ((rules (mapcar 'ast-transform lexer-rule-specs))
	 (fragments (remove-if (lambda (r) (typep r 'lexer-rule)) rules))
	 (lexer-rules (remove-if (lambda (r) (typep r 'fragment)) rules)))
    (list
     (make-instance 'lexer-mode
		    :children (coerce lexer-rules 'vector)
		    :name (intern (string-upcase (content (first (children identifier))))
				  (find-package "KEYWORD")))
     fragments)))

(define-node-walker (*ast-transform* "parserRuleSpec")
  (rule-modifiers rule-ref arg-action-block rule-returns throws-spec locals-spec rule-prequel colon rule-block semi exception-group)
  (ast-transform rule-block))

(define-node-walker (*ast-transform* "lexerRuleSpec")
		    (fragment token-ref options-spec colon lexer-rule-block semi)
  (declare (ignore colon semi))
  (let ((fragment-p fragment)
	(name (content token-ref))
	(options (if options-spec
		     (ast-transform options-spec))))
    (make-instance (if fragment-p 'fragment 'lexer-rule)
		   :name name
		   :child (ast-transform lexer-rule-block))))

(define-node-walker (*ast-transform* "labeledAlt") (alternative &optional identifier)
  ;; TODO: what does the identifier do?
  (ast-transform alternative))

(define-node-walker (*ast-transform* "alternative") (&optional element-options elements)
  ;; TODO: element options
  (if elements
      (transform-and (mapcar 'ast-transform elements))
      nil))

(define-node-walker (*ast-transform* "lexerAlt") (&optional elements commands)
  ;; TODO: commands
  (if elements
      (ast-transform elements)
      nil))

(define-node-walker (*ast-transform* "element") (&rest values)
  ;; element has very different possible grammars
  (let ((first-rule (name (rule (first values)))))
    (cond
      ((or (string= "atom" first-rule)
	   (string= "labeledElement" first-rule))
       ;; atom ebnfSuffix?
       (destructuring-bind (atom suffix) values
	 (apply-ebnf-suffix
	  (ast-transform atom)
	  (if suffix
	      (ast-transform suffix)
	      nil))))
      ((string= "ebnf" first-rule)
       (ast-transform (first values)))
      ((string= "actionBlock" first-rule)
       ;; actionBlock has optional question
       (error "Unimplemented")))))

(define-node-walker (*ast-transform* "lexerElement") (&rest values)
  (let ((first-rule (name (rule (first values)))))
    (cond
      ((or (string= "labeledLexerElement" first-rule)
	   (string= "lexerAtom" first-rule)
	   (string= "lexerBlock" first-rule))
       (destructuring-bind (rule suffix) values
	 (apply-ebnf-suffix
	  (ast-transform rule)
	  (if suffix
	      (ast-transform suffix)
	      nil))))
      ((string= "actionBlock" first-rule)
       (destructuring-bind (action-block question) values
	 (error "Unimplemented"))))))

(define-node-walker (*ast-transform* "lexerAtom") (atom &optional element-options)
  ;; TODO: DOT can have element-options
  (declare (ignore element-options))
  (ast-transform atom))

(define-node-walker (*ast-transform* "atom") (rule &optional element-options)
  ;; DOT can have element-options
  (declare (ignore element-options))
  (ast-transform rule))

(define-node-walker (*ast-transform* "setElement") (element &optional element-options)
  ;; TODO: TOKEN_REF and STRING_LITERAL can have elementOptions
  (declare (ignore element-options))
  (ast-transform element))

(define-node-walker (*ast-transform* "notSet") (not element)
  (declare (ignore not))
  (make-instance 'not-rule :child (ast-transform element)))

(define-node-walker (*ast-transform* "block") (lparen options alt-list rparen)
  (declare (ignore lparen options rparen))
  ;; TODO: options parsing
  (ast-transform alt-list))

(define-node-walker (*ast-transform* "ruleref") (rule-ref arg-action-block element-options)
  (declare (ignore arg-action-block element-options))
  (make-instance 'parser-subrule :name (content rule-ref)))

(define-node-walker (*ast-transform* "terminal") (value element-options)
  (declare (ignore element-options))
  ;; TODO: all values can have element-options
  (ast-transform value))

(define-node-walker (*ast-transform* "ebnf") (block block-suffix)
  (apply-ebnf-suffix
   (ast-transform block)
   (if block-suffix
       (ast-transform block-suffix))))

(define-node-walker (*ast-transform* "ebnfSuffix") (modifier question)
  ;; Modifier is one of ?, *, or +. Question is an optional ?.
  ;; The only possible EBNF suffixes are ?, *, +, ??, *?, +?.
  (format nil "~A~A" (content modifier) (if question (content question) "")))

;; Trivial definitions

(define-node-walker (*ast-transform* "rules") (&rest rule-specs)
  (mapcar 'ast-transform rule-specs))

(define-node-walker (*ast-transform* "lexerElements") (&rest elements)
  (transform-and (mapcar 'ast-transform elements)))

(define-node-walker (*ast-transform* "ruleSpec") (rule)
  (ast-transform rule))

(define-node-walker (*ast-transform* "ruleBlock") (rule-alt-list)
  (ast-transform rule-alt-list))

(define-node-walker (*ast-transform* "lexerRuleBlock") (lexer-alt-list)
  (ast-transform lexer-alt-list))

(define-node-walker (*ast-transform* "lexerBlock") (lparen lexer-alt-list rparen)
  (declare (ignore lparen rparen))
  (ast-transform lexer-alt-list))

(define-node-walker (*ast-transform* "blockSuffix") (ebnf-suffix)
  (ast-transform ebnf-suffix))

(labels ((transform-alt-list (alternative alternatives)
	   ;; alternatives has the grammar of (OR alternatives)*
	   (let ((alts (cons alternative (mapcar 'second alternatives))))
	     (transform-or (mapcar 'ast-transform alts)))))

  (define-node-walker (*ast-transform* "ruleAltList")  (alternative alternatives)
    (transform-alt-list alternative alternatives))

  (define-node-walker (*ast-transform* "lexerAltList") (alternative alternatives)
    (transform-alt-list alternative alternatives))

  (define-node-walker (*ast-transform* "altList")      (alternative alternatives)
    (transform-alt-list alternative alternatives)))

(define-node-walker (*ast-transform* "identifier") (child)
  (ast-transform child))

;; Terminal token rules

(defun unescape-chars (string)
  "Converts escaped characters in an ANTLR string into their realized forms.
Returns a list of Lisp characters."
  (loop
    ;; Strip leading and trailing single / double quotes
    with set = (subseq string 1 (1- (length string)))
    with i = 0
    while (< i (length set))
    ;; TODO: Unicode escape sequences
    when (char= #\\ (char set i))
      collect (prog1 (case (char set (1+ i))
		       ((#\b) #\Backspace)
		       ((#\t) #\Tab)
		       ((#\n) +newline+)
		       ((#\f) #\Page)
		       ((#\r) +return+)
		       ((#\u)
			;; Unicode code point:
			;; Assumming the correct behavior is to parse as many
			;; digits as possible (up to 4)
			(loop with point = 0
			      for j from 1 upto 4
			      for digit = (let ((c (char set (+ 1 i j))))
					    (cond
					      ((char<= #\0 c #\9)
					       (- (char-code c) (char-code #\0)))
					      ((char<= #\a c #\f)
					       (- (char-code c) 87))
					      ((char<= #\A c #\F)
					       (- (char-code c) 55))
					      (t nil)))
			      while digit
			      do (setf point (+ (ash point 4) digit))
			      finally (incf i j)
				      (return (code-char point))))
		       (otherwise (char set (1+ i))))
		(incf i))
    else
      collect (char set i)
    do (incf i)))

(define-node-walker (*ast-transform* "STRING_LITERAL") (content)
  (let ((chars (unescape-chars content)))
    (if (= 1 (length chars))
	;; Should this optimization be moved into code generation?
	(make-instance 'character-rule
		       :value (first chars))
	(make-instance 'string-rule
		       :value (coerce chars 'string)))))

(define-node-walker (*ast-transform* "LEXER_CHAR_SET") (content)
  ;; The ANTLR implementation seems to compile a regex from the LEXER_CHAR_SET.
  ;; This feels a little strange though as other places in the ANTLR grammar don't
  ;; allow arbitrary regexes.
  ;; It feels like overkill to bring in a regex library just for character groups,
  ;; so this implements negation (^) and character ranges (-).
  (loop with chars = (unescape-chars content)
	with invert = nil
	  initially (when (eql #\^ (first chars))
		      (setf chars (rest chars))
		      (setf invert t))
	while chars
	when (eql #\- (second chars))
	  collect (make-instance 'char-range-rule :low (first chars) :high (third chars))
	    into subrules
	    and do (setf chars (cdddr chars))
	else
	  collect (make-instance 'character-rule :value (first chars)) into subrules
	  and do (setf chars (cdr chars))
	finally (let ((or-rule (make-instance 'or-rule :children subrules)))
		  (return (if invert
			      (make-instance 'not-rule :child or-rule)
			      or-rule)))))

(define-node-walker (*ast-transform* "characterRange") (low-string range high-string)
  (declare (ignore range))
  (let ((low  (first (unescape-chars (content low-string))))
	(high (first (unescape-chars (content high-string)))))
    (make-instance 'char-range-rule :low low :high high)))

(define-node-walker (*ast-transform* "TOKEN_REF") (content)
  (cond
    ((string= "EOF" content)
     (make-instance 'eof-rule))
    ;; TODO: fragment resolution
    (t
     (make-instance 'object-literal-rule
		    :value content
		    :comparison nil
		    :key 'rule))))

(define-node-walker (*ast-transform* "DOT") ()
  (make-instance 'wildcard-rule))

;; Fragment and lexer subrule resolution

(defmethod resolve-subrule (rule rule-list)
  rule)

(defmethod resolve-subrule ((rule child-mixin) rule-list)
  (setf (child rule)
	(resolve-subrule (child rule) rule-list))
  rule)

(defmethod resolve-subrule ((rule children-mixin) rule-list)
  (loop for child in (children rule)
	collect (resolve-subrule child rule-list) into c
	finally (setf (children rule) c))
  rule)

(defmethod resolve-subrule :after ((rule or-rule) rule-list)
  ;; For optimization, transform or-rule after subrules have been resolved:
  (let ((children))
    ;; Flatten nested or-rules
    (loop for child in (children rule)
	  nconcing (if (typep child 'or-rule)
		       (children child)
		       (list child))
	    into children*
	  finally (setf children children*))
    ;; Collect groups of character literals and character ranges
    (loop with char-group = '()
	  with children* = '()
	  for child in children
	  when (or (typep child 'char-range-rule)
		   (typep child 'character-rule))
	    do (push child char-group)
	  else
	    do (progn
		 (when char-group
		   (push (make-instance 'or-rule :children (nreverse char-group))
			 children*)
		   (setf char-group '()))
		 (push child children*))
	  finally (when char-group
		    (push (make-instance 'or-rule :children (nreverse char-group))
			  children*)
		    (setf char-group '()))
		  (setf children (nreverse children*)))
    (setf (children rule) children)))

(defmethod resolve-subrule ((rule object-literal-rule) rule-list)
  (cond
    ((null (comparison rule))
     ;; Inline fragments
     (let ((subrule (gethash (value rule) rule-list nil)))
       (unless subrule
	 (error "Reference to unknown rule ~A" (value rule)))
       ;;(child subrule)
       ;;subrule
       (make-instance 'fragment-reference-rule :child (value rule))
       ))
    (t rule)))

(defmethod resolve-subrule ((lexer lexer) rule-list)
  (declare (ignore rule-list))
  (let ((rule-list (make-hash-table :test 'equal)))
    ;; Collect all rules and fragments
    (loop for fragment in (fragments lexer)
	  do (setf (gethash (name fragment) rule-list) fragment))
    (loop for mode in (modes lexer)
	  do (loop for rule across (rules mode)
		   do (setf (gethash (name rule) rule-list) rule)))
    ;; Resolve rule references
    (loop for rule being the hash-values of rule-list
	  do (resolve-subrule rule rule-list))
    lexer))

;; Compile rule trees to Lisp ===================================================

(defgeneric compile-match (rule success failure start-form)
  (:documentation
   "Compiles `rule` into a Common Lisp form.
`success` is a function of one argument which takes the place of the number of
elements matched and produces a continuation form which will be evaluated if
`rule` successfully matches.
Likewise, `failure` takes the place of the number of elements matched and
produces a failure continuation form.
`start-form` is the form which contains the index at which to begin matching."))

(defmethod compile-match ((rule fragment-reference-rule) success failure start-form)
  (with-gensyms (fragment)
  `(let ((,fragment (,(intern (child rule)) input ,start-form)))
     (if ,fragment
	 ,(funcall success fragment)
	 ,(funcall failure nil)))))

(defmethod compile-match ((rule character-rule) success failure start-form)
  `(if ,(cond
	  ;; Unicode syntax is not standardized across Lisp implementations
	  ((> (char-code (value rule)) 127)
	   `(= ,(char-code (value rule)) (look-ahead input ,start-form)))
	  (t
	   `(char= ,(value rule) (look-ahead input ,start-form))))
       ,(funcall success 1)
       ,(funcall failure nil)))

(defmethod compile-match ((rule string-rule) success failure start-form)
  `(if (string= ,(value rule) input
		:start2 ,start-form
		:end2 (+ ,start-form ,(length (value rule))))
       ,(funcall success (length (value rule)))
       ,(funcall failure nil)))

(defmethod compile-match ((rule char-range-rule) success failure start-form)
  `(if ,(if (and (< (char-code (low rule))  127)
		 (< (char-code (high rule)) 127))
	    ;; Unicode syntax is not standardized across implementations
	    `(char<= ,(low rule) (look-ahead input ,start-form) ,(high rule))
	    `(<= ,(char-code (low rule)) (look-ahead input ,start-form) ,(char-code (high rule))))
       ,(funcall success 1)
       ,(funcall failure nil)))

(defmethod compile-match ((rule eof-rule) success failure start-form)
  `(if (>= ,start-form (length input))
       ,(funcall success 0)
       ,(funcall failure nil)))

(defmethod compile-match ((rule wildcard-rule) success failure start-form)
  ;; TODO: needs to handle EOF correctly
  `(if (look-ahead input ,start-form)
       ,(funcall success 1)
       ,(funcall failure nil)))

(defmethod compile-match ((rule maybe-rule) success failure start-form)
  (declare (ignore failure))
  (compile-match (child rule) success (constantly 0) start-form))

(defmethod compile-match ((rule not-rule) success failure start-form)
  (let ((subrule (compile-match (child rule) success failure start-form)))
    (cond
      ;; Optimization when the child rule produces a simple if (no nested ifs):
      ;; just reverese the then / else branches.
      ((and (eq 'if (first subrule))
	    (not (and (listp (third subrule))  (eq 'if (first (third subrule)))))
	    (not (and (listp (fourth subrule)) (eq 'if (first (fourth subrule))))))
       (destructuring-bind (if test then else)
	   (compile-match (child rule) 'identity failure start-form)
	 (declare (ignore if then))
	 ;; Not matching increments the start index by one
	 `(if ,test ,else ,(funcall success 1))))
      (t
       (with-gensyms (not-rule)
	 `(let ((,not-rule (compile-match (child rule) 'identity 'identity start-form)))
	    (if ,not-rule ,(funcall failure nil) ,(funcall success 1))))))))

(defmethod compile-match ((rule or-rule) success failure start-form)
  (cond
    ((= 1 (length (children rule)))
     (compile-match (first (children rule)) success failure start-form))
    ;; Optimization when matching a set of character literals
    ((every (lambda (r) (typep r 'character-rule)) (children rule))
     `(if (member (look-ahead input ,start-form)
		  ',(mapcar 'value (children rule))
		  :test 'char=)
	  ,(funcall success 1)
	  ,(funcall failure nil)))
    ;; Matching a list of chars or char-ranges
    ((every (lambda (r) (or (typep r 'char-range-rule)
			    (typep r 'character-rule)))
	    (children rule))
     (with-gensyms (char-code)
       `(if (let ((,char-code (char-code (look-ahead input ,start-form))))
	      (or ,@(mapcar
		     (lambda (r)
		       (cond
			 ((typep r 'char-range-rule)
			  `(<= ,(char-code (low r)) ,char-code ,(char-code (high r))))
			 (t
			  `(= ,(char-code (value r)) ,char-code))))
		     (children rule))))
	    ,(funcall success 1)
	    ,(funcall failure nil))))
    (t
     (with-gensyms (or-rule)
       `(block ,or-rule
	  ,@(mapcar (lambda (r)
		      (compile-match
		       r
		       (lambda (x)
			 `(return-from ,or-rule ,(funcall success x)))
		       (constantly nil)
		       start-form))
		    (children rule))
	  ,(funcall failure nil))))))

(defmethod compile-match ((rule and-rule) success failure start-form)
  (cond
    ((= 1 (length (children rule)))
     (compile-match (first (children rule)) success failure start-form))
    (t
     (with-gensyms (and-rule start length)
       `(block ,and-rule
	  (let ((,start ,start-form) (,length 0))
	    ,@(mapcar (lambda (r)
			(compile-match
			 r
			 (lambda (x)
			   `(incf ,length ,x))
			 (lambda (y)
			   `(return-from ,and-rule ,(funcall failure y)))
			 `(+ ,start ,length)))
		      (children rule))
	    (return-from ,and-rule ,(funcall success length))))))))

(defmethod compile-match ((rule repeat-rule) success failure start-form)
  (declare (ignore failure))
  (with-gensyms (repeat-rule start length)
    `(loop named ,repeat-rule
	   with ,start = ,start-form
	   with ,length = 0
	   do ,(compile-match
		(child rule)
		(lambda (x) `(incf ,length ,x))
		(constantly `(return-from ,repeat-rule ,(funcall success length)))
		start))))

(defmethod compile-match ((rule lazy-repeat-rule) success failure start-form)
  (with-gensyms (lazy-repeat start length)
    `(loop named ,lazy-repeat
	   with ,start = ,start-form
	   with ,length = 0
	   do ,(compile-match
		(stop rule)
		(lambda (x)
		  `(return-from ,lazy-repeat ,(funcall success `(+ ,length ,x))))
		;; Failure to match the stop rule does not mean the lazy-repeat-rule fails.
		(constantly nil)
		start)
	   ,(compile-match
	     (child rule)
	     (lambda (x) `(incf ,length ,x))
	     failure
	     start))))

(defmethod compile-match ((rule one-or-more-rule) success failure start-form)
  (with-gensyms (one-or-more-rule start length)
    `(loop named ,one-or-more-rule
	   with ,start = ,start-form
	   with ,length = 0
	     initially ,(compile-match
			 (child rule)
			 (lambda (x) `(incf ,length ,x))
			 (lambda (y) `(return-from ,one-or-more-rule ,(funcall failure y)))
			 start)
	   do ,(compile-match
		(child rule)
		(lambda (x) `(incf ,length ,x))
		(constantly `(return-from ,one-or-more-rule ,length))
		start))))

;;; Lexer Compilation ===========================================================

(defmethod compile-match ((rule fragment) success failure start-form)
  (declare (ignore success failure start-form))
  (let ((*gensym-counter* 0))
    `(defun ,(intern (name rule)) (input start)
       ,(compile-match (child rule) 'identity 'identity 'start))))

(defmethod compile-match ((rule lexer-rule) success failure start-form)
  (declare (ignore success failure start-form))
  (let ((*gensym-counter* 0))
    `(defun ,(intern (name rule)) (input start)
       ,(compile-match (child rule) 'identity 'identity 'start))))

(defmethod build-object ((rule lexer-rule))
  `(make-instance 'lexer-rule
		  :name ,(name rule)
		  :child ',(intern (name rule))))

(defmethod build-object ((mode lexer-mode))
  `(make-instance 'lexer-mode
		  :name ,(name mode)
		  :children ,(map 'vector 'build-object (children mode))))

(defmethod build-object ((lexer lexer))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; TODO: store in global var?
     (make-instance 'lexer
		    :name ,(name lexer)
		    :modes (list ,@(mapcar 'build-object (modes lexer))))))

(defun compile-lexer (lexer path)
  (with-open-file (file path :direction :output
			     :element-type 'character
			     :if-exists :supersede)
    ;; TODO: package preamble
    (let ((*print-case* :downcase)
	  (all-rules (make-hash-table :test 'equal)))
      (loop for fragment in (fragments lexer)
	    do (setf (gethash (name fragment) all-rules) fragment))
      (loop for mode in (modes lexer)
	    do (loop for rule across (children mode)
		     do (setf (gethash (name rule) all-rules) rule)))
      ;; Bare functions for fragments
      (loop for fragment in (fragments lexer)
	    do (pprint (compile-match (resolve-subrule fragment all-rules) 'identity 'identity 'start) file)
	       (format file "~%"))
      ;; Bare functions for lexer rules
      (loop for mode in (modes lexer)
	    do (loop for rule across (children mode)
		     do (pprint (compile-match (resolve-subrule rule all-rules) 'identity 'identity 'start) file)
			(format file "~%")))
      ;; Building lexer rules
      (pprint (build-object lexer) file)))
  lexer)
