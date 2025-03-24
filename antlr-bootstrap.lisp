(in-package #:stantler)

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

(defun lexer-rule (name rule &rest args &key (mode :default) &allow-other-keys)
  (vector-push-extend
   (apply 'make-instance 'lexer-rule :child rule :name name args)
   (mode-rules *antlr-lexer* mode)))

(defmethod mode-rules ((lexer lexer) name)
  (let ((mode (find name (modes lexer) :key 'name :test 'equal)))
    (if mode
	(rules mode)
	(let ((new-mode (make-instance 'lexer-mode :name name)))
	  (push new-mode (modes lexer))
	  (rules new-mode)))))

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

