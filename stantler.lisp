(in-package #:stantler)

(defun compile-lexer* (grammar-filespec output-filespec)
  (let* ((parser-text (slurp-file grammar-filespec))
	 (tokens (lex *antlr-lexer* parser-text 0))
	 (tokens* (remove-if (lambda (x) (not (eq x :default)))
			     tokens
			     :key 'channel))
	 (tokens** (apply #'vector (mapcar 'convert-id tokens*)))
	 (root-node (parse-tree (parser-subrule "grammarSpec") tokens** 0)))
    (compile-lexer (first (node-walk *ast-transform* root-node)) output-filespec)))

