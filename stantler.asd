;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "stantler"
  :version "0.0.1"
  :author "David Sun"
  :description "ANTLR4-compatible parser generator."
  :components
  ((:file "packages")
   (:file "lexer" :depends-on ("packages"))
   (:file "compound-rules" :depends-on ("packages"))
   (:file "bootstrap-lexer" :depends-on ("lexer" "compound-rules"))))
