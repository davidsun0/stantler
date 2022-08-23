;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "stantler"
  :version "0.0.1"
  :author "David Sun"
  :description "ANTLR4-compatible parser generator"
  :components
  ((:file "packages")
   (:file "utils" :depends-on ("packages"))
   (:file "matcher-rules" :depends-on ("packages" "utils"))
   (:file "lexer" :depends-on ("matcher-rules"))
   (:file "parser" :depends-on ("matcher-rules"))
   (:file "antlr" :depends-on ("lexer" "parser"))))
