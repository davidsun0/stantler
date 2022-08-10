(in-package #:stantler)

(defclass children-mixin ()
  ((children%
    :reader children
    :initarg :children)))

(defclass child-mixin ()
  ((child%
    :reader child
    :initarg :child)))

;; Compatibility because #\Newline is platform-dependent

(defconstant +return+ (code-char #xA)
  "Carriage Return character")

(defconstant +newline+ (code-char #xD)
  "Newline character")

(define-condition eof-error (error)
  ((index%
    :reader index
    :initarg :index))
  (:documentation "Error for reading past the end of a character or token stream."))
