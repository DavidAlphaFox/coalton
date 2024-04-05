(coalton-library/utils:defstdlib-package #:coalton-library/split
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:types #:coalton-library/types))
  (:export
   #:splittable
   #:split))


(in-package #:coalton-library/split)

(named-readtables:in-readtable coalton:coalton)


;;;
;;; Split
;;;

(coalton-toplevel

  (define-class (Splittable :seq)
    "Sequence types that can be split by element equality."
    (split ((Eq :a) (types:runtimerepr :a) => :a -> (:seq :a) -> (iter:iterator (:seq :a))))))



;; TODO (Maybe):
;; Add instance for Slice
