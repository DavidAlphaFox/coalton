(coalton-library/utils:defstdlib-package #:coalton-library/system
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:export
   #:gc
   #:time
   #:sleep)
  (:export
   #:Keyword
   #:which-architecture
   #:which-os
   #:which-hostname
   #:which-implementation
   #:which-lisp-version
   #:which-lisp-impl-directory
   #:which-configuration-pathnames
   #:which-features
   #:which-cmd-args
   #:which-argv0))

(in-package #:coalton-library/system)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (declare gc (Unit -> Unit))
  (define (gc _)
    "Perform a full garbage collection."
    (lisp Unit ()
      (trivial-garbage:gc :full cl:t)
      Unit))

  (declare time ((Unit -> :a) -> (Tuple :a Integer)))
  (define (time f)
    "Run the thunk `f` and return a tuple containing its value along with the run time in microseconds.

While the result will always contain microseconds, some implementations may return a value rounded to less precision (e.g., rounded to the nearest second or millisecond)."
    (let start = (lisp Integer () (cl:get-internal-run-time)))
    (let value = (f))
    (let end   = (lisp Integer () (cl:get-internal-run-time)))
    (Tuple value
           (lisp Integer (start end)
             (cl:values
              (cl:round
               (cl:* 1000000 (cl:- end start))
               cl:internal-time-units-per-second)))))

  (declare sleep (Integer -> Unit))
  (define (sleep n)
    "Sleep for `n` seconds."
    (lisp Unit (n)
      (cl:sleep n)
      Unit)))

;;;
;;; Gathering System information
;;;

(coalton-toplevel

  (repr :native cl:keyword)
  (define-type Keyword
    "A Keyword represented by a Common Lisp keyword.")
  
  (declare which-architecture (Unit -> Keyword))
 (define (which-architecture)
   "Returns your system's architecture."
   (lisp Keyword ()
     (uiop:architecture)))

 (declare which-os (Unit -> Keyword))
 (define (which-os)
   "Returns your system's Operating System."
   (lisp Keyword ()
     (uiop:detect-os)))

 (declare which-hostname (Unit -> String))
 (define (which-hostname)
   "Returns your system's Hostname."
   (lisp String ()
         (uiop:hostname)))

 (declare which-implementation (Unit -> Keyword))
 (define (which-implementation)
   "Returns your lisp implementation."
   (lisp Keyword ()
     (uiop:implementation-type)))

 (declare which-lisp-version (Unit -> String))
 (define (which-lisp-version)
   "Returns the version of your lisp implementation."
   (lisp String ()
         (uiop:lisp-version-string)))

 (declare which-lisp-impl-directory (Unit -> String))
 (define (which-lisp-impl-directory)
   "Returns your lisp implementation's directory."
   (lisp String ()
         (cl:namestring (uiop:lisp-implementation-directory))))

 (declare which-configuration-pathnames (Unit -> (List String)))
 (define (which-configuration-pathnames)
   "Returns a list of configuration pathnames."
   (lisp (List String) ()
         (uiop:system-config-pathnames)))

 (declare which-features (Unit -> (List Keyword)))
 (define (which-features)
   "Returns a list of active features, from `cl:*features*`."
   (lisp (list Keyword) ()
         cl:*features*))

 (declare which-cmd-args (Unit -> (List String)))
 (define (which-cmd-args)
   "Returns the current command line arguments."
   (lisp (List String) ()
         (uiop:command-line-arguments)))

 (declare which-argv0 (Unit -> String))
 (define (which-argv0)
   "Returns the argv0, first command line argument."
   (lisp String ()
         (uiop:argv0))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SYSTEM")
