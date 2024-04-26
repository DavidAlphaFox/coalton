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
   #:SysError

   #:getenv
   #:setenv
   #:defenv
   
   #:architecture
   #:os
   #:hostname
   #:implementation
   #:lisp-version
   #:lisp-impl-directory
   #:configuration-pathnames
   #:features
   #:add-feature
   #:cmd-args
   #:argv0))

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

  (define-type SysError
    "An error message for system errors."
    (SysError String String))

  ;;
  ;; error handling options for System errors
  ;;
  
  (define-instance (Signal SysError)
    (define (error (SysError msg obj))
      (error (lisp String (msg obj)
               (cl:format cl:nil "System Error:~%~%~a ~a" msg obj))))
    (define (warn (SysError msg obj))
      (warn (lisp String (msg obj)
              (cl:format cl:nil "~%Coalton System: ~a ~a" msg obj)))))

  ;;
  ;; Accessing Environment Variables
  ;;
  
  (declare getenv (String -> (Result SysError String)))
  (define (getenv var)
    "Gets the value of the environmental variable `var`, errors if `var` doesn't exist."
    (lisp (Result SysError String) (var)
               (cl:let ((env (uiop:getenvp var)))
                 (cl:if env
                        (Ok env)
                        (Err (SysError "Environment variable not found:" var))))))

  
  (declare setenv (String -> String -> (Result SysError Unit)))
  (define (setenv var val)
    "Sets an environment variable `var` to string `val`, only if `var` already exists."
    (match (getenv var)
      ((Err x)
       (Err x))
      ((Ok _)
       (lisp Integer (var val)
           (cl:setf (uiop:getenv var) val))
       (Ok Unit))))

  (declare defenv (String -> String -> (Result SysError Unit)))
  (define (defenv var val)
    "Defines a new environmental variable `var`, sets it to `val`."
    (match (getenv var)
      ((Err _)
       (lisp Integer (var val)
           (cl:setf (uiop:getenv var) val))
       (Ok Unit))
      ((Ok _)
       (Err (SysError "Environment variable already exists:" var)))))

  ;;
  ;; Typical Environment/System variables
  ;;
  
  (declare architecture (String))
  (define architecture
    "The system's architecture (stored at compile time)."
    (lisp String ()
      (cl:string (uiop:architecture))))

  (declare os (String))
  (define os
    "The system's operating system (stored at compile time)."
    (lisp String ()
      (cl:string (uiop:detect-os))))

  (declare hostname (Unit -> String))
  (define (hostname)
    "Returns the system's hostname. This is a function because the hostname can be redefined."
    (lisp String ()
      (uiop:hostname)))

  (declare implementation (String))
  (define implementation
    "The lisp implementation (stored at compile time)."
    (lisp String ()
      (cl:string (uiop:implementation-type))))

  (declare lisp-version (String))
  (define lisp-version
    "The lisp implementation version (stored at compile time)."
    (lisp String ()
      (uiop:lisp-version-string)))

  (declare lisp-impl-directory (String))
  (define lisp-impl-directory
    "The lisp implementation's directory (stored at compile time)."
    (lisp String ()
      (cl:namestring (uiop:lisp-implementation-directory))))

  (declare configuration-pathnames (Unit -> (List String)))
  (define (configuration-pathnames)
    "Returns the list of directories which store default user configuration."
    (lisp (List String) ()
      (uiop:system-config-pathnames)))

  (declare features (Unit -> (List String)))
  (define (features)
    "Returns a list of active features, from `cl:*features*`."
    (lisp (list String) ()
      (cl:mapcar #'cl:string cl:*features*)))

  (declare add-feature (String -> Unit))
  (define (add-feature feat)
    "Adds a feature `feat` to `cl:*features*`."
    (lisp Boolean (feat)
      (cl:push (cl:intern feat "KEYWORD")
               cl:*features*)
      cl:t)
    Unit)

  ;;
  ;; Command line arguments
  ;;
  
  (declare cmd-args ((Result SysError (List String))))
  (define cmd-args
    "The current command line arguments (stored at compile time)."
    (lisp (Result SysError (List String)) ()
      (cl:let ((cla (uiop:command-line-arguments)))
        (cl:if (cl:null cla)
               (Err (SysError "No command line arguments found." ""))
               (Ok cla)))))

  (declare argv0 ((Result SysError String)))
  (define argv0
    "The first command line argument (stored at compile time)."
    (lisp (Result SysError String) ()
        (cl:if (uiop:argv0)
               (Ok (uiop:argv0))
               (Err (SysError "Argv0 not found." ""))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SYSTEM")
