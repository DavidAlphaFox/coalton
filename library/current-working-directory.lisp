(coalton-library/utils:defstdlib-package #:coalton-library/current-working-directory
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/tuple)
  (:local-nicknames (#:state #:coalton-library/monad/state)
                    (#:file #:coalton-library/file))
  (:export
   #:cwd
   #:cwd-relative-pathname
   #:pwd
   #:ls
   #:cd
   #:up
   #:down
   #:exists?
   #:files
   #:subdirs
   #:with-cwd
   #:with-system-cwd))

(in-package #:coalton-library/current-working-directory)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; Current Working Directory
;;;
;;; These functions use a State Monad to carry a current working pathname
;;;

;; potential fix for with-cwd list
#+ignore(coalton-toplevel
          (define-type CWDout
            (Empt)
            (Path file:Pathname)
            (Paths (List file:Pathname))
            (String-In String)
            (Strings-In (List String))))

(coalton-toplevel

  (declare cwd (Unit -> (State:st file:Pathname Unit)))
  (define (cwd)
    "Returns the current working directory."
    (do
     (path <- state:get)
     (state:put path)))
  
  (declare cwd-relative-pathname (String -> (State:st file:Pathname Unit)))
  (define (cwd-relative-pathname extension)
    "Returns the cwd relative pathname with a given extension."
    (do
     (path <- state:get)
     (state:put (file:merge path (into extension)))))

  (declare pwd (Unit -> (State:st file:Pathname Unit)))
  (define (pwd)
    "Prints the current working directory."
    (do
     (path <- state:get)
     (state:put path)
      (pure (traceobject "Current Working Directory" path))))

  (declare ls (Unit -> (State:st file:Pathname Unit)))
  (define (ls)
    "Prints the contents of the current working directory."
    (do
     (path <- state:get)
     (pure (file:print-directory-contents path))))

  (declare cd (file:Pathname -> (State:st file:Pathname Unit)))
  (define (cd new-path)
    "Changes the current directory to a new pathname."
    (do
     (state:put new-path)))

  (declare cd-system (String -> (State:st file:Pathname Unit)))
  (define (cd-system system-name)
    "Changes the directory to a given asdf system"
    (cd (file:system-relative-pathname system-name "")))

  (declare up (Unit -> (State:st file:Pathname Unit)))
  (define (up)
    "Moves the current directory one level up."
    (do
     (path <- state:get)
     (state:put (file:merge (into "../") path))))
  
  (declare down (String -> (State:st file:Pathname Unit)))
  (define (down sub-name)
    "Moves the current directory one level down to a given sub-pathname."
    (do
     (path <- state:get)
     (state:put (file:merge (into sub-name) path))))

  (declare exists? (String -> (State:st file:Pathname Unit)))
  (define (exists? filename)
    "Checks whether a file or directory exists within the current working directory. "
    (do
     (path <- state:get)
     (pure (traceobject "File Probe" (if (file:exists? (file:merge path (into filename)))
                                         filename
                                         "File not found")))))

  (declare cat (String -> (State:st file:Pathname Unit)))
  (define (cat filename)
    (do
     (path <- state:get)
     (pure (file:cat (file:merge path (into filename))))))

  (declare files (Unit -> (state:ST file:Pathname (List file:Pathname))))
  (define (files)
    "Returns all files within the current working directory"
    (do
     (dirpath <- state:get)
     (pure (file:directory-files dirpath))))

  (declare subdirs (Unit -> (state:ST file:Pathname (List file:Pathname))))
  (define (subdirs)
    "Returns all files within the current working directory"
    (do
     (dirpath <- state:get)
     (pure (file:subdirectories dirpath))))

  
  (declare write-to-file (String -> file:IFExistsOption -> file:IFDoesNotExistOption -> String -> (State:ST file:Pathname Unit)))
  (define (write-to-file filename if-exists if-does-not-exist data)
    (do
     (path <- state:get)
     (pure (file:write-to-file (fst (state:run (cwd-relative-pathname filename) path)) if-exists if-does-not-exist data)))))


(coalton-toplevel

  ;; TODO should eventually be a macro-
  ;; which may fix the typing issue
  (declare with-cwd (file:Pathname -> (List (State:st file:Pathname Unit)) -> (Tuple file:Pathname Unit)))
  (define (with-cwd cwd body)
    "Executes a body of functions in the current working directory"
    (match body
      ((Cons x (Nil))
       (state:run x cwd))
      ((Cons x xs)
       (with-cwd (fst (state:run x cwd)) xs))
      (_ (error "No commands provided to `with-cwd` form."))))

  (declare with-system-cwd (String -> (List (State:st file:Pathname Unit)) -> (Tuple file:Pathname Unit)))
  (define (with-system-cwd system body)
    "Executes a body of functions within an asdf system directory."
    (with-cwd (file:system-relative-pathname system "") body)))

;;
;;  A simple test case
;;
(coalton-toplevel

  (define (test1)
    (with-system-cwd "coalton"
      (make-list (pwd)
                 (cd-system "computable-reals")
                 (pwd)
                 (cd-system "coalton")
                 (pwd)
                 (exists? "coalton.asd")
                 (down "library/")
                 (pwd)
                 (exists? "file.lisp")
                 (cat "file.lisp")
                 (exists? "files.lisp")))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CURRENT-WORKING-DIRECTORY")
