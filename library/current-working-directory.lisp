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

;;;
;;; Navigation, exploration, and probing
;;;

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

  (declare exists? (String -> (State:st file:Pathname Boolean)))
  (define (exists? filename)
    "Checks whether a file or directory exists within the current working directory. "
    (do
     (path <- state:get)
     (pure (let ((ex (file:exists? (file:merge path (into filename)))))
             (traceobject filename (if ex "File Found" "File Not Found"))
             ex))))

  (declare cat (String -> (State:st file:Pathname Unit)))
  (define (cat filename)
    "Prints the file to standard-output."
    (do
     (path <- state:get)
     (pure (file:cat (file:merge path (into filename)))))))

;;
;; Interacting directories
;;

(coalton-toplevel

  (declare mkdir (String -> (state:ST file:Pathname Unit)))
  (define (mkdir dirname)
    "Makes a subdirectory in the current working directory."
    (do
     (path <- state:get)
     (pure (file:create-directory (file:merge path (into dirname))))))

  (declare rmdir (String -> (state:ST file:Pathname Unit)))
  (define (rmdir dirname)
    "Removes a subdirectory from the current working directory."
    (do
     (path <- state:get)
     (pure (file:delete-directory (file:merge path (into dirname))))))
  
  (declare files (Unit -> (state:ST file:Pathname (List file:Pathname))))
  (define (files)
    "Returns all files within the current working directory."
    (do
     (dirpath <- state:get)
     (pure (file:directory-files dirpath))))

  (declare subdirs (Unit -> (state:ST file:Pathname (List file:Pathname))))
  (define (subdirs)
    "Returns all files within the current working directory."
    (do
     (dirpath <- state:get)
     (pure (file:subdirectories dirpath)))))

;;
;; Reading and writing to files
;;

(coalton-toplevel

  (declare copy (String -> String -> (state:ST file:Pathname Unit)))
  (define (copy input-filename output-filename)
    "Copies a file to an output file in the same directory."
    (do
     (path <- state:get)
     (pure (file:copy (file:merge path (into input-filename))
                      (file:merge path (into output-filename))))))

  (declare move-to (String -> file:Pathname -> (state:ST file:Pathname Unit)))
  (define (move-to filename directory)
    "Moves a file from the current working directory to a given directory."
    (do
     (path <- state:get)
     (let ((filepath (file:merge path (into filename))))
       (pure (progn (file:copy filepath directory)
                    (file:delete filepath))))))

  (declare rename (String -> String -> (state:ST file:Pathname Unit)))
  (define (rename filename1 filename2)
    "Renames a file (destructive)."
    (do
     (path <- state:get)
     (let ((f1 (file:merge path (into filename1)))
           (f2 (file:merge path (into filename2))))
       (pure (progn (file:copy f1 f2)
                    (file:delete f1))))))
  
  (declare write (String -> file:IFExistsOption -> file:IFDoesNotExistOption -> String -> (State:ST file:Pathname Unit)))
  (define (write filename if-exists if-does-not-exist data)
    "Writes a given data-string to a file."
    (do
        (path <- state:get)
        (pure (file:write-to-file (file:merge path (into filename)) if-exists if-does-not-exist data))))

  (declare read->string (String -> (State:ST file:Pathname (Optional String))))
  (define (read->string filename)
    "Reads a file into an `(Optional String)`."
    (do
     (path <- state:get)
     (pure (file:file->string (file:merge path (into filename))))))

  (declare read->line (String -> UFix -> (State:ST file:Pathname (Optional String))))
  (define (read->line filename n)
    "Reads a designated line of a file into an `(Optional String)`."
    (do
     (path <- state:get)
     (pure (file:file->line (file:merge path (into filename)) n))))

  (declare read->lines (String -> (State:ST file:Pathname (List String))))
  (define (read->lines filename)
    "Reads a file into a list of line strings."
    (do
     (path <- state:get)
     (pure (file:file->lines (file:merge path (into filename)))))))


(coalton-toplevel

  (declare with-cwd (file:Pathname -> (State:st file:Pathname :a) -> (Tuple file:Pathname :a)))
  (define (with-cwd cwd body)
    "Executes a body of functions in a given working directory"
    (state:run body cwd))

  (declare with-system-cwd (String -> (State:st file:Pathname :a) -> (Tuple file:Pathname :a)))
  (define (with-system-cwd system body)
    "Executes a body of functions within an asdf system directory."
    (with-cwd (file:system-relative-pathname system "") body)))

;;
;;  A simple test case
;;

;; get path, set path , etc

;; look into haskell io monad
(coalton-toplevel

  (define (test1)
    (with-system-cwd "coalton"
      (do
       (pwd)
       (cd-system "computable-reals")
        (pwd)
        (cd-system "coalton")
        (pwd)
        (exists? "coalton.asd")
        (down "library/")
        (pwd)
        (up)
        (pwd)
        (down "library/")
        (pwd)
        (exists? "files.lisp")
        (exists? "file.lisp")
        (read->line "file.lisp" 0))))

  (define (test2)
    (with-system-cwd "coalton"
      (do
       (down "examples/small-coalton-programs/src/")
        (cwd)
        (write "example-file.txt~" file:Supersede file:Create "Wow this works")
        (cat "example-file.txt~")
        (write "example-file.txt~" file:Supersede file:Create "Wow this *still* works")
        (cat "example-file.txt~")))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CURRENT-WORKING-DIRECTORY")
