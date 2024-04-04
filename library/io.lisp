(coalton-library/utils:defstdlib-package #:coalton-library/io
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)
  (:local-nicknames (#:str #:coalton-library/string)
                    (#:iter #:coalton-library/iterator)
                    (#:cell #:coalton-library/cell)
                    (#:list #:coalton-library/list)
                    (#:vec #:coalton-library/vector))
  (:export
   ;; system information
   #:which-architecture
   #:which-os
   #:which-hostname
   #:which-implementation
   #:which-lisp-version
   #:which-lisp-impl-directory
   #:which-configuration-pathnames
   #:which-features
   #:which-cmd-args
   #:which-argv0

   ;; FilePath type
   #:FilePath
   #:dir-chain
   #:filename
   #:filetype
   #:filepath->string
   #:string->filepath
   #:parent-directories
   #:file-type
   #:file-name

   ;; directory functions
   #:directory-exists-p
   #:directory-files
   #:subdirectories
   #:print-directory-contents
   #:system-relative-pathname

   ;; file functions
   #:file-exists-p
   #:concat-files
   #:copy-file
   #:file->string
   #:file->line
   #:file->lines))

(in-package #:coalton-library/io)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; system information
;;;

(coalton-toplevel

 (declare which-architecture (Unit -> String))
 (define (which-architecture)
   "Returns your system's architecture."
   (lisp String ()
     (cl:subseq (cl:write-to-string (uiop:architecture)) 1)))

 (declare which-os (Unit -> String))
 (define (which-os)
   "Returns your system's Operating System."
   (lisp String ()
     (cl:subseq (cl:write-to-string (uiop:detect-os)) 1)))

 (declare which-hostname (Unit -> String))
 (define (which-hostname)
   "Returns your system's Hostname."
   (lisp String ()
     (uiop:hostname)))

 (declare which-implementation (Unit -> String))
 (define (which-implementation)
   "Returns your lisp implementation."
   (lisp String ()
     (cl:subseq (cl:write-to-string (uiop:implementation-type)) 1)))

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

 (declare which-features (Unit -> (List String)))
 (define (which-features)
   "Returns a list of active features, from `cl:*features*`."
   (lisp (list String) ()
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

;;;
;;; Working with Directories
;;;

(coalton-toplevel

 (declare directory-exists-p (String -> Boolean))
 (define (directory-exists-p dir)
   "Checks whether a directory exists"
   (lisp Boolean (dir)
         (cl:if (uiop:directory-exists-p dir)
                cl:t
                cl:nil)))

 (declare directory-files (String -> (List String)))
 (define (directory-files dir)
   "Returns all files within a directory."
   (lisp (List String) (dir)
         (uiop:directory-files dir)))

 (declare subdirectories (String -> (List String)))
 (define (subdirectories dir)
   "Returns all subdirectories in a given directory."
   (lisp (List String) (dir)
         (uiop:subdirectories dir)))

 (declare print-directory-contents (String -> String))
 (define (print-directory-contents dir)
   "Prints all files and subdirectories of a directory."
   (lisp String (dir)
         (cl:format cl:nil "~%Contents of Directory \"~a\":~%~%Subdirectories:~%~{~a~%~}Files:~%~{~a~%~}"
                    dir
                    (uiop:subdirectories dir)
                    (uiop:directory-files dir))))

 (declare system-relative-pathname (String -> String -> String))
 (define (system-relative-pathname system-name name)
   "Generates a system-relative-pathname for a given filename or path"
   (lisp String (system-name name)
         (cl:namestring (asdf:system-relative-pathname system-name name)))))

;;;
;;; Current working directory
;;;

(coalton-toplevel

 (declare cwd (Unit -> String))
 (define (cwd)
   "Returns the Current Working Directory"
   (lisp String ()
         (cl:namestring (uiop:getcwd))))

 (declare chdir (String -> Unit))
 (define (chdir dir)
   "Changes the current working directory"
   (lisp UFix (dir)
         (uiop:chdir dir))
   (traceobject "Current Working Directory" (cwd)))

 (declare cwd-relative-pathname (String -> String))
 (define (cwd-relative-pathname name)
   "Applies the full current working directory path to a file or path name."
   (str:concat (cwd) name))

 (declare cwd-files (Unit -> (List String)))
 (define (cwd-files)
   "Returns all files in the current working directory."
   (directory-files (cwd)))

 (declare cwd-subdirectories (Unit -> (List String)))
 (define (cwd-subdirectories)
   "Returns all subdirectories in the current working directory."
   (subdirectories (cwd)))

 (declare pwd (Unit -> String))
 (define (pwd)
   "Prints all contents of the current working directory."
   (print-directory-contents (cwd))))


;; Working with files
(coalton-toplevel

  (declare file-exists-p (String -> Boolean))
  (define (file-exists-p name)
    "Checks whether a file exists."
    (let file = (cwd-relative-pathname name))
    (lisp Boolean (file)
      (cl:if (cl:probe-file file)
             cl:t
             cl:nil)))

  (declare concat-files ((List String) -> String -> Unit))
  (define (concat-files inputs output)
    (let files = (map cwd-relative-pathname inputs))
    (let output-file = (cwd-relative-pathname output))
    (lisp Unit (files output-file)
      (uiop:concatenate-files files output-file))
    (traceobject "Files combined into" output-file))

  (declare copy-file (String -> String -> Unit))
  (define (copy-file input output)
    (let file = (cwd-relative-pathname input))
    (let output-file = (cwd-relative-pathname output))
    
    (lisp Unit (file output-file)
      (uiop:copy-file file output-file))
    (traceobject "File copied to" output-file))
  
  (declare file->string (String -> (Optional String)))
  (define (file->string filename)
    (let resolved-filename = (cwd-relative-pathname filename))
    (if (file-exists-p filename)
        (Some (lisp String (resolved-filename)
                (uiop:read-file-string resolved-filename)))
        None))

  (declare file->line (String -> UFix -> (Optional String)))
  (define (file->line filename line)
    (let resolved-filename = (cwd-relative-pathname filename))
    (if (file-exists-p filename)
        (Some (Lisp String (resolved-filename line)
                (uiop:read-file-line resolved-filename :at line)))
        None))

  (declare file->lines (String -> (List String)))
  (define (file->lines filename)
    "Reads a file into lines, "
    (let resolved-filename = (cwd-relative-pathname filename))
    (if (file-exists-p filename)
        (lisp (List String) (resolved-filename)
          (uiop:read-file-lines resolved-filename))
        Nil)))

;; currently unused, but I think it may prove useful- depends on Splittable PR
#+ignore(coalton-toplevel

          (define-struct FilePath
            (dir-chain (List String))
            (filename String)
            (filetype String))

          (declare filepath->string (FilePath -> String))
          (define (filepath->string (FilePath dirs name type))
            "Converts a FilePath object to a string."
            (lisp String (dirs name type)
              (cl:format nil "~{~a/~}~a.~a" dirs name type)))

          (declare string->filepath (String -> FilePath))
          (define (string->filepath str)
            "Converts a string file path to a FilePath object."
            (let ((undotted (str:split #\. str))
                  (unslashed (list:reverse (str:split #\/ (list:car undotted))))
                  (dirs (list:cdr unslashed))
                  (name (list:car unslashed))
                  (type (unwrap (list:last undotted))))
              (FilePath dirs name type)))

          ;; filetype, filename, directores

          (declare parent-directories (FilePath -> (List String)))
          (define (parent-directories (FilePath dirs _ _))
            dirs)

          (declare file-type (FilePath -> String))
          (define (file-type (FilePath _ _ filetype))
            filetype)

          (declare file-name (FilePath -> String))
          (define (file-name (FilePath _ filename _))
            filename))


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/IO")
