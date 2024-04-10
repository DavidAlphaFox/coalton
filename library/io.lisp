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

   ;; ;; FilePath type
   ;; #:FilePath
   ;; #:dir-chain
   ;; #:filename
   ;; #:filetype
   ;; #:filepath->string
   ;; #:string->filepath
   ;; #:parent-directories
   ;; #:file-type
   ;; #:file-name

   ;; directory functions
   #:directory-exists-p
   #:directory-files
   #:subdirectories
   #:print-directory-contents
   #:system-relative-pathname

   ;; current working directory
   #:cwd
   #:chdir
   #:cwd-relative-pathname
   #:cwd-files
   #:cwd-subdirectories
   #:pwd

   ;; file functions
   #:file-exists-p
   #:concat-files
   #:copy-file
   #:file->string
   #:file->line
   #:file->lines

   ;; Streams
   #:Stream
   #:BroadcastStream
   #:ConcatenatedStream
   #:EchoStream
   #:TwoWayStream
   #:StringStream
   #:FileStream
   #:DirectionOption
   #:IfExistsOption
   #:IfDoesNotExistOption
   #:open
   #:close
   #:load
   ))

(in-package #:coalton-library/io)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

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
      (cl:let ((subdirs (uiop:subdirectories dir))
               (dirfiles (uiop:directory-files dir)))
        (cl:if (cl:and (cl:not subdirs)
                       (cl:not dirfiles))
               (cl:format cl:nil "Directory ~a is empty" dir)
               (cl:format cl:nil "~%Contents of Directory ~a:~%~%~a~a"
                          dir
                          (cl:if subdirs
                                 (cl:format cl:nil "Subdirectories:~%~{~a~%~}~%" subdirs)
                                 "")
                          (cl:if dirfiles
                                 (cl:format cl:nil "Files:~%~{~a~%~}" dirfiles)
                                 ""))))))

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

  (declare back-up (Unit -> Unit))
  (define (back-up)
    "Returns the current working directory one level up."
    (chdir ".."))

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
    (print-directory-contents (cwd)))

  (declare system-relative-cwd (String -> Unit))
  (define (system-relative-cwd system-name)
    "Sets the current working directory to the given asdf system's directory."
    (chdir (system-relative-pathname system-name ""))))

;;;
;;; Basic file operations
;;;

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
    "Concatenates two files into a target file."
    (let files = (map cwd-relative-pathname inputs))
    (let output-file = (cwd-relative-pathname output))
    (lisp Unit (files output-file)
      (uiop:concatenate-files files output-file))
    (traceobject "Files combined into" output-file))

  (declare copy-file (String -> String -> Unit))
  (define (copy-file input output)
    "Copies a file to a target file."
    (let file = (cwd-relative-pathname input))
    (let output-file = (cwd-relative-pathname output))
    (lisp Boolean (file output-file)
      (uiop:copy-file file output-file))
    (traceobject "File copied to" output-file))

  (declare delete-file (String -> Unit))
  (define (delete-file file)
    "Deletes a given file, comments on whether the file exists."
    (let fl = (cwd-relative-pathname file))
    (if (lisp Boolean (fl)
          (uiop:delete-file-if-exists fl))
        (traceobject "File deleted" file)
        (traceobject "File not found" file)))
  
  (declare file->string (String -> (Optional String)))
  (define (file->string filename)
    "Reads a file into a string."
    (let resolved-filename = (cwd-relative-pathname filename))
    (if (file-exists-p filename)
        (Some (lisp String (resolved-filename)
                (uiop:read-file-string resolved-filename)))
        None))

  (declare file->line (String -> UFix -> (Optional String)))
  (define (file->line filename line)
    "Reads the nth line of a file."
    (let resolved-filename = (cwd-relative-pathname filename))
    (if (file-exists-p filename)
        (Some (Lisp String (resolved-filename line)
                (uiop:read-file-line resolved-filename :at line)))
        None))

  (declare file->lines (String -> (List String)))
  (define (file->lines filename)
    "Reads a file into lines."
    (let resolved-filename = (cwd-relative-pathname filename))
    (if (file-exists-p filename)
        (lisp (List String) (resolved-filename)
          (uiop:read-file-lines resolved-filename))
        Nil)))

;;;
;;; Streams
;;;

(coalton-toplevel

  (repr :native cl:stream)
  (define-type Stream
    "A stream represented by a Common Lisp stream.")

  (repr :native cl:broadcast-stream)
  (define-type BroadcastStream
    "A `BroadcastStream` is an output stream which passes output to multiple defined streams.")

  (repr :native cl:concatenated-stream)
  (define-type ConcatenatedStream
    "A `ConcatenatedStream` is a composite input stream comprised of zero or more `Stream`s, which are read in order.")

  (repr :native cl:echo-stream)
  (define-type EchoStream)
  
  (repr :native cl:two-way-stream)
  (define-type TwoWayStream
    "A stream for both input and output.")

  (repr :native cl:string-stream)
  (define-type StringStream
    "A String Stream."))

;;;
;;; File streams
;;;

(coalton-toplevel

  (repr :native cl:file-stream)
  (define-type FileStream))

;; defining acceptable keyword options

(cl:defun direction-option-p (x)
  (cl:member x '(:input :output :io :probe)))

(cl:deftype direction-option ()
  `(cl:satisfies direction-option-p))

(cl:defun if-exists-option-p (x)
  (cl:member x '(:error :new-version :rename :rename-and-delete :overwrite :append :supersede Nil)))

(cl:deftype if-exists-option ()
  `(cl:satisfies if-exists-option-p))

(cl:defun if-does-not-exist-option-p (x)
  (cl:member x '(:error :create Nil)))

(cl:deftype if-does-not-exist-option ()
  `(cl:satisfies if-does-not-exist-option-p))

(coalton-toplevel

  (repr :native direction-option)
  (define-type DirectionOption)

  (repr :native if-exists-option)
  (define-type IfExistsOption)

  (repr :native if-does-not-exist-option)
  (define-type IfDoesNotExistOption)

  (declare open (String -> DirectionOption -> IfExistsOption -> IfDoesNotExistOption -> FileStream))
  (define (open file direction if-exists if-does-not-exist)
    "Opens a filestream given a file."
    (lisp FileStream (file direction if-exists if-does-not-exist)
      (cl:open file :direction direction :if-exists if-exists :if-does-not-exist if-does-not-exist)))

  (declare close (Stream -> Unit))
  (define (close stream)
    "Closes a stream."
    (lisp Unit (stream)
      (cl:close stream)))

  (declare load (String -> Boolean))
  (define (load file)
    "Loads a lisp file."
    (lisp Boolean (file)
      (cl:load file))))

;; synonym streams

(coalton-toplevel

  (repr :native cl:synonym-stream)
  (define-type SynonymStream)

  (define (make-synonym-stream stream)
    (lisp Stream (stream)
      (cl:make-synonym-stream stream))))

#+ignore(coalton-toplevel

  #+ignore(define (write-to-file file if-exists if-does-not-exist data)
    (lisp Unit (file if-exists if-does-not-exist data)
      (cl:with-open-file (stream file
                                 :direction :output
                                 :if-exists if-exists
                                 :if-does-not-exist if-does-not-exist)
        (cl:write-sequence data stream) ))
    (traceobject "Data written to file" file)))

;; file open, file close, file read, file write

;; 



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
