(coalton-library/utils:defstdlib-package #:coalton-library/file
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
   #:load))

(in-package #:coalton-library/file)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; Working with directories
;;;

(coalton-toplevel

  (repr :native cl:pathname)
  (define-type Pathname)

  (define-instance (Into String Pathname)
    (define (into s)
      (lisp Pathname (s)
        (cl:pathname s))))

  (define-instance (Into Pathname String)
    (define (into p)
      (lisp String (p)
        (cl:namestring p))))

  (declare merge (Pathname -> Pathname -> Pathname))
  (define (merge path1 path2)
      "Merges two pathnames together."
      (lisp Pathname (path1 path2)
            (cl:merge-pathnames path1 path2))))

(coalton-toplevel

  (declare directory-exists? (Pathname -> Boolean))
  (define (directory-exists? dir)
    "Checks whether a directory exists."
    (lisp Boolean (dir)
      (cl:if (uiop:directory-exists-p dir)
             cl:t
             cl:nil)))

  (declare directory-files (Pathname -> (List String)))
  (define (directory-files dir)
    "Returns all files within a directory."
    (lisp (List String) (dir)
      (uiop:directory-files dir)))

  (declare subdirectories (Pathname -> (List String)))
  (define (subdirectories dir)
    "Returns all subdirectories in a given directory."
    (lisp (List String) (dir)
      (uiop:subdirectories dir)))

  (declare print-directory-contents (Pathname -> Unit))
  (define (print-directory-contents dir)
    "Prints all files and subdirectories of a directory."
    (trace (lisp String (dir)
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
                                        "")))))))

  (declare system-relative-pathname (String -> String -> Pathname))
  (define (system-relative-pathname system-name name)
    "Generates a system-relative-pathname for a given filename or path"
    (lisp Pathname (system-name name)
      (asdf:system-relative-pathname system-name name))))

;;;
;;; Current working directory
;;;

;; autocurrying -reader monad

;; maybe to move to shell or nav
#+ignore(coalton-toplevel

  (declare cwd (Unit -> String))
  (define (cwd)
    "Returns the current working directory."
    (lisp String ()
      (cl:namestring (uiop:getcwd))))

  (declare pwd (Unit -> String))
  (define (pwd)
    "Returns the current working directory."
    (cwd))

  (declare cd (String -> Unit))
  (define (cd dir)
    "Changes the current working directory"
    (lisp UFix (dir)
      (uiop:chdir dir))
    (traceobject "Current Working Directory" (cwd)))

  (declare back (Unit -> Unit))
  (define (back)
    "Returns the current working directory one level up."
    (cd ".."))

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

  (declare ls (Unit -> String))
  (define (ls)
    "Prints all contents of the current working directory."
    (print-directory-contents (cwd)))

  (declare system-relative-cwd (String -> Unit))
  (define (system-relative-cwd system-name)
    "Sets the current working directory to the given asdf system's directory."
    (cd (system-relative-pathname system-name ""))))

;;;
;;; Basic file operations
;;;

(coalton-toplevel

  (declare file-exists? (Pathname -> Boolean))
  (define (file-exists? path)
    "Checks whether a file exists."
    (lisp Boolean (path)
      (cl:if (cl:probe-file path)
             cl:t
             cl:nil)))

  (declare concat-files ((List Pathname) -> Pathname -> Unit))
  (define (concat-files inputs output)
    "Concatenates two files into a target file."
    (lisp Unit (inputs output)
      (uiop:concatenate-files inputs output))
    (traceobject "Files combined into" output))

  (declare copy-file (Pathname -> Pathname -> Unit))
  (define (copy-file input output)
    "Copies a file to a target file."
    (lisp Boolean (input output)
      (uiop:copy-file input output))
    (traceobject "File copied to" output))

  (declare delete-file (Pathname -> Unit))
  (define (delete-file file)
    "Deletes a given file, comments on whether the file exists."
    (if (lisp Boolean (file)
          (uiop:delete-file-if-exists file))
        (traceobject "File deleted" file)
        (traceobject "File not found" file)))
  
  (declare file->string (Pathname -> (Optional String)))
  (define (file->string filespec)
    "Reads a file into a string."
    ;(let resolved-filename = (cwd-relative-pathname filename))
    (if (file-exists? filespec)
        (Some (lisp String (filespec)
                (uiop:read-file-string filespec)))
        None))

  (declare cat (Pathname -> Unit))
  (define (cat filespec)
    "Traces the contents of a given file."
    (match (file->string filespec)
      ((Some s)
       (traceobject "File contents" s))
      ((None) (trace "File empty."))))

  (declare file->line (Pathname -> UFix -> (Optional String)))
  (define (file->line filespec n)
    "Reads the nth line of a file."
    (if (file-exists? filespec)
        (Some (Lisp String (filespec n)
                (uiop:read-file-line filespec :at n)))
        None))

  (declare file->lines (Pathname -> (List String)))
  (define (file->lines filespec)
    "Reads a file into lines."
    (if (file-exists? filespec)
        (lisp (List String) (filespec)
          (uiop:read-file-lines filespec))
        Nil)))

;;;
;;; Streams
;;;

;; defining acceptable keyword options

(coalton-toplevel

  (repr :enum)
  (Define-type DirectionOption
    Input
    Output)
  
  (repr :enum)
  (define-type IfExistsOption
    ExistsError
    Append
    Supersede)

  (repr :enum)
  (define-type IfDoesNotExistOption
    DoesNotExistError
    Create))

(cl:defvar *if-exists-options*
  '((IfExistsOption/ExistsError :error)
    (IfExistsOption/Append :append)
    (IfExistsOption/Supersede :supersede)))

(cl:defun match-if-exists-option (x)
  (cl:second (cl:assoc x *if-exists-options*)))

(cl:defvar *if-does-not-exist-options*
  '((IfExistsOption/DoesNotExistError :error)
    (IfExistsOption/Append :create)))

(cl:defun match-if-does-not-exist-option (x)
  (cl:second (cl:assoc x *if-does-not-exist-options*)))

(cl:defvar *direction-options*
  '((DirectionOption/Input :input)
    (DirectionOption/Output :output)))

(cl:defun match-direction-option (x)
  (cl:second (cl:assoc x *direction-options*)))


;;;
;;; File streams
;;;

(coalton-toplevel

  (repr :native cl:stream)
  (define-type Stream
    "A stream represented by a Common Lisp stream.")
  
  (repr :native cl:file-stream)
  (define-type FileStream))


#+ignore(coalton-toplevel                       

  ;; open, close, write, read should eventually be a typeclass over different stream types
  
  (declare open (String -> DirectionOption -> IfExistsOption -> IfDoesNotExistOption -> FileStream))
  (define (open file direction if-exists if-does-not-exist)
    "Opens a filestream given a file."
    (lisp FileStream (file direction if-exists if-does-not-exist)
      (cl:let ((file (cwd-relative-pathname file))
               (direct (match-direction-option direction))
               (exists (match-if-exists-option if-exists))
               (doesnt-exist (match-if-does-not-exist-option if-does-not-exist)))
        (cl:open file :direction direct :if-exists exists :if-does-not-exist doesnt-exist))))

  (declare write (String -> FileStream -> String))
  (define (write data stream)
    "Writes a string to the stream."
    (lisp String (data stream)
      (cl:write-sequence data stream)))

  (declare write-byte (Integer -> FileStream -> Integer))
  (define (write-byte x stream)
    (lisp Integer (x stream)
      (cl:write-byte x stream)))
  
  (declare read (FileStream -> String))
  (define (read stream)
    (lisp String (stream)
      (cl:read stream)))
  
  (declare close (FileStream -> Unit))
  (define (close stream)
    "Closes a stream."
    (lisp Boolean (stream)
      (cl:close stream))
    (traceobject "Stream closed" stream))

  (declare load (String -> Boolean))
  (define (load filename)
    "Loads a lisp file."
    (let ((file (cwd-relative-pathname filename)))
      (lisp Boolean (file)
        (cl:load file))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; there are two options for write-to-file, one using with-open-file and one using native coalton code.
  ;; We'll pick one in the review process
  ;;

  (declare write-to-file (String -> IFExistsOption -> IfDoesNotExistOption -> String -> Unit))
  (define (write-to-file filename if-exists if-does-not-exist data)
    "Writes data to a given file, with if-exists options `ExistsError` `Append` and `Supersede`, and if-does-not-exist options `DoesNotExistError` and `Create`."
    (let ((file (cwd-relative-pathname filename)))
      (lisp Boolean (file if-exists if-does-not-exist data)
        (cl:let ((exists (match-if-exists-option if-exists))
                 (doesnt (match-if-does-not-exist-option if-does-not-exist)))
          (cl:with-open-file (stream file :direction :output :if-exists exists :if-does-not-exist doesnt)
            (cl:write-sequence data stream)))
        cl:t))
    (traceobject "Data written to" filename))

  ;; just coalton
  (define (write-to-file2 filename if-exists if-does-not-exist data)
    (let ((Stream (open filename Output if-exists if-does-not-exist)))
      (write data stream)
      (close stream))
    (traceobject "Data written to" filename)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Other stream types that might be useful
;;;


(coalton-toplevel
  (repr :native cl:broadcast-stream)
  (define-type DualStream
    "A `BroadcastStream` is an output stream which passes output to multiple defined streams.")

  (define (make-dualstream stream-a stream-b)
    "Makes an output stream which outputs to both stream-a and stream-b."
    (lisp DualStream (stream-a stream-b)
      (cl:make-broadcast-stream stream-a stream-b)))
  )

(coalton-toplevel

  
  (repr :native cl:concatenated-stream)
  (define-type ConcatenatedStream
    "A `ConcatenatedStream` is a composite input stream comprised of zero or more `Stream`s, which are read in order.")

  (repr :native cl:string-stream)
  (define-type StringStream
    "A String Stream."))








;; currently unused, but I think it would be useful for seaching by type, etc. - depends on Splittable PR
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
(sb-ext:lock-package "COALTON-LIBRARY/FILE")
