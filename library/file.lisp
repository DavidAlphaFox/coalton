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
   #:Pathname
   #:merge
   #:exists?

   #:create-directory
   #:delete-directory
   #:directory-files
   #:subdirectories
   #:print-directory-contents
   #:system-relative-pathname

   ;; file functions
   #:concat
   #:copy
   #:delete
   #:file->string
   #:file->line
   #:file->lines
   #:cat

   #:DirectionOption
   #:InputFile
   #:OutputFile
   #:IfExistsOption
   #:ExistsError
   #:AppendFile
   #:SupersedeFile
   #:IfDoesNotExistOption
   #:DoesNotExistError
   #:CreateFile
   #:match-direction-option
   #:match-if-exists-option
   #:match-if-does-not-exist-option
   
   #:FileStream
   
   #:open
   #:write
   #:write-byte
   #:read
   #:close
   #:load
   #:write-to-file))

(in-package #:coalton-library/file)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; Pathnames
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
      (cl:merge-pathnames path1 path2)))

  (declare exists? (Pathname -> Boolean))
  (define (exists? path)
    "Checks whether a file or directory exists."
    (lisp Boolean (path)
      (cl:if (cl:probe-file path)
             cl:t
             cl:nil))))

;;;
;;; Working with directories
;;;

(coalton-toplevel

  (declare create-directory (Pathname -> Unit))
  (define (create-directory dir)
    "Creates a directory if it doesn't exist."
    (if (exists? dir)
        (trace "Directory already exists")
        (traceobject "Directory created"
                     (lisp Pathname (dir)
                       (cl:ensure-directories-exist dir)))))

  (declare delete-directory (Pathname -> Unit))
  (define (delete-directory dir)
    "Deletes a target directory."
    (if (exists? dir)
        (traceobject "Directory deleted"
                     (lisp Pathname (dir)
                       (uiop:delete-directory-tree dir :validate cl:t)))
        (trace "Directory does not exist.")))

  (declare directory-files (Pathname -> (List Pathname)))
  (define (directory-files dir)
    "Returns all files within a directory."
    (lisp (List Pathname) (dir)
      (uiop:directory-files dir)))

  (declare subdirectories (Pathname -> (List Pathname)))
  (define (subdirectories dir)
    "Returns all subdirectories in a given directory."
    (lisp (List Pathname) (dir)
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
;;; Basic File Operations
;;;

(coalton-toplevel

  (declare concat ((List Pathname) -> Pathname -> Unit))
  (define (concat inputs output)
    "Concatenates two files into a target file."
    (lisp Unit (inputs output)
      (uiop:concatenate-files inputs output))
    (traceobject "Files combined into" output))

  (declare copy (Pathname -> Pathname -> Unit))
  (define (copy input output)
    "Copies a file to a target file."
    (lisp Boolean (input output)
      (uiop:copy-file input output))
    (traceobject "File copied to" output))

  (declare delete (Pathname -> Unit))
  (define (delete file)
    "Deletes a given file, comments on whether the file exists."
    (if (lisp Boolean (file)
          (uiop:delete-file-if-exists file))
        (traceobject "File deleted" file)
        (traceobject "File not found" file)))
  
  (declare file->string (Pathname -> (Optional String)))
  (define (file->string filespec)
    "Reads a file into a string."
    (if (exists? filespec)
        (Some (lisp String (filespec)
                (uiop:read-file-string filespec)))
        None))

  (declare file->line (Pathname -> UFix -> (Optional String)))
  (define (file->line filespec n)
    "Reads the nth line of a file."
    (if (exists? filespec)
        (Some (Lisp String (filespec n)
                (uiop:read-file-line filespec :at n)))
        None))

  (declare file->lines (Pathname -> (List String)))
  (define (file->lines filespec)
    "Reads a file into lines."
    (if (exists? filespec)
        (lisp (List String) (filespec)
          (uiop:read-file-lines filespec))
        Nil))

  (declare cat (Pathname -> Unit))
  (define (cat filespec)
    "Traces the contents of a given file."
    (match (file->string filespec)
      ((Some s)
       (traceobject "File contents" s))
      ((None) (trace "File empty.")))))


(coalton-toplevel

  (repr :enum)
  (Define-type DirectionOption
    InputFile
    OutputFile)
  
  (repr :enum)
  (define-type IfExistsOption
    ExistsError
    AppendFile
    SupersedeFile)

  (repr :enum)
  (define-type IfDoesNotExistOption
    DoesNotExistError
    CreateFile))

(cl:defvar *direction-options*
  '((DirectionOption/InputFile :input)
    (DirectionOption/OutputFile :output)))

(cl:defun match-direction-option (x)
  (cl:second (cl:assoc x *direction-options*)))

(cl:defvar *if-exists-options*
  '((IfExistsOption/ExistsError :error)
    (IfExistsOption/AppendFile :append)
    (IfExistsOption/SupersedeFile :supersede)))

(cl:defun match-if-exists-option (x)
  (cl:second (cl:assoc x *if-exists-options*)))

(cl:defvar *if-does-not-exist-options*
  '((IfDoesNotExistOption/DoesNotExistError :error)
    (IfDoesNotExistOption/CreateFile :create)))

(cl:defun match-if-does-not-exist-option (x)
  (cl:second (cl:assoc x *if-does-not-exist-options*)))

;;;
;;; File streams
;;;

(coalton-toplevel

  (repr :native cl:file-stream)
  (define-type FileStream))


(coalton-toplevel                       

  (declare open (Pathname -> DirectionOption -> IfExistsOption -> IfDoesNotExistOption -> FileStream))
  (define (open filespec direction if-exists if-does-not-exist)
    "Opens a filestream given a file."
    (lisp FileStream (filespec direction if-exists if-does-not-exist)
      (cl:let ((direct (match-direction-option direction))
               (exists (match-if-exists-option if-exists))
               (doesnt-exist (match-if-does-not-exist-option if-does-not-exist)))
        (cl:open filespec :direction direct :if-exists exists :if-does-not-exist doesnt-exist))))

  (declare write (String -> FileStream -> String))
  (define (write data stream)
    "Writes a string to the stream."
    (lisp String (data stream)
      (cl:write-sequence data stream)))

  (declare write-byte (Ufix -> FileStream -> Unit))
  (define (write-byte x stream)
    "Writes a single byte to a stream."
    (lisp UFix (x stream)
      (cl:write-byte x stream))
    Unit)
  
  (declare write-char (Char -> FileStream -> Unit))
  (define (write-char c stream)
    "Writes a single character to a stream."
    (lisp Char (c stream)
      (cl:write-char c stream))
    Unit)
  
  (declare read (FileStream -> String))
  (define (read stream)
    (lisp String (stream)
      (cl:read stream)))
  
  (declare close (FileStream -> Unit))
  (define (close stream)
    "Closes a stream."
    (lisp Boolean (stream)
      (cl:close stream))
    Unit)

  (declare load (Pathname -> Boolean))
  (define (load filespec)
    "Loads a lisp file."
    (lisp Boolean (filespec)
      (cl:load filespec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; there are two options for write-to-file, one using with-open-file and one using native coalton code.
  ;; We'll pick one in the review process
  ;;

  (declare write-to-file (Pathname -> IFExistsOption -> IfDoesNotExistOption -> String -> Unit))
  #+ignore(define (write-to-file filepath if-exists if-does-not-exist data)
    "Writes data to a given file, with if-exists options `ExistsError` `Append` and `Supersede`, and if-does-not-exist options `DoesNotExistError` and `Create`."
    (lisp Boolean (filepath if-exists if-does-not-exist data)
        (cl:let ((exists (match-if-exists-option if-exists))
                 (doesnt (match-if-does-not-exist-option if-does-not-exist)))
          (cl:with-open-file (stream filepath :direction :output :if-exists exists :if-does-not-exist doesnt)
            (cl:write-sequence data stream)))
        cl:t)
    (traceobject "Data written to" filepath))

  ;; just coalton
  (define (write-to-file filename if-exists if-does-not-exist data)
    "Writes data to a given file, with if-exists options `ExistsError` `Append` and `Supersede`, and if-does-not-exist options `DoesNotExistError` and `Create`."
    (let ((Stream (open filename OutputFile if-exists if-does-not-exist)))
      (write data stream)
      (close stream))
    (traceobject "Data written to" filename)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/FILE")
