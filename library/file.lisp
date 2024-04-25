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
                    (#:vec #:coalton-library/vector)
                    (#:res #:coalton-library/result))
  (:export
   #:FileError
   #:ReadError
   #:WriteError

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
  (define-type Pathname
    "Pathname object. Uses `cl:pathname`")

  (define-instance (TryInto String Pathname)
    (define (tryinto s)
      (lisp (Result String Pathname) (s)
        (cl:if (cl:pathname s)
               (Ok (cl:pathname s))
               (Err "Invalid pathname string.")))))

  (define-instance (Into Pathname String)
    (define (into p)
      (lisp String (p)
        (cl:namestring p))))

  (declare merge (Pathname -> Pathname -> Pathname))
  (define (merge path1 path2)
    "Merges two pathnames together. The order is important- for adding subdirectories, make sure to put the subpath (the one without the leading `/`) in `path`."
    (lisp Pathname (path1 path2)
      (cl:merge-pathnames path1 path2))))

;;;
;;; File Error handler
;;;

(coalton-toplevel

  (define-type FileError
    (PathError String Pathname)
    (FileError String String)
    (ASDFError String String)
    (ReadError String String)
    (WriteError String String)))


;;;
;;; Handling existential path queries
;;;

(coalton-toplevel

  (declare exists? (Pathname -> Boolean))
  (define (exists? path)
    "Returns whether a file or directory exists."
    (lisp Boolean (path)
      (to-boolean (cl:probe-file path))))

  (declare if-exists (Pathname -> :a -> (Result FileError :a)))
  (define (if-exists path action)
    "Performs an action if the path exists, else returns an error."
    (if (exists? path)
        (Ok action)
        (Err (PathError "This path does not represent an existing file or directory:" path))))

  (declare if-does-not-exist (Pathname -> :a -> (Result FileError :a)))
  (define (if-does-not-exist path action)
    "Performs an action if the path does not exist, else returns an error."
    (if (exists? path)
        (Err (PathError "This file or directory already exists:" path))
        (Ok action)))

  ;;
  ;; Handling quanderies of path identities
  ;;
  
  (declare if-directory (Pathname -> :a -> (Result FileError :a)))
  (define (if-directory path action)
    "Performs an action only if the path represents a directory."
    (lisp (Result FileError :a) (path action)
      (cl:if (uiop:file-pathname-p path)
             (Err (PathError "Pathname does not represent a directory. Please amend with a trailing `/`." path))
             (Ok action))))

  (declare if-file (Pathname -> :a -> (Result FileError :a)))
  (define (if-file path action)
    "Performs an action only if the path represents a directory."
    (lisp (Result FileError :a) (path action)
      (cl:if (uiop:file-pathname-p path)             
             (Ok action)
             (Err (PathError "Pathname represents a directory. Please remove the trailing `/` or add a valid filename." path))))))

;;;
;;; Working with directories
;;;

(coalton-toplevel
  
  (declare mkdir (Pathname -> (Result FileError Pathname)))
  (define (mkdir path)
    "Makes a directory or chain of directories if they don't already exist. The pathname must end with a trailing `/`."
    (if-directory path (lisp Pathname (path)
                        (cl:ensure-directories-exist path))))

  (declare ensure-directories (Pathname -> Unit))
  (define (ensure-directories path)
    "Ensures that a directory or chain of directories exists. The pathname can end with either a file or directory; ending with a file will ensure all directories above that file, ending with a directory (with a trailing '/') will ensure all directories above and including that directory."
    (lisp Unit (path)
      (cl:ensure-directories-exist path)
      Unit))

  (declare directory-files (Pathname -> (Result FileError (List Pathname))))
  (define (directory-files path)
    "Returns all files within the directory. Returns an error if the pathname does not represent a directory."
    (if-directory path (lisp (List Pathname) (path)
                        (uiop:directory-files path))))

  (declare subdirectories (Pathname -> (Result FileError (List Pathname))))
  (define (subdirectories path)
    "Returns all subdirectories within the directory. Returns an error if the pathname does not represent a directory."
    (if-directory path (lisp (List Pathname) (path)
                        (uiop:subdirectories path))))

  ;;
  ;; Handling directory behavior that depends on emptiness
  ;;
  
  (declare empty? (Pathname -> (Result FileError Boolean)))
  (define (empty? path)
    "Checks whether a directory is empty."
    (if-directory path
                  (and (list:null? (unwrap (directory-files path)))
                       (list:null? (unwrap (subdirectories path))))))

  (declare if-empty (Pathname -> :a -> (Result FileError :a)))
  (define (if-empty path action)
    "Performs an action only if the directory is empty."
    (match (empty? path)
      ((Ok mt)
       (if mt
           (Ok action)
           (Err (PathError "Directory is not empty." path))))
      ((Err x)
       (Err x))))

  (declare if-not-empty (Pathname -> :a -> (Result FileError :a)))
  (define (if-not-empty path action)
    "Performs an action only if the directory is not empty."
    (match (empty? path)
      ((Ok mt)
       (if mt
           (Err (PathError "Directory is not empty." path))
           (Ok action)))
      ((Err x)
       (Err x))))

  
  (declare rmdir (Pathname -> (Result FileError Unit)))
  (define (rmdir path)
    "Deletes a directory `dir` if it's empty, otherwise, returns`FileError`."
    (if-empty path (lisp Unit (path)
                    (uiop:delete-empty-directory path)
                    Unit)))
  
  (declare rmdir-unsafe (Pathname -> (Result FileError Unit)))
  (define (rmdir-unsafe path)
    "Deletes a target directory recursively. Equivalent to `rm -rf`. Errors if the path is not a directory."
    (if-directory path (lisp Unit (path)
                        (uiop:delete-directory-tree path :validate cl:t)
                        Unit)))
  
  (declare system-relative-pathname (String -> String -> (Result FileError Pathname)))
  (define (system-relative-pathname system-name name)
    "Generates a system-relative-pathname for a given filename or path."
    (lisp (Result FileError Pathname) (system-name name)
      (cl:handler-case (Ok (asdf:system-relative-pathname system-name name))
        (cl:error () (Err (ASDFError "System not found" system-name)))))))

;;;
;;; Basic File Operations
;;;

(coalton-toplevel

  (declare copy (Pathname -> Pathname -> (Result FileError (Result FileError Unit))))
  (define (copy input output)
    "Copies a file to a target file."
    (if-does-not-exist
     output
     (if-file
      input
      (lisp Unit (input output)
        (uiop:copy-file input output)
        Unit))))

  (declare delete (Pathname -> (Result FileError Unit)))
  (define (delete path)
    "Deletes a given file, comments on whether the file exists."
    (if-file path
             (lisp Unit (path)
               (uiop:delete-file-if-exists path))))
  
  (declare read-file-string (Pathname -> (Result FileError String)))
  (define (read-file-string path)
    "Reads a file into a string."
    (lisp (Result FileError String) (path)
      (cl:handler-case (Ok (uiop:read-file-string path))
        (cl:error () (Err (PathError "File does not exist." path))))))

  (declare read-file-lines (Pathname -> (Result FileError (List String))))
  (define (read-file-lines path)
    "Reads a file into lines."
    (lisp (Result FileError (List String)) (path)
      (cl:handler-case (Ok (uiop:read-file-lines path))
        (cl:error () (Err (PathError "File does not exist." path))))))

  (declare cat (Pathname -> Unit))
  (define (cat path)
    "Traces the contents of a given file to the repl."
    (traceobject "File Contents" (read-file-string path))))

;;;
;;; FileStreams, options, and opening
;;;

(coalton-toplevel

  (repr :native cl:file-stream)
  (define-type FStream)

  (define-type FileStream
    "Represents a lisp file-stream or collection of file-streams."
    (InputStream FStream)
    (OutputStream FStream)
    (TwoWayStream FStream FStream)
    (BroadcastStream (List FStream)))

  ;;
  ;; Stream Options
  ;;
  
  (repr :enum)
  (define-type IfExists
    "Possible options for opening a stream when the file exists."
    ExistsError
    NewVersion
    Rename
    RenameAndDelete
    Overwrite
    Append
    Supersede)

  (repr :enum)
  (define-type IfDoesNotExist
    "Possible options for opening a stream when the file does not exist."
    DNExistError
    Create)

  (repr :enum)
  (define-type ElementType
    Int
    SFix
    S64
    S32
    S16
    S8
    USFix
    US64
    US32
    US16
    US8
    Character)
  
  (define-type StreamOptions
    (Input Pathname ElementType)
    (Output Pathname IfExists IfDoesNotExist ElementType)
    (TwoWay Pathname IfExists IfDoesNotExist ElementType)
    (Broadcast (list Pathname) IfExists IfDoesNotExist ElementType))

  ;;
  ;;
  ;;
  
  (declare %open-input (Pathname -> ElementType -> (Result FileError FStream)))
  (define (%open-input path etype)
    (lisp (Result FileError FStream) (path etype)
      (cl:handler-case (Ok (cl:open path
                                    :direction :input
                                    :element-type (cl:case etype
                                                    (ElementType/Int       'cl:integer)
                                                    (ElementType/SFix      'cl:fixnum)
                                                    (ElementType/S64       '(cl:signed-byte 64))
                                                    (ElementType/S32       '(cl:signed-byte 32))
                                                    (ElementType/S16       '(cl:signed-byte 16))
                                                    (ElementType/S8        '(cl:signed-byte 8))
                                                    (ElementType/USFix     '(cl:and cl:fixnum cl:unsigned-byte))
                                                    (ElementType/US64      '(cl:unsigned-byte 64))
                                                    (ElementType/US32      '(cl:unsigned-byte 32))
                                                    (ElementType/US16      '(cl:unsigned-byte 16))
                                                    (ElementType/US8       '(cl:unsigned-byte 8))
                                                    (ElementType/Character 'cl:character))))
        (cl:error () (Err (PathError "Error opening input stream with path." path))))))

  

  (declare %open-output (Pathname -> IfExists -> IfDoesNotExist -> ElementType -> (Result FileError FStream)))
  (define (%open-output path if-exists if-does-not-exist etype)
    (lisp (Result FileError FStream) (path if-exists if-does-not-exist etype)
      (cl:handler-case (Ok (cl:open path
                                    :direction :output
                                    :element-type (cl:case etype
                                                    (ElementType/Int      'cl:integer)
                                                    (ElementType/SFix     'cl:fixnum)
                                                    (ElementType/S64      '(cl:signed-byte 64))
                                                    (ElementType/S32      '(cl:signed-byte 32))
                                                    (ElementType/S16      '(cl:signed-byte 16))
                                                    (ElementType/S8       '(cl:signed-byte 8))
                                                    (ElementType/USFix     '(cl:and cl:fixnum cl:unsigned-byte))
                                                    (ElementType/US64     '(cl:unsigned-byte 64))
                                                    (ElementType/US32     '(cl:unsigned-byte 32))
                                                    (ElementType/US16     '(cl:unsigned-byte 16))
                                                    (ElementType/US8      '(cl:unsigned-byte 8))
                                                    (ElementType/Character 'cl:character))
                                    :if-exists (cl:case if-exists
                                                 (IfExists/ExistsError ':error)
                                                 (IfExists/NewVersion ':new-version)
                                                 (IfExists/Rename ':rename)
                                                 (IfExists/RenameAndDelete ':rename-and-delete)
                                                 (IfExists/Overwrite ':overwrite)
                                                 (IfExists/Append ':append)
                                                 (IfExists/Supersede ':supersede))
                                    :if-does-not-exist (cl:case if-does-not-exist
                                                         (IfDoesNotExist/DNExistError ':error)
                                                         (IfDoesNotExist/Create ':create))))
        (cl:error () (Err (PathError "Error opening output stream with path." path))))))

  (declare %open-twoway (Pathname -> IFExists -> IfDoesNotExist -> ElementType -> (Result FileError FileStream)))
  (define (%open-twoway path if-exists if-does-not-exist etype)
    (match (Tuple (%open-input path etype) (%open-output path if-exists if-does-not-exist etype))
      ((Tuple (Ok a) (Ok b))
       (Ok (TwoWayStream a b)))
      ((Tuple (Ok _) (Err r))
       (Err r))
      ((Tuple (Err r) (Ok _))
       (Err r))
      (_
       (Err (PathError "Setting up two-way stream failed in both directions" path)))))

  (declare %open-broadcast ((List Pathname) -> IfExists -> IfDoesNotExist -> ElementType -> (Result FileError FileStream)))
  (define (%open-broadcast paths if-exists if-does-not-exist etype)
    (let ((streams (map (fn (p)
                          (%open-output p if-exists if-does-not-exist etype))
                        paths))
          (error-found (iter:find! res:err? (iter:into-iter streams))))
      (match error-found
        ((Some (Err x))
         (Err x))
        ((None)
         (Ok (BroadcastStream (iter:collect! (map (fn (x)
                                                    (unwrap (the (Optional FStream)
                                                                 (into x))))
                                                  (iter:into-iter streams))))))
        (_ (Err (FileError "Invalid Stream" ""))))))


  (declare open (StreamOptions -> (Result FileError FileStream)))
  (define (open stream-options)
    (match stream-options
      ((Input path etype)
       (match (%open-input path etype)
         ((Ok s)
          (Ok (InputStream s)))
         ((Err r)
          (Err r))))
      ((Output path exists does-not-exist etype)
       (match (%open-output path exists does-not-exist etype)
         ((Ok s)
          (Ok (OutputStream s)))
         ((Err r)
          (Err r))))
      ((TwoWay path exists does-not-exist etype)
       (%open-twoway path exists does-not-exist etype))
      ((Broadcast paths exists does-not-exist etype)
       (%open-broadcast paths exists does-not-exist etype)))))

;;;
;;; Operations on FileStreams
;;;

(coalton-toplevel

  (define (element-type stream)
    (let ((str (match stream
                 ((InputStream str)
                  str)
                 ((OutputStream str)
                  str)
                 ((TwoWayStream stri _)
                  stri)
                 ((BroadcastStream strs)
                  (list:car strs)))))
      (lisp ElementType (str)
        (cl:typecase (cl:stream-element-type str)
          (cl:integer                          'ElementType/Int)
          (cl:fixnum                           'ElementType/SFix)
          ((cl:signed-byte 64)                 'ElementType/S64)
          ((cl:signed-byte 32)                 'ElementType/S32)
          ((cl:signed-byte 16)                 'ElementType/S16)
          ((cl:signed-byte 8)                  'ElementType/S8)
          ((cl:and cl:fixnum cl:unsigned-byte) 'ElementType/USFix)
          ((cl:unsigned-byte 64)               'ElementType/US64)
          ((cl:unsigned-byte 32)               'ElementType/US32)
          ((cl:unsigned-byte 16)               'ElementType/US16)
          ((cl:unsigned-byte 8)                'ElementType/US8)
          (cl:character                        'ElementType/Character)))))

  (define-class (Streamable :a)
    (write (stream )))
  )
                                       


#+ignore
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
