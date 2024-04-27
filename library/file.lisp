(coalton-library/utils:defstdlib-package #:coalton-library/file
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)
  (:local-nicknames
   (#:str #:coalton-library/string)
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:list #:coalton-library/list)
   (#:vec #:coalton-library/vector)
   (#:res #:coalton-library/result)
   (#:types #:coalton-library/types))
  (:export
   #:Pathname
   #:Merge

   #:exists?
   #:if-exists
   #:if-does-not-exist
   #:if-directory
   #:if-file

   ;; directory functions
   #:mkdir
   #:ensure-directories
   #:directory-files
   #:subdirectories
   #:empty?
   #:if-empty
   #:if-not-empty
   #:rmdir
   #:rmdir-unsafe
   #:system-relative-pathname

   #:copy
   #:delete
   #:read-file-string
   #:read-file-lines
   #:cat

   #:FileStream
   #:InputStream
   #:OutputStream
   #:TwoWayStream
   #:BroadcastStream

   #:IfExists
   #:EError
   #:NewVersion
   #:Rename
   #:RenameAndDelete
   #:Overwrite
   #:Append
   #:Supersede

   #:IfDoesNotExist
   #:DNEError
   #:Create

   #:ElementType
   #:Int
   #:SFix
   #:S64
   #:S32
   #:S16
   #:S8
   #:USFix
   #:US64
   #:US32
   #:US16
   #:US8
   #:Character

   #:StreamOptions
   #:Input
   #:Output
   #:TwoWay
   #:Broadcast

   #:open))

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

  (define-instance (Into String Pathname)
    (define (into s)
      (lisp Pathname (s)
        (cl:pathname s))))
  #+ig(define-instance (TryInto String Pathname)
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
    (StreamError String)
    (ASDFError String String)
    (ReadError String String)
    EOF
    (WriteError String String))

  (define-instance (Signal FileError)
    (define (error ferr)
      (match ferr
        ((PathError str path)
         (error (lisp String (str path)
                  (cl:format cl:nil "Path Error:~%~%~a ~a" str path))))
        ((FileError str path)
         (error (lisp String (str path)
                  (cl:format cl:nil "File Error:~%~%~a ~a" str path))))
        ((ASDFError str1 str2)
         (error (lisp String (str1 str2)
                  (cl:format cl:nil "ASDF Error:~%~%~a ~a" str1 str2))))
        ((EOF)
         (error (lisp String ()
                  (cl:format cl:nil "Error:~%~%End of File"))))
        ((WriteError str1 str2)
         (error (lisp String (str1 str2)
                  (cl:format cl:nil "Write Error:~%~%~a ~a" str1 str2))))
        (_ (error "not yet"))))
    
    (define (warn ferr)
      (match ferr
        ((PathError str path)
         (warn (lisp String (str path)
                 (cl:format cl:nil "~a ~a" str path))))
        ((FileError str path)
         (error (lisp String (str path)
                  (cl:format cl:nil "~a ~a" str path))))
        ((ASDFError str1 str2)
         (warn (lisp String (str1 str2)
                 (cl:format cl:nil "~a ~a" str1 str2))))
        ((EOF)
         (warn "End of File"))
        ((WriteError str1 str2)
         (warn (lisp String (str1 str2)
                 (cl:format cl:nil "~a ~a" str1 str2))))
        (_ (warn "not yet"))))))


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
        (cl:error () (Err (ASDFError "System not found:" system-name)))))))

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

  (define-type (FileStream :a)
    "Represents a lisp file-stream or collection of file-streams with element-type `:a`."
    (InputStream FStream)
    (OutputStream FStream)
    (TwoWayStream FStream Fstream)
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
  
  (define-type StreamOptions
    (Input Pathname)
    (Output Pathname IfExists IfDoesNotExist)
    (TwoWay Pathname IfExists IfDoesNotExist)
    (Broadcast (list Pathname) IfExists IfDoesNotExist))

  ;;
  ;; Opening file streams
  ;;
  
  (declare %open-input (Pathname -> types:lisptype -> (Result FileError FStream)))
  (define (%open-input path etype)
    (lisp (Result FileError FStream) (path etype)
      (cl:handler-case (Ok (cl:open path
                                    :direction :input
                                    :element-type etype))
        (cl:error () (Err (PathError "Error opening input stream with path" path))))))


  (declare %open-output (Pathname -> IfExists -> IfDoesNotExist -> types:lisptype -> (Result FileError FStream)))
  (define (%open-output path if-exists if-does-not-exist etype)
    (lisp (Result FileError FStream) (path if-exists if-does-not-exist etype)
      (cl:handler-case (Ok (cl:open path
                                    :direction :output
                                    :element-type etype
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
        (cl:error () (Err (PathError "Error opening output stream with path" path))))))

  (declare %open-twoway (Pathname -> IFExists -> IfDoesNotExist -> types:lisptype -> (Result FileError (FileStream :a))))
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

  (declare %open-broadcast ((List Pathname) -> IfExists -> IfDoesNotExist -> types:lisptype -> (Result FileError (FileStream :a))))
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

  (declare %open (StreamOptions -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open stream-options etype)
    (match stream-options
      ((Input path)
       (match (%open-input path etype)
         ((Ok s)
          (Ok (InputStream s)))
         ((Err r)
          (Err r))))
      ((Output path exists does-not-exist)
       (match (%open-output path exists does-not-exist etype)
         ((Ok s)
          (Ok (OutputStream s)))
         ((Err r)
          (Err r))))
      ((TwoWay path exists does-not-exist)
       (%open-twoway path exists does-not-exist etype))
      ((Broadcast paths exists does-not-exist)
       (%open-broadcast paths exists does-not-exist etype))))

  ;;
  ;;
  ;;

  (declare %close-fstream ((FStream -> (Result FileError :a))))
  (define (%close-fstream fs)
    (lisp (Result FileError :a) (fs) 
      (cl:handler-case (Ok (cl:close fs))
        (cl:error () (Err (StreamError "Issue closing stream."))))))
  
  (declare %close ((Result FileError (FileStream :a)) -> (Result FileError Unit)))
  (define (%close fs)
    (match fs
      ((Err _r)
       (Err (StreamError "Stream can't be closed, already not open.")))
      ((Ok s)
       (match s
         ((InputStream f)
          (%close-fstream f))
         ((OutputStream f)
          (%close-fstream f))
         ((TwoWayStream in out)
          (%close-fstream in)
          (%close-fstream out))
         ((BroadcastStream streams)
          (Ok (for stream in streams
                   (%close-fstream stream))))))))

  ;;
  ;; Reading from File Streams
  ;;
  
  (declare %read-char-from-stream (FStream -> (Result FileError Char)))
  (define (%read-char-from-stream fus)
    (lisp (Result FileError Char) (fus)
      (cl:handler-case (Ok (cl:read-char fus))
        (cl:end-of-file () (Err (EOF)))
        (cl:error () (Err (ReadError "Non-EOF error reading from stream" fus))))))
  
  (declare %read-char ((Result FileError (FileStream Char)) -> (Result FileError Char)))
  (define (%read-char fs)
    (match fs
      ((Ok stream)
       (match stream
         ((InputStream s)
          (%read-char-from-stream s))
         ((TwoWayStream in _out)
          (%read-char-from-stream in))
         (_ (Err (ReadError "Invalid Stream" "Target stream is Output only.")))))
      ((Err r)
       (Err r))))

  (declare %read-byte-from-stream (FStream -> (Result FileError :a)))
  (define (%read-byte-from-stream fus)
    (lisp (Result FileError :a) (fus)
      (cl:handler-case (Ok (cl:read-byte fus))
        (cl:end-of-file () (Err (EOF)))
        (cl:error () (Err (ReadError "Non-EOF error reading from stream" fus))))))
  
  (declare %read-byte ((Result FileError (FileStream :a)) -> (Result FileError :a)))
  (define (%read-byte fs)
    (match fs
      ((Ok stream)
       (match stream
         ((InputStream s)
          (%read-byte-from-stream s))
         ((TwoWayStream in _out)
          (%read-byte-from-stream in))
         (_ (Err (ReadError "Invalid Stream" "Target stream is Output only.")))))
      ((Err r)
       (Err r))))

  ;;
  ;; Wrte
  ;;
  
  (declare %write-char-to-stream (FStream -> :a -> (Result FileError :b)))
  (define (%write-char-to-stream fs data)
    (lisp (Result FileError :a) (fs data)
      (cl:handler-case (Ok (cl:write-char data fs))
        (cl:error () (Err (WriteError "Invalid Write Operation." ""))))))
  
  (declare %write-char ((Result FileError (FileStream :a)) -> :a -> (Result FileError Unit)))
  (define (%write-char fs data)
    (match fs
      ((Ok s)
       (match s
         ((OutputStream stream)
          (%write-char-to-stream stream data))
         ((TwoWayStream _in out)
          (%write-char-to-stream out data))
         ((BroadcastStream streams)
          (for s in streams
               (%write-char-to-stream s data))
          (Ok Unit))
         ((InputStream _)
          (Err (WriteError "Invalid write operation:" "Target stream is Input only.")))))
      ((Err r)
       (Err r))))

  (declare %write-byte-to-stream (FStream -> :a -> (Result FileError :b)))
  (define (%write-byte-to-stream fs data)
    (lisp (Result FileError :a) (fs data)
      (cl:handler-case (Ok (cl:write-byte data fs))
        (cl:error () (Err (WriteError "Invalid Write Operation." ""))))))
  
  (declare %write-byte ((Result FileError (FileStream :a)) -> :a -> (Result FileError Unit)))
  (define (%write-byte fs data)
    (match fs
      ((Ok s)
       (match s
         ((OutputStream stream)
          (%write-byte-to-stream stream data))
         ((TwoWayStream _in out)
          (%write-byte-to-stream out data))
         ((BroadcastStream streams)
          (for s in streams
               (%write-byte-to-stream s data))
          (Ok Unit))
         ((InputStream _)
          (Err (WriteError "Invalid write operation:" "Target stream is Input only.")))))
      ((Err r)
       (Err r))))

  
  (define-class (Streamable :a)
    "A class of types which are able to be written to or read from a file."
    (open (StreamOptions -> (Result FileError (Filestream :a))))
    (close ((Result FileError (FileStream :a)) -> (Result FileError Unit)))
    (read ((Result FileError (FileStream :a)) -> (Result FileError :a)))
    ;; (read-sequence)
    (write ((Result FileError (FileStream :a)) -> :a -> (Result FileError Unit)))
    ;; (write-sequence ((Iterable :b) (FileStream :a) -> (:b :a) -> (Result FileError Unit)))
    )

  ;; proxy of-

  (define-instance (Streamable Char)
    (define (open stream-options)
      (let ((Type (types:runtime-repr (the (types:Proxy Char) types:Proxy))))
        (%open stream-options type)))
    (define (close fs)
      (%close fs))
    (define (read fs)
      (%read-char fs))
    (define (write fs data)
      (%write-char fs data)))

  (define-instance (Streamable U64)
    (define (open stream-options)
      (let ((type (types:runtime-repr (the (types:Proxy U64) types:Proxy))))
        (%open stream-options type)))
    (define (close fs)
      (%close fs))
    (define (read fs)
      (%read-byte fs))
    (define (write fs data)
      (%write-byte fs data)))

  (declare test-open-read-close (Unit -> (Result FileError Unit)))
  (define (test-open-read-close)
    (let ((stream (open (Input (into "test.py")))))
      (traceobject "Character read:" (the (Result FileError Char) (read stream)))
      (close stream)))

  (declare read-whole-file (Unit -> (Result FileError Unit)))
  (define (read-whole-file)
    (let ((stream (open (Input (into "test.py"))))
          (fread (fn ()
                   (match (the (Result FileError Char) (read stream))
                     ((Ok x)
                      (traceobject "Character read:" x)
                      (fread))
                     ((Err _r)
                      Unit)))))
      (fread)
      (close stream))) ;; file error :a

  ;; look at iter:collect!
  



  #+ignore  (define-instance (Streamable I64)
              (define (open stream-options)
                (let ((type (types:runtime-repr (the (types:Proxy I64) types:Proxy))))
                  (%open stream-options type)))
              (define (read fs)
                (%read fs)))

  

  
  #+ignore(define-instance (Streamable Integer)
            (define (open stream-options)
              (the (Proxy Integer)
                   (types:runtime-repr proxy-inner)) (let p = types:Proxy)
              (let p_ = (types:proxy-inner p))
              (let type = (types:runtime-repr p_))
              (type:as-proxy-of
               )
              ;; (the (proxy Integer))
              (let ((type ())
                    )
                (match ))))

  
  )

#+ignore
(coalton-toplevel

  (repr :native cl:file-stream)
  (define-type FStream)

  (define-type (FileStream :a)
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

                                        ;(repr :enum)
  #+ignore(define-type ElementType
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
    (TwoWay Pathname IfExists IfDoesNotExist)
    (Broadcast (list Pathname) IfExists IfDoesNotExist))

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

#+ignore(coalton-toplevel

  (define-class (Streamable :a)
    "A class of types which are able to be written to or read from a file."
    (open (StreamOptions -> (Result FileError (Filestream :a))))
    (read (FileStream -> :a))
    (write (FileStream -> :a -> Unit)))

  
  ;(define (write stream data))
  )

#+ignore(coalton-toplevel

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
