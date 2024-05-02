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
   (#:types #:coalton-library/types)
   (#:str #:coalton-library/string)
   (#:char #:coalton-library/char))
  (:export
   
   #:Pathname
   #:TryintoPathname
   #:Merge

   #:exists?
   #:directory?
   #:file?

   ;; directory functions
   #:create-directory
   #:ensure-directories
   #:directory-files
   #:subdirectories
   #:empty?
   #:remove-directory
   #:remove-directory-unsafe
   #:system-relative-pathname

   #:copy
   #:delete-file

   #:FStream
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

   #:StreamOptions
   #:Input
   #:Output
   #:TwoWay
   #:Broadcast

   #:Streamable
   #:open
   #:close
   #:read
   #:read-sequence
   #:write
   #:write-sequence

   #:with-open-file
   #:read-char
   #:read-byte
   #:write-char
   #:write-byte
   #:write-string

   #:force
   #:finish
   #:clear
   #:file-position
   #:set-file-position
   
   #:read-file-string
   #:read-file-lines
   ))

(in-package #:coalton-library/file)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; File Error handling
;;;
;;; File errors will return results of type `(Result FileError :a)`
;;;


;;;
;;; Pathnames
;;;

(coalton-toplevel

  (repr :native cl:pathname)
  (define-type Pathname
    "Pathname object. Uses `cl:pathname`"))

(coalton-toplevel

  ;; TODO finalize FileError types, error/warning messages
  (define-type FileError
    "Errors for file functions. File functions will return results of the type `(Result FileError :a)`. You can add signalling by wrapping a returned FileError with either `warn` or `error` (e.g. `(match (open (Input \"file.txt\")) ((Ok stream) stream) ((Err r) (warn r)))`)"
    (TryintoPathnameError String String)
    (PathError String Pathname)
    (LispIMPLError String)
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
        ((FileError str1 str2)
         (error (lisp String (str1 str2)
                  (cl:format cl:nil "File Error:~%~%~a ~a" str1 str2))))
        ((ASDFError str1 str2)
         (error (lisp String (str1 str2)
                  (cl:format cl:nil "ASDF Error:~%~%~a ~a" str1 str2))))
        ((EOF)
         (error (lisp String ()
                  (cl:format cl:nil "Error:~%~%End of File"))))
        ((LispIMPLError str)
         (error (lisp String (str)
                  (cl:format cl:nil "Lisp Implementation Error:~%~%~a" str))))
        ((WriteError str1 str2)
         (error (lisp String (str1 str2)
                  (cl:format cl:nil "Write Error:~%~%~a ~a" str1 str2))))
        (_ (error "not yet"))))
    
    (define (warn ferr)
      (match ferr
        ((PathError str path)
         (warn (lisp String (str path)
                 (cl:format cl:nil "~a ~a" str path))))
        ((FileError str1 str2)
         (error (lisp String (str1 str2)
                  (cl:format cl:nil "~a ~a" str1 str2))))
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
;;; Pathname type
;;;

(coalton-toplevel

  (define-class (TryintoPathname :a)
    (tryinto-pathname (:a -> (Result FileError Pathname))))

  (define-instance (TryIntoPathname String)
    (define (tryinto-pathname s)
      (lisp (Result FileError Pathname) (s)
        (cl:handler-case (Ok (cl:pathname s))
          (cl:error ()                
            (Err (TryintoPathnameError "Invalid pathname string:" s)))))))

  (define-instance (TryIntoPathname Pathname)
    (define (tryinto-pathname s)
      (Ok s)))
  
  (define-instance (Into Pathname String)
    (define (into p)
      (lisp String (p)
        (cl:namestring p))))
  
  (declare merge ((TryintoPathname :a) (TryintoPathname :b) => :a -> :b -> (Result FileError Pathname)))
  (define (merge path1 path2)
    "Merges two pathnames together. The order is important- for adding subdirectories, make sure to put the subpath (the one without the leading `/`) in `path1`."
    (do
     (p1 <- (tryinto-pathname path1))
     (p2 <- (tryinto-pathname path2))
      (pure (lisp Pathname (p1 p2)
              (cl:merge-pathnames p1 p2))))))

;;;
;;; Handling existential path queries
;;;

(coalton-toplevel

  (declare exists? ((TryintoPathname :a) => :a -> (Result FileError Boolean)))
  (define (exists? path)
    "Returns whether a file or directory exists."
    (do
     (p <- (tryinto-pathname path))
     (pure (lisp Boolean (p)
             (to-boolean (cl:probe-file p))))))

  (declare directory? ((TryintoPathname :a) => :a -> (Result FileError Boolean)))
  (define (directory? path)
    "Returns whether a path represents a directory."
    (do
     (p <- (tryinto-pathname path))
     (pure (lisp Boolean (p)
             (cl:not (to-boolean (uiop:file-pathname-p p)))))))

  (declare file? ((TryintoPathname :a) => :a -> (Result FileError Boolean)))
  (define (file? path)
    "Returns whether a path represents a file."
    (do
     (p <- (tryinto-pathname path))
     (pure (lisp Boolean (p)
             (to-boolean (uiop:file-pathname-p p)))))))

;;;
;;; Working with directories
;;;

(coalton-toplevel
  
  (declare create-directory ((TryintoPathname :a) => :a -> (Result FileError Pathname)))
  (define (create-directory path)
    "This is equivalent to `mkdirp`. Makes a directory or chain of directories if they don't already exist. The pathname must end with a trailing `/`."
    (do
     (p <- (tryinto-pathname path))
     (dir <- (directory? p))
      (if dir
          (Ok (lisp Pathname (p)
                (cl:ensure-directories-exist p)))
          (Err (PathError "This path does not represent a directory:" p)))))

  (declare ensure-directories ((TryintoPathname :a) => :a -> (Result FileError Unit)))
  (define (ensure-directories path)
    "Ensures that a directory or chain of directories exists. The pathname can end with either a file or directory; ending with a file will ensure all directories above that file, ending with a directory (with a trailing '/') will ensure all directories above and including that directory."
    (do
     (p <- (tryinto-pathname path))
     (pure (lisp Unit (p)
             (cl:ensure-directories-exist p)
             Unit))))

  (declare directory-files ((TryintoPathname :a) => :a -> (Result FileError (List Pathname))))
  (define (directory-files path)
    "Returns all files within the directory. Returns an error if the pathname does not represent a directory."
    (do
     (p <- (tryinto-pathname path))
     (dir <- (directory? p))
      (if dir
          (Ok (lisp (List Pathname) (p)
                (uiop:directory-files p)))
          (Err (PathError "Pathname does not represent a directory. Please add a trailing `\`." p)))))

  (declare subdirectories ((TryintoPathname :a) => :a -> (Result FileError (List Pathname))))
  (define (subdirectories path)
    "Returns all subdirectories within the directory. Returns an error if the pathname does not represent a directory."
    (do
     (p <- (tryinto-pathname path))
     (dir <- (directory? p))
      (if dir
          (Ok (lisp (List Pathname) (path)
                (uiop:subdirectories path)))
          (Err (PathError "Pathname does not represent a directory. Please add a trailing `\`." p)))))

  ;;
  ;; Handling directory behavior that depends on emptiness
  ;;
  
  (declare empty? ((TryintoPathname :a) => :a -> (Result FileError Boolean)))
  (define (empty? path)
    "Checks whether a directory is empty."
    (do
     (p <- (tryinto-pathname path))
     (dir <- (directory? p))
      (if dir
          (Ok (and (list:null? (unwrap (directory-files p)))
                   (list:null? (unwrap (subdirectories p)))))
          (Err (PathError "Pathname does not represent a directory" p)))))

  (declare remove-directory ((TryintoPathname :a) => :a -> (Result FileError Unit)))
  (define (remove-directory path)
    "Deletes a directory `dir` if it's empty, otherwise, returns`FileError`."
    (do
     (p <- (tryinto-pathname path))
     (mt <- (empty? p))
      (if mt
          (Ok (Lisp Unit (p)
                (uiop:delete-empty-directory p)
                Unit))
          (Err (PathError "Directory is not empty:" p)))))
  
  (declare remove-directory-unsafe ((TryintoPathname :a) => :a -> (Result FileError Unit)))
  (define (remove-directory-unsafe path)
    "Deletes a target directory recursively. Equivalent to `rm -r`. Errors if the path is not a directory (if the path wasn't a directory, this would remove the path's parent directory)."
    (do
     (p <- (tryinto-pathname path))
     (dir <- (directory? p))
      (if dir
              (Ok (lisp Unit (path)
                    (uiop:delete-directory-tree path :validate cl:t)
                    Unit))
              (Err (PathError "Path does not represent a directory." p)))))
  
  (declare system-relative-pathname ((Into :a String) => :a -> String -> (Result FileError Pathname)))
  (define (system-relative-pathname system-name name)
    "Generates a system-relative-pathname for a given filename or path."
    (lisp (Result FileError Pathname) (system-name name)
      (cl:handler-case (Ok (asdf:system-relative-pathname system-name name))
        (cl:error () (Err (ASDFError "System not found:" system-name)))))))

;;;
;;; Basic File Operations
;;;

(coalton-toplevel

  (declare copy ((TryintoPathname :a) (TryintoPathname :b) => :a -> :b -> (Result FileError Unit)))
  (define (copy input output)
    "Copies a file to a nonexistant file. Will error if the output path already exists or if the input path isn't a file."
    (do
     (in <- (tryinto-pathname input))
     (out <- (tryinto-pathname output))
      (output-exists <- (exists? out))
      (input-is-file <- (file? in))
      (if output-exists
          (Err (PathError "Invalid output for copying, path already exists:" out))
          (if (not input-is-file)
              (Err (PathError "Invalid input for copying, path is not a file:" in))
              (pure (lisp Unit (in out)
                      (uiop:copy-file in out)
                      Unit))))))

  (declare delete-file ((TryintoPathname :a) => :a -> (Result FileError Unit)))
  (define (delete-file path)
    "Deletes a given file if the file exists."
    (do
     (p <- (tryinto-pathname path))
     (is-file <- (file? p))
      (if is-file
          (Ok (lisp Unit (p)
                (uiop:delete-file-if-exists p)))
          (Err (PathError "Can't delete file: path does not represent a file" p))))))

;;;
;;; FStreams, FileStreams, and options
;;;

(coalton-toplevel

  ;;
  ;; FStreams, FileStreams, and options
  ;;
  (repr :native cl:file-stream)
  (define-type FStream
    "Represents a lisp file-stream, not intended to be accessed directly.")

  (define-type (FileStream :a)
    "Contains an `FStream` or multiple `FStreams` with element-type `:a`."
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
    EError
    NewVersion
    Rename
    RenameAndDelete
    Overwrite
    Append
    Supersede)

  (repr :enum)
  (define-type IfDoesNotExist
    "Possible options for opening a stream when the file does not exist."
    DNEError
    Create)
  
  (define-type StreamOptions
    "A type for providing parameters for opening streams. StreamOptions take strings for pathnames, but they will error if they are not proper and appropriate pathnames."
    (Input String)
    (Output String IfExists IfDoesNotExist)
    (TwoWay String IfExists IfDoesNotExist)
    (Broadcast (list String) IfExists IfDoesNotExist))
  (declare %open-input (Pathname -> types:lisptype -> (Result FileError FStream)))
  (define (%open-input path etype)
    "Opens an input stream for the given filepath, and for a given type."
    (lisp (Result FileError FStream) (path etype)
      (cl:handler-case (Ok (cl:open path
                                    :direction :input
                                    :element-type etype))
        (cl:error () (Err (PathError "Error opening input stream with path" path))))))

  ;;
  ;; Opening streams
  ;;
  (declare %open-output (Pathname -> IfExists -> IfDoesNotExist -> types:lisptype -> (Result FileError FStream)))
  (define (%open-output path if-exists if-does-not-exist etype)
    "Opens an output stream for the given filepath, and for a given type."
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
    "Opens an two way stream for the given filepath, and for a given type."
    (do
     (in <- (%open-input path etype))
     (out <- (%open-output path if-exists if-does-not-exist etype))
      (pure (TwoWayStream in out))))

  (declare %open-broadcast ((TryintoPathname :a) => (List :a) -> IfExists -> IfDoesNotExist -> types:lisptype -> (Result FileError (FileStream :b))))
  (define (%open-broadcast paths if-exists if-does-not-exist etype)
    "Opens a broadcast stream for a list of filepaths. These are all output streams."
    (let streams = (cell:new Nil))
    (for path in paths
         (do
          (p <- (tryinto-pathname path))
          (stream <- (%open-output p if-exists if-does-not-exist etype))
           (Ok (BroadcastStream (cell:push! streams stream)))))
    (Ok (BroadcastStream (cell:read streams))))

  (declare %open (StreamOptions -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open stream-options etype)
    "Opens a FileStream for a given type and StreamOptions."
    (match stream-options
      ((Input path)
       (do
        (p <- (tryinto-pathname path))
        (in <- (%open-input p etype))
         (pure (InputStream in))))
      ((Output path exists does-not-exist)
       (do
        (p <- (tryinto-pathname path))
        (out <- (%open-output p exists does-not-exist etype))
         (pure (OutputStream out))))
      ((TwoWay path exists does-not-exist)
       (do
        (p <- (tryinto-pathname path))
        (%open-twoway p exists does-not-exist etype)))
      ((Broadcast paths exists does-not-exist)
       (%open-broadcast paths exists does-not-exist etype))))

  ;;
  ;; Closing streams
  ;;
  (declare %close-fstream ((FStream -> (Result FileError Unit))))
  (define (%close-fstream fs)
    "Closes an FStream."
    (lisp (Result FileError Unit) (fs) 
      (cl:handler-case (Ok (cl:progn
                             (cl:close fs)
                             Unit))
        (cl:error () (Err (StreamError "Issue closing stream."))))))

  (declare %close ((FileStream :a) -> (Result FileError Unit)))
  (define (%close fs)
    "Closes a Filestream, if it's open."
    (match fs
      ((InputStream f)
       (%close-fstream f))
      ((OutputStream f)
       (%close-fstream f))
      ((TwoWayStream in out)
       (%close-fstream in)
       (%close-fstream out))
      ((BroadcastStream streams)

       ;; mapM for-each M
       (Ok (for stream in streams
                (do
                 (%close-fstream stream)))))))

  ;;
  ;; Reading from Input streams
  ;;
  (declare %read-char-from-stream (FStream -> (Result FileError Char)))
  (define (%read-char-from-stream fs)
    "Reads a character from an FStream."
    (lisp (Result FileError Char) (fs)
      (cl:handler-case (Ok (cl:read-char fs))
        (cl:end-of-file () (Err (EOF)))
        (cl:error () (Err (ReadError "Non-EOF error reading from stream" fs))))))
  
  (declare read-char ((FileStream Char) -> (Result FileError Char)))
  (define (read-char fs)
    "Reads a character from a character FileStream."
    (match fs
      ((InputStream s)
       (%read-char-from-stream s))
      ((TwoWayStream in _out)
       (%read-char-from-stream in))
      (_ (Err (ReadError "Invalid Stream" "Only input streams can be read.")))))

  (declare %read-byte-from-stream (FStream -> (Result FileError :a)))
  (define (%read-byte-from-stream fs)
    (lisp (Result FileError :a) (fs)
      (cl:handler-case (Ok (cl:read-byte fs))
        (cl:end-of-file () (Err (EOF)))
        (cl:error () (Err (ReadError "Non-EOF error reading from stream" fs))))))
  
  (declare read-byte ((FileStream :a) -> (Result FileError :a)))
  (define (read-byte fs)
    "Reads a byte from the FileStream."
    (match fs
      ((InputStream s)
       (%read-byte-from-stream s))
      ((TwoWayStream in _out)
       (%read-byte-from-stream in))
      (_ (Err (ReadError "Invalid Stream" "Target stream is Output only.")))))

  (declare %read-sequence ((Streamable :a) => (FileStream :a) -> (Result FileError (iter:Iterator :a))))
  (define (%read-sequence fs)
    "Reads a file into an Iterator."
    (let ((sequence (cell:new Nil))
          (fread (fn ()
                   (match (read fs)
                     ((Err (EOF))
                      (Ok (iter:into-iter (list:reverse (cell:read sequence)))))
                     ((Err r)
                      (Err r))
                     ((Ok elt)
                      (cell:push! sequence elt)
                      (fread))))))
      (fread)))

  ;;
  ;; Writing to streams
  ;;
  (declare %write-char-to-stream (FStream -> :a -> (Result FileError Unit)))
  (define (%write-char-to-stream fs data)
    (lisp (Result FileError Unit) (fs data)
      (cl:handler-case (Ok (cl:progn
                             (cl:write-char data fs)
                             Unit))
        (Cl:Nil () (Err (WriteError "Invalid Write Operation" "")))
        (cl:error () (Err (WriteError "Invalid Write Operation." ""))))))
  
  (declare write-char ((FileStream :a) -> :a -> (Result FileError Unit)))
  (define (write-char fs data)
    "Writes a `Char` to the stream."
    (match fs
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

  (declare %write-byte-to-stream (FStream -> :a -> (Result FileError Unit)))
  (define (%write-byte-to-stream fs data)
    (lisp (Result FileError Unit) (fs data)
      (cl:handler-case (Ok (cl:progn
                             (cl:write-byte data fs)
                             Unit))
        (cl:error () (Err (WriteError "Invalid Write Operation." ""))))))
  
  (declare write-byte ((FileStream :a) -> :a -> (Result FileError Unit)))
  (define (write-byte fs data)
    "Writes a byte to the stream."
    (match fs
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

  (declare %write-sequence (Streamable :a => (FileStream :a) -> (iter:Iterator :a) -> (Result FileError Unit)))
  (define (%write-sequence fs sequence)
    (Ok (for element in sequence
             (do
              (written <- (write fs element))
              (pure written)))))

  ;;
  ;; Other Stream functions
  ;;
  (declare %force (FStream -> (Result FileError Unit)))
  (define (%force fs)
    (lisp :a (fs)
      (cl:handler-case (Ok (cl:force-output fs))
        (cl:error () (Err (StreamError "Force did not work."))))))
  
  (declare force ((FileStream :a) -> (Result FileError Unit)))
  (define (force fs)
    "Forces output to a given stream. Force initiates the emptying of internal buffers but does not wait for completion."
    (match fs
      ((OutputStream f)
       (%force f))
      ((TwoWayStream _in out)
       (%force out))
      ((BroadcastStream streams)
       (Ok (for s in streams
                (%force s))))
      ((InputStream _)
       (Err (StreamError "Force output can't be used on input streams.")))))

  (declare %finish (FStream -> (Result FileError Unit)))
  (define (%finish fs)
    (lisp :a (fs)
      (cl:handler-case (Ok (cl:finish-output fs))
        (cl:error () (Err (StreamError "Finish did not work."))))))

  (declare finish ((FileStream :a) -> (Result FileError Unit)))
  (define (finish fs)
    "Attempts to ensure that any buffered output sent to output has reached its destination."
    (match fs
      ((OutputStream f)
       (%force f))
      ((TwoWayStream _in out)
       (%force out))
      ((BroadcastStream streams)
       (Ok (for s in streams
                (%force s))))
      ((InputStream _)
       (Err (StreamError "Finish output can't be used on input streams.")))))

  (declare %clear (FStream -> (Result FileError Unit)))
  (define (%clear fs)
    (lisp :a (fs)
      (cl:handler-case (Ok (cl:clear-output fs))
        (cl:error () (Err (StreamError "Finish did not work."))))))

  
  (declare clear ((FileStream :a) -> (Result FileError Unit)))
  (define (clear fs)
    "Attempts to abort any outstanding output operation in order to allow as little output as possible to continue to the destination."
    (match fs
      ((OutputStream f)
       (%force f))
      ((TwoWayStream _in out)
       (%force out))
      ((BroadcastStream streams)
       (Ok (for s in streams
                (%force s))))
      ((InputStream _)
       (Err (StreamError "Finish output can't be used on input streams.")))))

  ;;
  ;; File Position
  ;;
  (declare %file-position (FStream -> (Result FileError UFix)))
  (define (%file-position fs)
    (lisp :a (fs)
      (cl:handler-case (Ok (cl:file-position fs))
        (cl:error () (Err (StreamError "File position can't be determined."))))))
  
  (declare file-position ((FileStream :a) -> (Result FileError UFix)))
  (define (file-position fs)
    "Returns the current position within a stream."
    (match fs
      ((OutputStream f)
       (%file-position f))
      ((TwoWayStream _in out)
       (%file-position out))
      ((BroadcastStream streams)
       (%file-position (list:car streams)))
      ((InputStream f)
       (%file-position f))))

  (declare %set-file-position (FStream -> UFix -> (Result FileError UFix)))
  (define (%set-file-position fs i)
    (lisp :a (fs i)
      (cl:handler-case (Ok (cl:file-position fs i))
        (cl:error () (Err (StreamError "File position can't be assigned."))))))
  
  (declare set-file-position ((FileStream :a) -> UFix -> (Result FileError UFix)))
  (define (set-file-position fs i)
    "Returns the current position within a string."
    (match fs
      ((OutputStream f)
       (%set-file-position f i))
      ((InputStream f)
       (%set-file-position f i))
      ((BroadcastStream streams) ;; this feels like a risky fix
       (%set-file-position (list:car streams) i))
      ;; this may not be supported
      ((TwoWayStream _in out)
       (%set-file-position out i))))

  ;;
  ;; Streamable class, top level access
  ;;
  (define-class (Streamable :a)
    "A class of types which are able to be written to or read from a file."
    (open           (StreamOptions   -> (Result FileError (Filestream :a))))
    (close          ((FileStream :a) -> (Result FileError Unit)))
    (read           ((FileStream :a) -> (Result FileError :a)))
    (read-sequence  ((FileStream :a) -> (Result FileError (iter:Iterator :a))))
    (write          ((FileStream :a) -> :a -> (Result FileError Unit)))
    (write-sequence ((FileStream :a) -> (iter:Iterator :a) -> (Result FileError Unit))))

  (declare with-open-file ((Streamable :a) => StreamOptions -> ((FileStream :a) -> (Result FileError :b)) -> (Result FileError :b)))
  (define (with-open-file stream-options thunk)
    "Opens a file stream and performs `thunk` on it"
    (do
     (stream <- (open stream-options))
     (res <- (thunk stream))
      (close stream)
      (pure res)))
  
  (define-instance (Streamable Char)
    (define (open stream-options)
      (let ((Type (types:runtime-repr (the (types:Proxy Char) types:Proxy))))
        (%open stream-options type)))
    (define (close fs)
      (%close fs))
    (define (read fs)
      (read-char fs))
    (define (read-sequence fs)
      (%read-sequence fs))
    (define (write fs data)
      (write-char fs data))
    (define (write-sequence fs sequence)
      (%write-sequence fs sequence)))

  (declare write-string ((FileStream Char) -> String -> (Result FileError Unit)))
  (define (write-string fs s)
    "Writes a `string` to a FileStream."
    (write-sequence fs (str:chars s)))

  (declare write-line ((FileStream Char) -> String -> (Result FileError Unit)))
  (define (write-line fs s)
    "Writes a string with an appended newline to the filestream."
    (write-sequence fs (Str:chars (lisp String (s)
                                    (cl:format cl:nil "~a~%" s)))))

  (declare read-file-string ((Into :a String) => :a -> (Result FileError String)))
  (define (read-file-string path-string)
    "Reads a file into a string, given a pathname string."
    (with-open-file (Input (into path-string))
      (fn (stream)
        (do
         (chars <- (read-sequence stream))
         (pure (into (the (list Char) (iter:collect! chars))))))))

  (declare %read-file-lines ((FileStream Char) -> (Result FileError (List String))))
  (define (%read-file-lines fs)
    "Reads a filestream into lines."
    (let current-line = (cell:new Nil))
    (let lines = (cell:new Nil))
    (do
     (chars <- (read-sequence fs))
     (pure (progn
             (iter:for-each!
              (fn (c)
                (cond ((== c (unwrap (char:code-char 10)))
                       (cell:push! lines (the String (into (list:reverse (cell:read current-line)))))
                       (cell:write! current-line Nil))
                      (True
                       (cell:push! current-line c)))
                Unit)
              chars)
             (if (not (list:null? (cell:read current-line)))
                 (cell:push! lines (the String (into (list:reverse (cell:read current-line)))))
                 (list:reverse (cell:read lines)))))))

  (declare read-file-lines ((Into :a String) => :a -> (Result FileError (List String))))
  (define (read-file-lines path-string)
    "Reads a file into lines, given a pathname string."
    (with-open-file (Input (into path-string))
      (fn (stream)
        (%read-file-lines stream)))))

;;;
;;; Streamable definitions for Integer types
;;;

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defmacro define-streamable (type)
    `(define-instance (Streamable ,type)
       (define (open stream-options)
         (let ((t (types:runtime-repr (the (types:Proxy ,type) types:Proxy))))
           (%open stream-options t)))
       (define (close fs)
         (%close fs))
       (define (read fs)
         (read-byte fs))
       (define (read-sequence fs)
         (%read-sequence fs))
       (define (write fs data)
         (write-byte fs data))
       (define (write-sequence fs sequence)
         (%write-sequence fs sequence)))))

(coalton-toplevel
  (define-streamable IFix)
  (define-streamable UFix)
  (define-streamable I8)
  (define-streamable U8)
  (define-streamable I16)
  (define-streamable U16)
  (define-streamable I32)
  (define-streamable U32)
  (define-streamable I64)
  (define-streamable U64))

;;; some examples/tests
(coalton-toplevel
  
  (define (test1)
    (with-open-file (Input "test.txt")
      (fn (stream)
        (read-char stream))))
  
  (declare test2 (Unit -> (Result FileError Char)))
  (define (test2)
    (with-open-file (Input "test.txt")
      (fn (stream)
        (read stream))))

  (declare test3 (Unit -> (Result FileError U8)))
  (define (test3)
    (with-open-file (Input "test.txt")
      (fn (stream)
        (read stream))))

  (define (test4)
    (res:unwrap-or-error
     (with-open-file (Input "test.txt")
       (fn (stream)
         (read-char stream)))))

    (define (test5)
    (res:unwrap-or-error
     (with-open-file (Input "test.t")
       (fn (stream)
         (read-char stream))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/FILE")
