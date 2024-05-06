(in-package #:coalton-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton:coalton-toplevel

  (coalton:declare ensure-empty (Unit -> (classes:Result FileError Boolean)))
  (coalton:define (ensure-empty)
    "Ensures that the "
    (do
     (let path = (file:system-relative-pathname "coalton" "tests/file-tests/"))
     (exists <- (file:exists? path))
      (dir <- (file:directory? path))
      (classes:pure (coalton:and (coalton:not exists)
                                 (coalton:not dir)))))
  
  (coalton:define (make-directory)
    (do
     (let path = (file:system-relative-pathname "coalton" "tests/file-tests/"))
     (file:create-directory path)
      (file:exists? (file:system-relative-pathname "coalton" "tests/file-tests/"))
      (file:directory? ))
    (coalton-library/current-working-directory:with-system-cwd "coalton"
      (coalton:do
       (coalton-library/current-working-directory:down "tests/")
       (coalton-library/current-working-directory:mkdir "file-test-temp/")
        (coalton-library/current-working-directory:down "file-test-temp/")
        (coalton-library/current-working-directory:cwd))))

  #+ig(coalton:define (file-writing-does-not-exist-error-test)
    (coalton-library/current-working-directory:with-system-cwd "coalton"
      (coalton:do
       (coalton-library/current-working-directory:down "tests/file-test-temp/")
       (coalton-library/current-working-directory:cwd)
        (coalton-library/current-working-directory:write "test-file.txt" coalton-library/file:SupersedeFile coalton-library/file:DoesNotExistError "If this has been written, there is an issue."))))

  #+ig(coalton:define (write-test-file)
    (coalton-library/current-working-directory:with-system-cwd "coalton"
      (coalton:do
       (coalton-library/current-working-directory:down "tests/file-test-temp/")
       (coalton-library/current-working-directory:cwd)
        (coalton-library/current-working-directory:write "test-file.txt" coalton-library/file:SupersedeFile coalton-library/file:CreateFile "Wow this works"))))

  #+ig(coalton:define (file-writing-exists-error-test)
    (coalton-library/current-working-directory:with-system-cwd "coalton"
      (coalton:do
       (coalton-library/current-working-directory:down "tests/file-test-temp/")
       (coalton-library/current-working-directory:cwd)
        (coalton-library/current-working-directory:write "test-file.txt" coalton-library/file:ExistsError coalton-library/file:CreateFile "If this has been written, there is an issue."))))

  #+ig(coalton:define (read-test-file)
    (coalton-library/classes:unwrap (coalton-library/current-working-directory:with-system-cwd "coalton"
                      (coalton:do
                       (coalton-library/current-working-directory:down "tests/file-test-temp/")
                       (coalton-library/current-working-directory:read->string "test-file.txt")))))

  #+ig(coalton:define (clear-file-test)
    (coalton-library/current-working-directory:with-system-cwd "coalton"
      (coalton:do
       (coalton-library/current-working-directory:down "tests/file-test-temp/")
       (coalton-library/current-working-directory:delete "test-file.txt"))))

  #+ig(coalton:define (file-exists-test)
    (coalton-library/current-working-directory:with-system-cwd "coalton"
      (coalton:do
       (coalton-library/current-working-directory:down "tests/file-test-temp/")
       (coalton-library/current-working-directory:exists? "file-test-temp/test-file.txt"))))
  
  #+ig(coalton:define (clear-directory-test)
    (coalton-library/current-working-directory:with-system-cwd "coalton"
      (coalton:do
       (coalton-library/current-working-directory:down "tests/")
       (coalton-library/current-working-directory:rmdir "file-test-temp/")
        )))

  #+ig(coalton:define (directory-exists-test)
    (coalton-library/current-working-directory:with-system-cwd "coalton"
      (coalton:do
       (coalton-library/current-working-directory:down "tests/")
       (coalton-library/current-working-directory:exists? "file-test-temp")))))

(deftest file-tests ()

  ;; make sure the testing environment is empty
  (is (not (coalton:coalton (file-exists-test))))
  (is (not (coalton:coalton (directory-exists-test))))

  ;; populate and work in the testing environment
  (coalton:coalton (make-directory))
  (is (coalton:coalton (directory-exists-test)))
  (signals error (coalton:coalton (file-writing-does-not-exist-error-test)))
  (coalton:coalton (write-test-file))
  (signals error (coalton:coalton (file-writing-exists-error-test)))
  (is (string= (coalton:coalton (read-test-file))
               "Wow this works"))

  ;; clear the testing environment
  (coalton:coalton (clear-file-test))
  (is (not (coalton:coalton (file-exists-test))))
  (coalton:coalton (clear-directory-test))
  (is (not (coalton:coalton (directory-exists-test)))))
