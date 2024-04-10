# Coalton System and IO Tools

## Introduction

The `coalton-library/io` and `coalton-library/system` packages provide access to many operations.

## The Current Working Directory

Coalton finds files and subdirectories relative to a current working directory.

Here is an example of basic current working directory navigation:

```

COALTON-LIBRARY/IO> (coalton (cwd))
"/Home/"

COALTON-LIBRARY/IO> (coalton (chdir "lisp"))
Current Working Directory: /Home/lisp/
COALTON::UNIT/UNIT

COALTON-LIBRARY/IO> (coalton (cwd))
"/Home/lisp/"

COALTON-LIBRARY/IO> (coalton (back-up))
Current Working Directory: /Home/
COALTON::UNIT/UNIT

COALTON-LIBRARY/IO> (coalton (system-relative-cwd "coalton"))
Current Working Directory: /Home/lisp/coalton/
COALTON::UNIT/UNIT

COALTON-LIBRARY/IO> (coalton (chdir "../.."))
Current Working Directory: /Home/
COALTON::UNIT/UNIT

```

Now an example of exploring directories:

```
COALTON-LIBRARY/IO> (coalton (system-relative-cwd "coalton"))
Current Working Directory: /Home/lisp/coalton/
COALTON::UNIT/UNIT

COALTON-LIBRARY/IO> (chdir "examples")
Current Working Directory: /Home/lisp/coalton/examples/
COALTON::UNIT/UNIT

COALTON-LIBRARY/IO> (coalton (pwd))
"
Contents of Directory /Home/lisp/coalton/examples/:

Subdirectories:
/Home/lisp/coalton/examples/coalton-testing-example-project/
/Home/lisp/coalton/examples/quil-coalton/
/Home/lisp/coalton/examples/small-coalton-programs/
/Home/lisp/coalton/examples/thih/

Files:
/Home/lisp/coalton/examples/README.md
"
COALTON-LIBRARY/IO> (coalton (chdir "small-coalton-programs/src"))
Current Working Directory: /Home/lisp/coalton/examples/small-coalton-programs/src/
COALTON::UNIT/UNIT

COALTON-LIBRARY/IO> (coalton (cwd-files))
(#P"/Home/lisp/coalton/examples/small-coalton-programs/src/brainfold.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/diff.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/freecalc.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/microbench1.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/package.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/primes.lisp")

COALTON-LIBRARY/IO> (coalton (file-exists-p "brainfold.lisp"))
COMMON-LISP:T

COALTON-LIBRARY/IO> (coalton (file-exists-p "new-example-never-before-seen.lisp"))
COMMON-LISP:NIL

COALTON-LIBRARY/IO> (coalton (copy-file "brainfold.lisp" "foldbrain.lisp"))
File copied to: /Home/lisp/coalton/examples/small-coalton-programs/src/foldbrain.lisp
COALTON::UNIT/UNIT

COALTON-LIBRARY/IO> (coalton (cwd-files))
(#P"/Home/lisp/coalton/examples/small-coalton-programs/src/brainfold.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/diff.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/foldbrain.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/freecalc.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/microbench1.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/package.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/primes.lisp")

COALTON-LIBRARY/IO> (coalton (delete-file "foldbrain.lisp"))
File deleted: foldbrain.lisp
COALTON::UNIT/UNIT

COALTON-LIBRARY/IO> (coalton (cwd-files))
(#P"/Home/lisp/coalton/examples/small-coalton-programs/src/brainfold.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/diff.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/freecalc.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/microbench1.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/package.lisp"
 #P"/Home/lisp/coalton/examples/small-coalton-programs/src/primes.lisp")

COALTON-LIBRARY/IO> (coalton (delete-file "foldbrain.lisp"))
File not found: foldbrain.lisp
COALTON::UNIT/UNIT

```

## System variables

System variables are gathered using functions prefixed with `which`. For example:

```
COALTON-LIBRARY/SYSTEM> (coalton (which-os))
:OS-MACOSX

COALTON-LIBRARY/SYSTEM> (coalton (which-architexture))
:X64
```