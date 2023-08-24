# subp.el: elisp sub-process library


# Installation

```emacs-lisp
(elpaca (subp :host github :repo "progfolio/subp"))
```


# Examples


## subp

`(subp PROGRAM &rest OPTIONS)`

Run `PROGRAM` synchronously with `OPTIONS`. `PROGRAM` is a string or a list of form (`PROGRAM` `ARGS`&#x2026;). If `PROGRAM` contains spaces, it will be split on spaces to supply program args. `OPTIONS` is a may be any of the key value pairs:

-   stdout: \`buffer\` to return a buffer, other values return a string.
-   stderr: same as above.
-   stdin: File path for program input.

Return a list of form: (`EXITCODE` `STDOUT` `STDERR`).

```emacs-lisp
(subp "date")

;; (0 "Wed Aug 23 09:24:43 PM EDT 2023
;; " nil)

```

```emacs-lisp
(subp "date -R")

;; (0 "Wed, 23 Aug 2023 21:24:43 -0400
;; " nil)

```

```emacs-lisp
(subp "date -x")

;; (1 nil "/usr/bin/date: invalid option -- 'x'
;; Try '/usr/bin/date --help' for more information.
;; ")

```


## subp-with

`(subp-with ARGS &rest BODY)`

Execute `BODY` in \`subp-with-result' of calling \`subp' with `ARGS`.

```emacs-lisp
(subp-with "date" stdout)

;; "Wed Aug 23 09:24:43 PM EDT 2023
;; "

```

```emacs-lisp
(subp-with "date -R" exit)

;; 0

```

```emacs-lisp
(subp-with "date -x" stderr)

;; "/usr/bin/date: invalid option -- 'x'
;; Try '/usr/bin/date --help' for more information.
;; "

```


## subp-cond

`(subp-cond ARGS &rest CONDITIONS)`

Eval `CONDITIONS` in context of \`subp-with' with `ARGS`.

```emacs-lisp
(subp-cond "date" (success stdout) (failure stderr))

;; "Wed Aug 23 09:24:43 PM EDT 2023
;; "

```

```emacs-lisp
(subp-cond "date -x" (success stdout) (failure stderr))

;; "/usr/bin/date: invalid option -- 'x'
;; Try '/usr/bin/date --help' for more information.
;; "

```