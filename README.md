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
-   stdin: File path for program input. @=TODO=: region/buffer as stdin.
-   lisp-error: If non-nil, signal Lisp errors, else return Lisp error object.
-   namespace: A symbol or string prefixed for anaphoric \`subp-with' bindings.

The following keywords apply to asynchronous sub processes:

-   async: When non-nil, execute `PROGRAM` asynchronously.
-   callback: A function called with at least one arg. Implies :async t.
-   cb-args: Additional args to pass to the :callback function
-   stop: When non-nil, return a stopped process object.

Return a list of form (`EXIT` `STDOUT` `STDERR` :=PROPS=&#x2026;) for synchrous processses. Return a process object for asynchronous processes.

```emacs-lisp
(subp "date")

;; (0 "Sun Aug 27 07:20:10 PM EDT 2023
;; " nil)

```

```emacs-lisp
(subp "date -R")

;; (0 "Sun, 27 Aug 2023 19:20:10 -0400
;; " nil)

```

```emacs-lisp
(subp "date -x")

;; (1 nil "/usr/bin/date: invalid option -- 'x'
;; Try '/usr/bin/date --help' for more information.
;; ")

```

```emacs-lisp
(subp '("bash" "-c" "sleep 2; date"))

;; (0 "Sun Aug 27 07:20:12 PM EDT 2023
;; " nil)

```

```emacs-lisp
(subp '("bash" "-c" "sleep 2; date") :callback #'identity)

;; #<process subp>

```


## subp-with

`(subp-with ARGS &rest BODY)`

Execute `BODY` in \`subp-with-result' of calling \`subp' with `ARGS`.

```emacs-lisp
(subp-with "date" stdout)

;; "Sun Aug 27 07:20:12 PM EDT 2023
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

;; "Sun Aug 27 07:20:12 PM EDT 2023
;; "

```

```emacs-lisp
(subp-cond "date -x" (success stdout) (failure stderr))

;; "/usr/bin/date: invalid option -- 'x'
;; Try '/usr/bin/date --help' for more information.
;; "

```