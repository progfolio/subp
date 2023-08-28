;;; subp.el --- Elisp library for working with sub-processes  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/subp
;; Created: Aug 22, 2023
;; Keywords: lisp, convenience
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; @TODO sequences of async subps

;;

;;; Code:
(eval-when-compile (require 'subr-x))
(require 'cl-lib) ;;@MAYBE: implement without using `cl-loop'

(defconst subp--stderr-file
  (expand-file-name (format "subp-stderr-%s" (emacs-pid)) temporary-file-directory)
  "File for storing processes' stderr.")

(defun subp--delete-stderr-file ()
  "Remove `subp--stderr-file'."
  (when (and (boundp 'subp-process--stderr) (file-exists-p subp-process--stderr))
    (delete-file subp-process--stderr)))

;;@TODO: gate behind user option
(add-hook 'kill-emacs-hook #'subp--delete-stderr-file)

(defun subp--ensure-list (object)
  "Return OBJECT as a list."
  (declare (side-effect-free error-free))
  (if (listp object) object (list object)))

(defun subp-resignal (error)
  "Resignal ERROR object."
  (signal (car error) (cdr error)))

(defun subp--declared-option (options option &optional default)
  "Return declared OPTION from OPTIONS or DEFAULT."
  (if-let ((declared (plist-member options option))) (cadr declared) default))

(defun subp--normalize-program-args (program)
  "Return normalized list of  PROGRAM."
  (let ((args (if (consp program) program (split-string program " " 'omit-nulls))))
    (setq program (pop args))
    (when (string-match-p "/" program) (setq program (expand-file-name program)))
    (cons program args)))

(defun subp--process (program options errbuff buffer-p)
  "Return stopped PROGRAM sub-process with OPTIONS.
Standard error is associated with ERRBUFF.
If BUFFER-P is non-nil dedicate a buffer to sub-proccess output."
  (let ((p (make-process
            :name (subp--declared-option options :name "subp")
            :buffer (when buffer-p (generate-new-buffer " subp-stdout"))
            :command (subp--normalize-program-args program)
            :noquery (subp--declared-option options :noquery t)
            :connection-type (subp--declared-option options :connection-type 'pipe)
            ;;Must be a buffer even if user wants string due to underlying API.
            :stderr errbuff)))
    ;;`singal-stop' seemed likes a better choice, but it sends SIGSTP, which can be ignored.
    ;; SIGSTOP shouldn't be ignored, and sentinel hasn't been isntalled yet.
    (signal-process p 'SIGSTOP)
    (process-put p :subp-options options)
    p))

(defun subp--process-option (process key)
  "Return KEY's value on PROCESS :subp-options."
  (plist-get (process-get process :subp-options) key))

(defun subp--concat-filter (key)
  "Return process filter for process KEY."
  (lambda (process output)
    (process-put process key (concat (process-get process key) output))))

(defun subp--async-process-sentinel (callback options errbuff)
  "Return async process sentinel for CALLBACK with OPTIONS and ERRBUFF."
  (lambda (process _)
    (when (and (memq (process-status process) '(exit failed signal)) callback)
      (apply callback (if (process-get process :latep)
                          (list 'timeout nil nil)
                        (list (process-exit-status process)
                              (if (eq (subp--process-option process :stdout) 'buffer)
                                  (process-buffer process)
                                (process-get process :stdout))
                              (if (eq (subp--process-option process :stderr) 'buffer)
                                  errbuff
                                (process-get process :stderr))
                              (subp--process-option process :props)))
             (plist-get options :cb-args)))))

(defun subp--timeout-process (process)
  "Timeout PROCESS."
  (process-put process :latep t)
  (kill-process process))

(defun subp--async (program callback &rest options)
  "Eval CALLBACK  with results of async PROGRAM with OPTIONS."
  (let* ((errbuff (generate-new-buffer " subp-stderr"))
         (stdout-buffer-p (eq (plist-get options :stdout) 'buffer))
         (process (subp--process program options errbuff stdout-buffer-p))
         (errproc (get-buffer-process errbuff)))
    (unless stdout-buffer-p (set-process-filter process (subp--concat-filter :stdout)))
    (set-process-sentinel process (subp--async-process-sentinel callback options errbuff))
    (unless (eq (plist-get options :stderr) 'buffer)
      (set-process-filter errproc (subp--concat-filter :stderr)))
    (when-let ((timeout (plist-get options :timeout)))
      (process-put process :timer
                   (run-at-time timeout nil #'subp--timeout-process process)))
    (if (plist-get options :stop) process (continue-process process))))

(defun subp--validate-args (program options)
  "Validate PROGRAM and OPTIONS."
  (or program (signal 'wrong-type-argument '(nil (stringp (stringp...)))))
  (when options (unless (keywordp (car options))
                  (signal 'wrong-type-argument (list (car options) 'keywordp)))))

(defun subp--stdout (bufferp)
  "Return stdout string. If BUFFERP is non-nil return `current-buffer'."
  (cond ((= (buffer-size) 0) (and (kill-buffer) nil))
        (bufferp (current-buffer))
        (t (prog1 (buffer-substring-no-properties (point-min) (point-max))
             (kill-buffer)))))

(defun subp--stderr (bufferp)
  "Return `subp--stderr-file' stdout string. If BUFFERP is non-nil return buffer."
  (unless (= (file-attribute-size (file-attributes subp--stderr-file)) 0)
    (with-current-buffer (generate-new-buffer " subp-stderr")
      (insert-file-contents subp--stderr-file)
      (if bufferp (current-buffer)
        (prog1 (buffer-substring-no-properties (point-min) (point-max))
          (kill-buffer))))))

(defun subp (program &rest options)
  "Run PROGRAM synchronously with OPTIONS.
PROGRAM is a string or a list of form (PROGRAM ARGS...).
If PROGRAM contains spaces, it will be split on spaces to supply program args.
OPTIONS is a may be any of the key value pairs:
  - stdout: `buffer` to return a buffer, other values return a string.
  - stderr: same as above.
  - stdin: File path for program input. @TODO: region/buffer as stdin.
  - lisp-error: If non-nil, signal Lisp errors, else return Lisp error object.
  - namespace: A symbol or string prefixed for anaphoric `subp-with' bindings.

The following keywords apply to asynchronous sub processes:

  - async: When non-nil, execute PROGRAM asynchronously.
  - callback: A function called with at least one arg. Implies :async t.
  - cb-args: Additional args to pass to the :callback function
  - stop: When non-nil, return a stopped process object.
Return a list of form (EXIT STDOUT STDERR :PROPS...) for synchrous processses.
Return a process object for asynchronous processes."
  (condition-case err
      (let ((callback (plist-get options :callback)))
        (subp--validate-args program options)
        (if (or callback (plist-get options :async))
            (apply #'subp--async program callback options)
          (let* ((normalized (subp--normalize-program-args program))
                 (program (car normalized))
                 (args (cdr normalized)))
            (with-current-buffer (generate-new-buffer " subp-stdout")
              (append (list (apply #'call-process program (plist-get options :stdin)
                                   (list t subp--stderr-file) nil args)
                            (subp--stdout (eq (plist-get options :stdout) 'buffer))
                            (subp--stderr (eq (plist-get options :stderr) 'buffer)))
                      (subp--ensure-list  (plist-get options :props)))))))
    (error (if (plist-get options :lisp-error) (subp-resignal err) err))))

(defun subps (programs callback &rest options)
  "Eval CALLBACK with result of async PROGRAMS.
Return list of PROGRAMS subprocesses.
OPTIONS @TODO: accept options."
  (cl-loop
   with required with optional
   with progcount = (length programs)
   with optcount = (cl-count-if (lambda (program) (plist-get (cdr-safe program) :optional))
                                programs)
   with limit = (max (- progcount optcount) 1)
   with firstp = (= optcount progcount)
   for program in programs
   for i below progcount
   for p =
   (apply #'subp (if (consp program) (car program) program)
          (append
           (when (consp (car-safe program)) (cdr program))
           (list :callback
                 (lambda (result id self)
                   (push (cons id result)
                         (if (plist-get (cdr-safe self) :optional) optional required))
                   (when (or (eq (length required) limit)
                             (and firstp (eq (length optional) limit)))
                     ;;@TODO: kill outstanding processes
                     (apply callback
                            (cl-loop with results = (append required optional)
                                     for i below (length programs)
                                     collect (alist-get i results '(declined nil nil)))
                            (plist-get options :cb-args))
                     ;;Ensure limit can't be reached after this point.
                     (setq limit -1 firstp nil)))
                 :stop t
                 :cb-args (list i program))))
   when (listp p) do ;;handle lisp-errors immediately returned result
   (push (cons i p) (if (plist-get (cdr-safe program) :optional) optional required))
   collect (if (processp p) (continue-process p) p)))

(defsubst subp--namespace-symbol (prefix name)
  "Reutrn symbol NAME with PREFIX."
  (intern (if (not prefix) name (concat prefix "-" name)))) ;;@MAYBE: Drop hyphen?

(defun subp-result-props (result)
  "Return props of RESULT."
  (nthcdr 3 result))

(defun subp-result-props-get (result key)
  "Return KEY's value from RESULT props."
  (plist-get (subp-result-props result) key))

(defmacro subp-with-result (namespace result &rest body)
  "Provide anaphoric RESULT bindings with NAMESPACE for duration of BODY.
RESULT must be an expression which evaluates to subp result.
Anaphoric bindings provided:
  result: the raw process result list
  exit: the exit code of the process
  invoked: t if process was invoked without a Lisp error
  success: t if process exited with exit code 0
  failure: t if process did not invoke or exited with a nonzero code
  err: Lisp error object
  stdout: output of stdout
  stderr: output of stderr"
  (declare (indent 1) (debug t))
  (let* ((ns (or (and namespace (if (stringp namespace) namespace (symbol-name namespace)))))
         (rsym     (subp--namespace-symbol ns "result"))
         (exit     (subp--namespace-symbol ns "exit"))
         (timeout  (subp--namespace-symbol ns "timeout"))
         (declined (subp--namespace-symbol ns "declined"))
         (invoked  (subp--namespace-symbol ns "invoked"))
         (success  (subp--namespace-symbol ns "success"))
         (failure  (subp--namespace-symbol ns "failure"))
         (err      (subp--namespace-symbol ns "err"))
         (stdout   (subp--namespace-symbol ns "stdout"))
         (stderr   (subp--namespace-symbol ns "stderr")))
    ;;@TODO: simplify bindings. Failure could be non-nil and capture reason.
    `(let* ((,rsym     ,result)
            (,exit     (car ,rsym))
            (,timeout  (eq ,exit 'timeout))
            (,declined (eq ,exit 'timeout))
            (,invoked  (or ,timeout ,declined (numberp ,exit)))
            (,success  (and (not (or ,timeout ,declined)) ,invoked (zerop ,exit)))
            (,failure  (and (not ,success) ,exit))
            (,err      (and (not (or ,timeout ,declined ,invoked)) ,rsym))
            (,stdout   (and ,invoked (nth 1 ,rsym)))
            (,stderr   (and ,invoked (nth 2 ,rsym))))
       ;; Prevent byte-compiler warnings.
       (ignore ,rsym ,exit ,timeout ,declined ,invoked
               ,success ,failure ,err ,stdout ,stderr)
       ,@body)))

(defmacro subps-with (spec &rest body)
  "Eval BODY with namespaced results of async SPEC programs."
  (declare (indent 1) (debug t))
  `(subps (list ,@(mapcar #'cadr spec)) ;;@FIX: spec quoting.
          (lambda (results)
            (thread-last
              (progn ,@body)
              ,@(cl-loop for (sym val) in spec
                         for i below (length spec)
                         for namespace =
                         (if val sym (intern (concat "subp" (number-to-string i))))
                         collect `(subp-with-result ,namespace (nth ,i results)))))))

(defmacro subp-with (args &rest body)
  "Execute BODY in `subp-with-result' of calling `subp' with ARGS."
  (declare (indent 1) (debug t))
  (let* ((args (if (consp args) args (list args)))
         (options (cdr-safe args))
         (callback (plist-get options :callback)))
    (when callback (warn "subp :callback replaced by macro BODY: %S" callback))
    (if (or (plist-get options :async) callback)
        `(subp--async ,(car args)
                      (lambda (result) (subp-with-result ,(plist-get options :namespace)
                                         result ,@body))
                      ,@options)
      `(subp-with-result ,(plist-get options :namespace) (subp ,@args) ,@body))))

(defmacro subp-cond (args &rest conditions)
  "Eval CONDITIONS in context of `subp-with' with ARGS."
  (declare (indent 1) (debug t))
  `(subp-with ,args (cond ,@conditions)))

(provide 'subp)
;;; subp.el ends here
