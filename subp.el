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


(defun subp (program &rest options)
  "Run PROGRAM synchronously with OPTIONS.
PROGRAM is a string or a list of form (PROGRAM ARGS...).
If PROGRAM contains spaces, it will be split on spaces to supply program args.
OPTIONS is a may be any of the key value pairs:
  - stdout: `buffer` to return a buffer, other values return a string.
  - stderr: same as above.
  - stdin: File path for program input.
  - lisp-error: If non-nil, signal Lisp errors, else return Lisp error object.
Return a list of form: (EXITCODE STDOUT STDERR)."
  (condition-case err
      (progn
        (or program (signal 'wrong-type-argument '(nil (stringp (stringp...)))))
        (when options (unless (keywordp (car options))
                        (signal 'wrong-type-argument (list (car options) 'keywordp))))
        (let ((callback (plist-get options :callback)))
          (if (or callback (plist-get options :async))
              ;;@TODO: lisp-error treatment when async?
              (apply #'subp-async program callback options)
            (let ((args (if (consp program) program (split-string program " " 'omit-nulls))))
              (setq program (pop args))
              (when (string-match-p "/" program) (setq program (expand-file-name program)))
              (with-current-buffer (generate-new-buffer " subp-stdout")
                (list (apply #'call-process program (plist-get options :stdin)
                             (list t subp--stderr) nil args)
                      (cond ((= (buffer-size) 0) (and (kill-buffer) nil))
                            ((eq (plist-get options :stdout) 'buffer) (current-buffer))
                            (t (prog1 (buffer-substring-no-properties (point-min) (point-max))
                                 (kill-buffer))))
                      (unless (= (file-attribute-size (file-attributes subp--stderr)) 0)
                        (with-current-buffer (generate-new-buffer " subp-stderr")
                          (insert-file-contents subp--stderr)
                          (if (eq (plist-get options :stderr) 'buffer)
                              (current-buffer)
                            (prog1 (buffer-substring-no-properties (point-min) (point-max))
                              (kill-buffer)))))))))))
    (error (if (plist-get options :lisp-error) (signal (car err) (cdr err)) err))))
(defsubst subp--namespace-symbol (prefix name)
  "Reutrn symbol NAME with PREFIX."
  (intern (if (not prefix) name (concat prefix "-" name)))) ;;@MAYBE: Drop hyphen?

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
  (let* ((ns (or (and namespace (if (stringp namespace) namespace (symbol-name namespace)))
                 (subp-result-props-get result :namespace)))
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

(defmacro subp--if-async (then else)
  "Return THEN if ARGS request aysnc subprocess. Otherwise return ELSE."
  (declare (indent 1))
  `(let* ((args (if (consp args) args (list args)))
          (options (cdr-safe args))
          (callback (plist-get options :callback)))
     (when callback (warn "subp :callback replaced by macro BODY: %S" callback))
     (if (or (plist-get options :async) callback)
         `(subp-async ,(car args) ,,then ,@options)
       ,else)))

(defmacro subp-with (args &rest body)
  "Execute BODY in `subp-with-result' of calling `subp' with ARGS."
  (declare (indent 1) (debug t))
  (subp--if-async `(lambda (result) (subp-with-result result ,@body))
    `(subp-with-result (subp ,@args ,@options) ,@body)))

(defmacro subp-cond (args &rest conditions)
  "Eval CONDITIONS in context of `subp-with' with ARGS."
  (declare (indent 1) (debug t))
  (subp--if-async
      `(lambda (result) (subp-with-result result (cond ,@conditions)))
    `(subp-with ,args (cond ,@conditions))))

(provide 'subp)
;;; subp.el ends here
