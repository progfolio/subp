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
;;; @TODO: async sub-processes?

;;

;;; Code:
(eval-when-compile (require 'subr-x))

(defconst subp--stderr
  (expand-file-name (format "subp-stderr-%s" (emacs-pid)) temporary-file-directory)
  "File for storing processes' stderr.")

(defun subp--delete-stderr-file ()
  "Remove `subp--stderr' file."
  (when (and (boundp 'subp-process--stderr) (file-exists-p subp-process--stderr))
    (delete-file subp-process--stderr)))

;;@TODO: gate behind user option
(add-hook 'kill-emacs-hook #'subp--delete-stderr-file)

(defun subp (program &rest options)
  "Run PROGRAM synchronously with OPTIONS.
PROGRAM is a string or a list of form (PROGRAM ARGS...).
If PROGRAM contains spaces, it will be split on spaces to supply program args.
OPTIONS is a may be any of the key value pairs:
  - stdout-type: `buffer` to return a buffer, other values return a string.
  - stderr-type: same as above.
  - stdin: File path for program input.
Return a list of form: (EXITCODE STDOUT STDERR)."
  (or program (signal 'wrong-type-argument '(nil (stringp (stringp...)))))
  (when options (unless (keywordp (car options))
                  (signal 'wrong-type-argument (list (car options) 'keywordp))))
  (let ((args (if (consp program) program (split-string program " " 'omit-nulls))))
    (setq program (pop args))
    (when (string-match-p "/" program) (setq program (expand-file-name program)))
    (with-current-buffer (generate-new-buffer " subp-stdout")
      (list (apply #'call-process program (plist-get options :stdin)
                   (list t subp--stderr) nil args)
            (cond ((= (buffer-size) 0) (and (kill-buffer) nil))
                  ((eq (plist-get options :stdout-type) 'buffer) (current-buffer))
                  (t (prog1 (buffer-substring-no-properties (point-min) (point-max))
                       (kill-buffer))))
            (unless (= (file-attribute-size (file-attributes subp--stderr)) 0)
              (with-current-buffer (generate-new-buffer " subp-stderr")
                (insert-file-contents subp--stderr)
                (if (eq (plist-get options :stderr-type) 'buffer)
                    (current-buffer)
                  (prog1 (buffer-substring-no-properties (point-min) (point-max))
                    (kill-buffer)))))))))

(defmacro subp-with-result (result &rest body)
  "Provide anaphoric RESULT bindings for duration of BODY.
RESULT must be an expression which evaluates to a list of form:
  (EXITCODE STDOUT STDERR)
Anaphoric bindings provided:
  result: the raw process result list
  exit: the exit code of the process
  success: t if process exited with exit code 0
  failure: t if process did not invoke or exited with a nonzero code
  stdout: output of stdout
  stderr: output of stderr"
  (declare (indent 1) (debug t))
  `(let* ((result ,result)
          (exit    (car result))
          (success (zerop exit))
          (failure (not success))
          (stdout  (nth 1 result))
          (stderr  (nth 2 result)))
     ;; Prevent byte-compiler warnings.
     (ignore result exit success failure stdout stderr)
     ,@body))

(defmacro subp-with (args &rest body)
  "Execute BODY in `subp-with-result' of calling `subp' with ARGS."
  (declare (indent 1) (debug t))
  `(subp-with-result (subp ,@(if (listp args) args (list args))) ,@body))

(defun subp-output (program &rest args)
  "Return output of running PROGRAM with ARGS.
Signal an error if the command returns a nonzero exit code."
  (subp-with-result (apply #'subp program args)
    (if success (concat stdout stderr) ; Programs may exit normally and print to stderr
      (error "%s exited with code %s: %s" program (car result) stderr))))

(defmacro subp-cond (args &rest conditions)
  "Eval CONDITIONS in context of `subp-with' with ARGS."
  (declare (indent 1) (debug t))
  `(subp-with ,args (cond ,@conditions)))

(provide 'subp)
;;; subp.el ends here
