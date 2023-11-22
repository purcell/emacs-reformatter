;;; reformatter.el --- Define commands which run reformatters on the current buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience, tools
;; Homepage: https://github.com/purcell/emacs-reformatter
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0.8

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

;; This library lets elisp authors easily define an idiomatic command
;; to reformat the current buffer using a command-line program,
;; together with an optional minor mode which can apply this command
;; automatically on save.

;; By default, reformatter.el expects programs to read from stdin and
;; write to stdout, and you should prefer this mode of operation where
;; possible.  If this isn't possible with your particular formatting
;; program, refer to the options for `reformatter-define', and see the
;; examples in the package's tests.

;; As an example, let's define a reformat command that applies the
;; "dhall format" command.  We'll assume here that we've already defined a
;; variable `dhall-command' which holds the string name or path of the
;; dhall executable:

;;     (reformatter-define dhall-format
;;       :program dhall-command
;;       :args '("format"))

;; The `reformatter-define' macro expands to code which generates
;; `dhall-format-buffer' and `dhall-format-region' interactive
;; commands, and a local minor mode called
;; `dhall-format-on-save-mode'.  The :args" and :program expressions
;; will be evaluated at runtime, so they can refer to variables that
;; may (later) have a buffer-local value.  A custom variable will be
;; generated for the mode lighter, with the supplied value becoming
;; the default.

;; The generated minor mode allows idiomatic per-directory or per-file
;; customisation, via the "modes" support baked into Emacs' file-local
;; and directory-local variables mechanisms.  For example, users of
;; the above example might add the following to a project-specific
;; .dir-locals.el file:

;;     ((dhall-mode
;;       (mode . dhall-format-on-save)))

;; See the documentation for `reformatter-define', which provides a
;; number of options for customising the generated code.

;; Library authors might like to provide autoloads for the generated
;; code, e.g.:

;;     ;;;###autoload (autoload 'dhall-format-buffer "current-file" nil t)
;;     ;;;###autoload (autoload 'dhall-format-region "current-file" nil t)
;;     ;;;###autoload (autoload 'dhall-format-on-save-mode "current-file" nil t)

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'ansi-color)

(defun reformatter--make-temp-file (sym)
  "Create a temporary file whose filename is based on SYM, but with
slashes replaced by underscores.  `make-temp-file' fails
otherwise as it cannot create intermediate directories."
  (make-temp-file
   (replace-regexp-in-string "/" "_" (symbol-name sym))))

(defun reformatter--do-region (name beg end program args stdin stdout input-file exit-code-success-p display-errors)
  "Do the work of reformatter called NAME.
Reformats the current buffer's region from BEG to END using
PROGRAM and ARGS.  For args STDIN, STDOUT, INPUT-FILE,
EXIT-CODE-SUCCESS-P and DISPLAY-ERRORS see the documentation of
the `reformatter-define' macro."
  (cl-assert input-file)
  (cl-assert (functionp exit-code-success-p))
  (when (and input-file
             (buffer-file-name)
             (string= (file-truename input-file)
                      (file-truename (buffer-file-name))))
    (error "The reformatter must not operate on the current file in-place"))
  (let* ((stderr-file (reformatter--make-temp-file name))
         (stdout-file (reformatter--make-temp-file name))
         ;; Setting this coding system might not universally be
         ;; the best default, but was apparently necessary for
         ;; some hand-rolled reformatter functions that this
         ;; library was written to replace.
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (unwind-protect
        (progn
          (write-region beg end input-file nil :quiet)
          (let* ((error-buffer (get-buffer-create (format "*%s errors*" name)))
                 (retcode
                  (condition-case e
                      (apply 'call-process program
                             (when stdin input-file)
                             (list (list :file stdout-file) stderr-file)
                             nil
                             args)
                    (error e))))
            (with-current-buffer error-buffer
              (let ((inhibit-read-only t))
                (insert-file-contents stderr-file nil nil nil t)
                (unless (integerp retcode)
                  (insert (error-message-string retcode)))
                (ansi-color-apply-on-region (point-min) (point-max)))
              (special-mode))
            (if (and (integerp retcode) (funcall exit-code-success-p retcode))
                (progn
                  (save-restriction
                    ;; This replacement method minimises
                    ;; disruption to marker positions and the
                    ;; undo list
                    (narrow-to-region beg end)
                    (reformatter-replace-buffer-contents-from-file (if stdout
                                                                       stdout-file
                                                                     input-file)))
                  ;; If there are no errors then we hide the error buffer
                  (delete-windows-on error-buffer))
              (if display-errors
                  (display-buffer error-buffer)
                (message (concat (symbol-name name) " failed: see %s") (buffer-name error-buffer))))))
      (delete-file stderr-file)
      (delete-file stdout-file))))

;;;###autoload
(cl-defmacro reformatter-define (name &key program args (mode t) (stdin t) (stdout t) input-file lighter keymap group (exit-code-success-p 'zerop))
  "Define a reformatter command with NAME.

When called, the reformatter will use PROGRAM and any ARGS to
reformat the current buffer.  The contents of the buffer will be
passed as standard input to the reformatter, which should output
them to standard output.  A nonzero exit code will be reported as
failure, and the output of the command to standard error will be
displayed to the user.

The macro accepts the following keyword arguments:

PROGRAM (required)

  Provides a form which should evaluate to a string at runtime,
  e.g. a literal string, or the name of a variable which holds
  the program path.

ARGS

  Command-line arguments for the program.  If provided, this is a
  form which evaluates to a list of strings at runtime.  Default
  is the empty list.  This form is evaluated at runtime so that
  you can use buffer-local variables to influence the args passed
  to the reformatter program: the variable `input-file' will be
  lexically bound to the path of a file containing the text to be
  reformatted: see the keyword options INPUT-FILE, STDIN and
  STDOUT for more information.

STDIN

  When non-nil (the default), the program is passed the input
  data on stdin.  Set this to nil when your reformatter can only
  operate on files in place.  In such a case, your ARGS should
  include a reference to the `input-file' variable, which will be
  bound to an input path when evaluated.

STDOUT

  When non-nil (the default), the program is expected to write
  the reformatted text to stdout.  Set this to nil if your
  reformatter can only operate on files in place, in which case
  the contents of the temporary input file will be used as the
  replacement text.

INPUT-FILE

  Sometimes your reformatter program might expect files to be in
  a certain directory or have a certain file extension.  This option
  lets you handle that.

  If provided, it is a form which will be evaluated before each
  run of the formatter, and is expected to return a temporary
  file path suitable for holding the region to be reformatted.
  It must not produce the same path as the current buffer's file
  if that is set: you shouldn't be operating directly on the
  buffer's backing file.  The temporary input file will be
  deleted automatically.  You might find the functions
  `reformatter-temp-file-in-current-directory' and
  `reformatter-temp-file' helpful.

MODE

  Unless nil, also generate a minor mode that will call the
  reformatter command from `before-save-hook' when enabled.
  Default is t.

GROUP

  If provided, this is the custom group used for any generated
  modes or custom variables.  Don't forget to declare this group
  using a `defgroup' form.

LIGHTER

  If provided, this is a mode lighter string which will be used
  for the \"-on-save\" minor mode.  It should have a leading
  space.  The supplied value will be used as the default for a
  generated custom variable which specifies the mode lighter.
  Default is nil, ie. no lighter.

KEYMAP

  If provided, this is the symbol name of the \"-on-save\" mode's
  keymap, which you must declare yourself.  Default is no keymap.

EXIT-CODE-SUCCESS-P

  If provided, this is a function object callable with `funcall'
  which accepts an integer process exit code, and returns non-nil
  if that exit code is considered successful.  This could be a
  lambda, quoted symbol or sharp-quoted symbol.  If not supplied,
  the code is considered successful if it is `zerop'."
  (declare (indent defun))
  (cl-assert (symbolp name))
  (cl-assert (functionp exit-code-success-p))
  (cl-assert program)
  ;; Note: we skip using `gensym' here because the macro arguments are only
  ;; referred to once below, but this may have to change later.
  (let* ((buffer-fn-name (intern (format "%s-buffer" name)))
         (region-fn-name (intern (format "%s-region" name)))
         (minor-mode-form
          (when mode
            (let ((on-save-mode-name (intern (format "%s-on-save-mode" name)))
                  (lighter-name (intern (format "%s-on-save-mode-lighter" name))))
              `(progn
                 (defcustom ,lighter-name ,lighter
                   ,(format "Mode lighter for `%s'." on-save-mode-name)
                   :group ,group
                   :type 'string)
                 (define-minor-mode ,on-save-mode-name
                   ,(format "When enabled, call `%s' when this buffer is saved.

To enable this unconditionally in a major mode, add this mode
to the major mode's hook.  To enable it in specific files or directories,
use the local variables \"mode\" mechanism, e.g. in \".dir-locals.el\" you
might use:

     ((some-major-mode
        (mode . %s-on-save)))
 " buffer-fn-name name)
                   :global nil
                   :lighter ,lighter-name
                   :keymap ,keymap
                   :group ,group
                   (if ,on-save-mode-name
                       (add-hook 'before-save-hook ',buffer-fn-name nil t)
                     (remove-hook 'before-save-hook ',buffer-fn-name t))))))))
    `(progn
       (defun ,region-fn-name (beg end &optional display-errors)
         "Reformats the region from BEG to END.
When called interactively, or with prefix argument
DISPLAY-ERRORS, shows a buffer if the formatting fails."
         (interactive "rp")
         (let ((input-file ,(if input-file
                                input-file
                              `(reformatter--make-temp-file ',name))))
           ;; Evaluate args with input-file bound
           (unwind-protect
               (progn
                 (reformatter--do-region
                  ',name beg end
                  ,program ,args ,stdin ,stdout input-file
                  #',exit-code-success-p display-errors))
             (when (file-exists-p input-file)
               (delete-file input-file)))))

       (defun ,buffer-fn-name (&optional display-errors)
         "Reformats the current buffer.
When called interactively, or with prefix argument
DISPLAY-ERRORS, shows a buffer if the formatting fails."
         (interactive "p")
         (message "Formatting buffer")
         (,region-fn-name (point-min) (point-max) display-errors))

       ,minor-mode-form)))


(defun reformatter-replace-buffer-contents-from-file (file)
  "Replace the accessible portion of the current buffer with the contents of FILE."
  ;; While the function `replace-buffer-contents' exists in recent
  ;; Emacs versions, it exhibits pathologically slow behaviour in many
  ;; cases, and the simple replacement approach we use instead is well
  ;; proven and typically preserves point and markers to a reasonable
  ;; degree.
  (insert-file-contents file nil nil nil t))

(defun reformatter-temp-file (&optional default-extension)
  "Make a temp file re-using the current extension.
If the current file is not backed by a file, then use
DEFAULT-EXTENSION, which should not contain a leading dot.

The working directory for the command will always be the
`default-directory' of the calling buffer."
  (let ((extension (if buffer-file-name
                       (file-name-extension buffer-file-name)
                     default-extension)))
    (make-temp-file "reformatter" nil
                    (when extension
                      (concat "." extension)))))

(defun reformatter-temp-file-in-current-directory (&optional default-extension)
  "Make a temp file in the current directory re-using the current extension.
If the current file is not backed by a file, then use
DEFAULT-EXTENSION, which should not contain a leading dot."
  (let ((temporary-file-directory default-directory))
    (reformatter-temp-file default-extension)))

(provide 'reformatter)
;;; reformatter.el ends here
