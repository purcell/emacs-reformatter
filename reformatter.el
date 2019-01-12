;;; reformatter.el --- Define commands which run reformatters on the current buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience, tools
;; Homepage: https://github.com/purcell/reformatter.el
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0

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

;; This library allows elisp authors to easily provide an idiomatic
;; command to reformat the current buffer using a command-line
;; program, together with an optional minor mode which can apply this
;; command automatically on save.

;; In its initial release it supports only reformatters which read
;; from stdin and write to stdout, but a more versatile interface will
;; be provided as development continues.

;; As an example, let's define a reformat command that applies the
;; "dhall format" command.  We'll assume here that we've already defined a
;; variable `dhall-command' which holds the string name or path of the
;; dhall executable:

;;     (reformatter-define dhall-format
;;       :program dhall-command
;;       :args '("format"))

;; The `reformatter-define' macro expands to code which generates both
;; the `dhall-format' interactive command and a local minor mode
;; called `dhall-format-on-save-mode'

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

;;     ;;;###autoload (autoload 'dhall-format "current-file" nil t)
;;     ;;;###autoload (autoload 'dhall-format-on-save-mode "current-file" nil t)

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'ansi-color)

;;;###autoload
(cl-defmacro reformatter-define (name &key program args (mode t) lighter keymap)
  "Define a reformatter command with NAME.

When called, the reformatter will use PROGRAM and any ARGS to
reformat the current buffer.  The contents of the buffer will be
passed as standard input to the reformatter, which should output
them to standard output.  A nonzero exit code will be reported as
failure, and the output of the command to standard error will be
displayed to the user.

The macro accepts the following keyword arguments:

:program (required)

  Provides a form which should evaluate to a string at runtime,
  e.g. a literal string, or the name of a variable which holds
  the program path.

:args

  If provided, this is a form which evaluates to a list of
  strings at runtime.  Default is the empty list.

:mode

  Unless nil, also generate a minor mode that will call the
  reformatter command from `before-save-hook' when enabled.
  Default is t.

:lighter

  If provided, this is a mode lighter string which will be used
  for the \"-on-save\" minor mode.  It should have a leading
  space.  Default is to use no lighter.

:keymap

  If provided, this is the symbol name of the \"-on-save\" mode's
  keymap, which you must declare yourself.  Default is no keymap.
"
  (declare (indent defun))
  (cl-assert (symbolp name))
  (cl-assert program)
  (let ((minor-mode-form
         (when mode
           (let ((on-save-mode-name (intern (format "%s-on-save-mode" name)))
                 (lighter-name (intern (format "%s-on-save-mode-lighter" name))))
             `(progn
                (defcustom ,lighter-name ,lighter
                  ,(format "Mode lighter for `%s'." on-save-mode-name)
                  :type 'string)
                (define-minor-mode ,on-save-mode-name
                  ,(format "When enabled, call `%s' when this buffer is saved." name)
                  nil
                  :global nil
                  :lighter ,lighter-name
                  :keymap ,keymap
                  (if ,on-save-mode-name
                      (add-hook 'before-save-hook ',name nil t)
                    (remove-hook 'before-save-hook ',name t))))))))
    `(progn
       (defun ,name (&optional display-errors)
         "Reformats the current buffer.
When called interactively, or with prefix argument
DISPLAY-ERRORS, shows a buffer if the formatting fails."
         (interactive "p")
         (message "Formatting buffer")
         (let* ((err-file (make-temp-file ,(symbol-name name)))
                (out-file (make-temp-file ,(symbol-name name)))
                (coding-system-for-read 'utf-8)
                (coding-system-for-write 'utf-8))
           (unwind-protect
               (let* ((error-buffer (get-buffer-create ,(format "*%s errors*" name)))
                      (retcode
                       (apply 'call-process-region (point-min) (point-max) ,program
                              nil (list (list :file out-file) err-file)
                              nil
                              ,args)))
                 (with-current-buffer error-buffer
                   (let ((inhibit-read-only t))
                     (insert-file-contents err-file nil nil nil t)
                     (ansi-color-apply-on-region (point-min) (point-max)))
                   (special-mode))
                 (if (eq retcode 0)
                     (progn
                       (insert-file-contents out-file nil nil nil t)
                       ;; In future this might be made optional, or a user-provided
                       ;; ":after" form could be inserted for execution
                       (whitespace-cleanup))
                   (if display-errors
                       (display-buffer error-buffer)
                     (message ,(concat (symbol-name name) " failed: see %s") (buffer-name error-buffer)))))
             (delete-file err-file)
             (delete-file out-file))))
       ,minor-mode-form)))


(provide 'reformatter)
;;; reformatter.el ends here
