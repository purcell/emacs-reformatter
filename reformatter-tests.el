;;; reformatter-tests.el --- Test suite for reformatter  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords:

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

;; Just a few basic regression tests

;;; Code:

(require 'reformatter)
(require 'ert)

(defgroup reformatter-tests nil "Reformatter tests" :group 'test)

;; We use `shfmt' because it can operate in a few modes

;; Pure stdin/stdout
(reformatter-define reformatter-tests-shfmt-stdio
  :program "shfmt"
  :args nil
  :mode nil)

(ert-deftest reformatter-tests-pure-stdio-no-args ()
  (with-temp-buffer
    (insert "[  foo  ] && echo yes\n")
    (reformatter-tests-shfmt-stdio-buffer)
    (should (equal "[ foo ] && echo yes\n" (buffer-string)))))

;; Read from stdin/stdout
(reformatter-define reformatter-tests-shfmt-tempfile-in-stdout
  :program "shfmt"
  :stdin nil
  :args (list input-file))

(ert-deftest reformatter-tests-tempfile-in-stdout ()
  (with-temp-buffer
    (insert "[  foo  ] && echo yes\n")
    (reformatter-tests-shfmt-tempfile-in-stdout-buffer)
    (should (equal "[ foo ] && echo yes\n" (buffer-string)))))

;; Same as `reformatter-tests-shfmt-tempfile-in-stdout', but with a
;; slash in the symbol name.
(reformatter-define reformatter-tests-tempfile/with-slash-in-symbol-name
  :program "shfmt"
  :stdin nil
  :args (list input-file))

(ert-deftest reformatter-tests-tempfile-with-slash-in-symbol-name ()
  (with-temp-buffer
    (insert "[  foo  ] && echo yes\n")
    (reformatter-tests-tempfile/with-slash-in-symbol-name-buffer)
    (should (equal "[ foo ] && echo yes\n" (buffer-string)))))

;; Modify a file in place
(reformatter-define reformatter-tests-shfmt-in-place
  :program "shfmt"
  :stdin nil
  :stdout nil
  :args (list "-w" input-file))

(ert-deftest reformatter-tests-tempfile-in-place ()
  (with-temp-buffer
    (insert "[  foo  ] && echo yes\n")
    (reformatter-tests-shfmt-in-place-buffer)
    (should (equal "[ foo ] && echo yes\n" (buffer-string)))))


(provide 'reformatter-tests)
;;; reformatter-tests.el ends here
