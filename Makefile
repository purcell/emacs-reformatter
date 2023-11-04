EMACS ?= emacs

# A space-separated list of required package names
DEPS =

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(DEPS)) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

all: compile package-lint test clean-elc

package-lint:
	${EMACS} -Q --eval $(subst PACKAGES,package-lint,${INIT_PACKAGES}) -batch -f package-lint-batch-and-exit reformatter.el

test:
	${EMACS} -Q --eval $(subst PACKAGES,${DEPS},${INIT_PACKAGES}) -batch -l reformatter.el -l reformatter-tests.el -f ert-run-tests-batch-and-exit

compile: clean-elc
	${EMACS} -Q --eval $(subst PACKAGES,${DEPS},${INIT_PACKAGES}) -L . -batch -f batch-byte-compile *.el

clean-elc:
	rm -f f.elc

.PHONY:	all compile clean-elc package-lint
