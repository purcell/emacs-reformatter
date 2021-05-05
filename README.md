[![Melpa Status](http://melpa.org/packages/reformatter-badge.svg)](http://melpa.org/#/reformatter)
[![Melpa Stable Status](http://stable.melpa.org/packages/reformatter-badge.svg)](http://stable.melpa.org/#/reformatter)
[![Build Status](https://github.com/purcell/emacs-reformatter/workflows/CI/badge.svg)](https://github.com/purcell/emacs-reformatter/actions)
<a href="https://www.patreon.com/sanityinc"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>

# Define commands which run reformatters on the current Emacs buffer

This library lets elisp authors easily define an idiomatic command to
reformat the current buffer using a command-line program, together
with an optional minor mode which can apply this command automatically
on save.

By default, reformatter.el expects programs to read from stdin and
write to stdout, and you should prefer this mode of operation where
possible.  If this isn't possible with your particular formatting
program, refer to the options for `reformatter-define`, and see the
examples in the package's tests.

In its initial release it supports only reformatters which can read
from stdin and write to stdout, but a more versatile interface will
be provided as development continues.

As an example, let's define a reformat command that applies the "dhall
format" command.  We'll assume here that we've already defined a
variable `dhall-command` which holds the string name or path of the
dhall executable:

```el
(reformatter-define dhall-format
  :program dhall-command
  :args '("format")
  :lighter " DF")
```

The `reformatter-define` macro expands to code which generates
`dhall-format-buffer` and `dhall-format-region` interactive commands,
and a local minor mode called `dhall-format-on-save-mode`. The `:args`
and `:program` expressions will be evaluated at runtime, so they can
refer to variables that may (later) have a buffer-local value. A
custom variable will be generated for the mode lighter, with the
supplied value becoming the default.

The generated minor mode allows idiomatic per-directory or per-file
customisation, via the "modes" support baked into Emacs' file-local
and directory-local variables mechanisms.  For example, users of the
above example might add the following to a project-specific
`.dir-locals.el` file:

```el
((dhall-mode
   (mode . dhall-format-on-save)))
```

See the documentation for `reformatter-define`, which provides a
number of options for customising the generated code.

Library authors might like to provide autoloads for the generated
code, e.g.:

```el
;;;###autoload (autoload 'dhall-format-buffer "current-file" nil t)
;;;###autoload (autoload 'dhall-format-region "current-file" nil t)
;;;###autoload (autoload 'dhall-format-on-save-mode "current-file" nil t)
```

## Examples of usage in the wild

To find reverse dependencies, look for "Needed by" on the [MELPA page
for reformatter](https://melpa.org/#/reformatter). Here are some
specific examples:

* [dhall-mode.el](https://github.com/psibi/dhall-mode/blob/master/dhall-mode.el)
* [elm-format.el](https://github.com/jcollard/elm-mode/blob/master/elm-format.el), in `elm-mode`
* [sqlformat.el](https://github.com/purcell/sqlformat/blob/master/sqlformat.el)
* [Here](https://github.com/purcell/emacs.d/blob/14f645a9bde04498ce2b60de268c2cbafa13604a/lisp/init-purescript.el#L18-L19) is the author defining a reformatter in his own configuration

## Rationale

I contribute to a number of Emacs programming language modes and
tools, and increasingly use code reformatters in my daily work.  It's
surprisingly difficult to write robust, correct code to apply these
reformatters, given that it must consider such issues as:

* Missing programs
* Buffers not yet saved to a file
* Displaying error output
* Colorising ANSI escape sequences in any error output
* Handling file encodings correctly

With this library, I hope to help the community standardise on best
practices, and make things easier for tool authors and end users
alike.

## FAQ

### How is this different from [format-all.el](https://github.com/lassik/emacs-format-all-the-code)?

`format-all` is a very different approach: it aims to provide a single
minor mode which you then enable and configure to do the right thing
(including nothing) for all the languages you use. It even tries to
tell you how to install missing programs. It's an interesting project,
but IMO it's hard to design the configuration for such a grand unified
approach, and it can get complex. For example, you'd have to be able
to configure which of two possible reformatters you want to use for a
specific language, and to be able to do that on a per-project basis.

In contrast reformatter produces small, self-contained and separate
formatters and minor modes which all work consistently and are
individually configured. It makes it possible to replace existing
formatter code, and it's also very convenient for users to define
their own ad-hoc reformatter wrappers

## Installation

### Manual

Ensure `reformatter.el` is in a directory on your load-path, and add
the following to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(require 'reformatter)
```

### MELPA

If you're an Emacs 24 user or you have a recent version of
`package.el` you can install `reformatter` from the
[MELPA](http://melpa.org) repository. The version of
`reformatter` there will always be up-to-date.

## About

Author: Steve Purcell <steve at sanityinc dot com>

Homepage: https://github.com/purcell/emacs-reformatter

<hr>

[💝 Support this project and my other Open Source work](https://www.patreon.com/sanityinc)

[💼 LinkedIn profile](https://uk.linkedin.com/in/stevepurcell)

[✍ sanityinc.com](https://www.sanityinc.com/)

[🐦 @sanityinc](https://twitter.com/sanityinc)
