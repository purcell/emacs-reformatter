[![Melpa Status](http://melpa.org/packages/reformatter-badge.svg)](http://melpa.org/#/reformatter)
[![Melpa Stable Status](http://stable.melpa.org/packages/reformatter-badge.svg)](http://stable.melpa.org/#/reformatter)
<a href="https://www.patreon.com/sanityinc"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>

# Define commands which run reformatters on the current Emacs buffer

This library allows elisp authors to easily provide an idiomatic
command to reformat the current buffer using a command-line program,
together with an optional minor mode which can apply this command
automatically on save.

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
  :lighter 'DF)
```

The `reformatter-define` macro expands to code which generates both
the `dhall-format` interactive command and a local minor mode called
`dhall-format-on-save-mode`.

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
;;;###autoload (autoload 'dhall-format "current-file" nil t)
;;;###autoload (autoload 'dhall-format-on-save-mode "current-file" nil t)
```


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

## Installation

### Manual

Ensure `reformatter.el` is in a directory on your load-path, and add
the following to your `~/.emacs` or `~/.emacs.d/init.el`:

``` lisp
(require 'reformatter)
```

### MELPA

If you're an Emacs 24 user or you have a recent version of
`package.el` you can install `reformatter` from the
[MELPA](http://melpa.org) repository. The version of
`reformatter` there will always be up-to-date.

See the command `reformatter`.

## About

Author: Steve Purcell <steve at sanityinc dot com>

Homepage: https://github.com/purcell/reformatter

<hr>

[💝 Support this project and my other Open Source work](https://www.patreon.com/sanityinc)

[💼 LinkedIn profile](https://uk.linkedin.com/in/stevepurcell)

[✍ sanityinc.com](http://www.sanityinc.com/)

[🐦 @sanityinc](https://twitter.com/sanityinc)
