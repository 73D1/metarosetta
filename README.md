# metarosetta

Convention-based textual meta-translation for Emacs.

# Introduction

Metarosetta is an Emacs package which introduces support for so-called metalanguage expressions.

Like regular expressions, but in plain English, these enable definitions of arbitrary textual conventions to capture and parse through matching textual snippets from within the Emacs editing environment.

For example, the following metalanguage expression

``` emacs-lisp
(literal ";;") (WORD as status to lowercase) (contextual paragraph as task) (":" prefixed words as tags)
```

will match the following line

``` emacs-lisp
;; TODO Test for edge cases :test :important
```

and parse out all the targeted semantic data, as defined by the metalanguage expression, and structure it in `alist` form.
In this example, `"Test for edge cases"` would be the value of the `task` key, `("test" "important")` the value for `tags` and so on.

The idea behind metalanguage expressions is to make writing conventions inherently actionable and automatically machine-digestible, be it from comments within code, org-mode entries, or any other form of conventional markup in any type of code or prose.

Furthermore, metalanguage expressions are designed to be two-directional. So any matched text can be updated in-place automatically by providing a compatible data structure.

Continuing with the example, providing `(status . "waiting")` to the match in context will update the text to the following

``` emacs-lisp
;; WAITING Test for edge cases :test :important
```

Ultimately, metalanguage expressions are best used when paired.

I.e., any two expressions can automatically sync match data back and forth provided that their output data structures are compatible.

By pairing the above expression with the following

``` emacs-lisp
(literal "-") ("[" prefixed "]" suffixed word as status) (contextual paragraph as task) ("#" prefixed words as tags)
```

will yield the following line in the designated output file

``` markdown
- [todo] Test for edge cases #test #important
```

Of course, Metarosetta provides synchronization mechanics where a change in either of the two matches will get automatically propagated to the other.

Hopefully, this will make all our scattered `TODO` comments a bit more meaningful. :wink:

# Installation

## Dependencies

# Configuration

# Metalanguage Expression Examples

See the [Demos](./metarosetta.org#demos) section within the source `org` file.

# Metalanguage Specification

See the [Metalanguage Specification](./metarosetta.org#language-specification) section within the source `org` file.

# Acknowledgements
