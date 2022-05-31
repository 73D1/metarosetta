# metarosetta

Convention-based textual meta-translation for Emacs.

## Introduction

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

The intent behind metalanguage expressions is to make writing conventions inherently actionable and automatically machine-digestible, be it from comments within code, org-mode entries, or any other form of conventional markup in any type of code or prose.

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

## Installation

At the moment of writing, Metarosetta isn't **yet** published on a public archive like MELPA, so `package-install` isn't available for installation.

However, by using the superb [straight.el package](https://github.com/radian-software/straight.el#the-recipe-format), it's still super simple to install the package directly from the git repo.

In cases of Doom users, it's even simpler, but still utilizing `straight`. Just add the following to your `.doom.d/packages.el`:

``` emacs-lisp
(package! metarosetta :recipe (:host github :repo "73D1/metarosetta"))
```

Finally, one can always clone this repo to the load path:

``` sh
git clone https://github.com/73D1/metarosetta ~/load/path
```

### Dependencies

Metarosetta is tested with the following configuration:

- emacs 27.2
- org 9.6
- org-ml 5.7

## Setup, Configuration and Usage

Metarosetta configuration is designed around specifically structured `org` documents called configurations.

Each configuration consists of two or more structurally-compatible metalanguage expressions, where the primary one, the so-called *root* expression, is used to catch matches throughout the Emacs editing environment, while others, the *output* expressions, are used to *transpile* the matched data to other conventions and formats.

An example configuration document is available [here](./example-config.org).

### Output Connectors

Note that for output expressions to be of any practical use, there needs to be a mechanism of persisting and managing output. For this purpose, the model of [Output Connectors](./metarosetta.org#output-connectors) was created.

The generic model of the output connector allows for various implementations of support for different format types and output scenarios, which the Metarosetta configuration is completely agnostic to.

Since many structured text formats, like `org` or `md`, share common logic to their markup syntax, Metarosetta comes bundled with an output connector implementation suitable for such output types in the form of the `metarosetta-out-to-structured-text` class.

As an example, the following Lisp expression registers a connector for markdown syntax, where new output matches are inserted as unordered list items, within sections marked as regular markdown headings.

``` emacs-lisp
(metarosetta-register-connectors (metarosetta-out-to-structured-text :syntax-type 'md
                                                                     :heading-mark "#"
                                                                     :item-mark "*"))
```

Note that the `syntax-type` symbol is used as a designating property within respective output expression configurations.

### Initialization

Once one or more configurations are defined and stored within a dedicated configuration directory, and all the required connectors are registered, Metarosetta can be initialized like so:

``` emacs-lisp
(metarosetta-load "~/path/to/configuration/directory")
```

### Matching

Finally, with all the configuration loaded, the `metarosetta-match` interactive function attempts to match the line at point within the currently active buffer against one of the configured *root* expressions.

In case of a successful match, the line is parsed, all configured outputs generated, and is assigned a unique identifier for two-way synchronization according to its configuration.

Of course, the match function can always be mapped as desired to make the action a bit more seamless.

### Synchronization

To enable automatic synchronization, it's best to add `metarosetta-sync` to the `after-save-hook`:

``` emacs-lisp
(add-hook 'after-save-hook 'metarosetta-sync)
```

This will trigger Metarosetta to check if the file just saved contains any tracked matches, and if so check against its stored *hash* for any potentnial changes. If changes were made, they will be propagated accordingly.

### Navigation

There's also a convenience function called `metarosetta-jump`, which, when called with point on any tracked output match, will navigate and jump directly to the corresponding original match in its source environment.

This is perfect for keeping an all-encompassing and up-to-date index of all the tracked matches of specific type, like actionable code comments or contextual reminder notes.

### Match Tracking Persistence

In order to save all the tracked matches from the current Emacs session just before killing Emacs, it's recommended to add `metarosetta-save` to the `kill-emacs-hook`:

``` emacs-lisp
(add-hook 'kill-emacs-hook 'metarosetta-save)
```

## Metalanguage Expression Examples

See the [Demos](./metarosetta.org#demos) section within the source `org` file.

## Metalanguage Specification

See the [Metalanguage Specification](./metarosetta.org#language-specification) section within the source `org` file.

## Acknowledgements

- The Free Software Foundation and all the contributors to Emacs and its included packages, especially `eieio` and `org`
- [@ndwarshuis](https://github.com/ndwarshuis), the author of [org-ml](https://github.com/ndwarshuis/org-ml)
