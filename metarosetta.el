;;; metarosetta.el --- Convention-based textual metatranslation -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Bruno Tedeschi

;; Author: Bruno Tedeschi <tedi@ffa500.io>
;; Version: 1.0.0-alpha
;; Package-Requires: ((emacs "27.1") (org "9.3") (org-ml "5.7") (dash "2.17"))
;; Keywords: extensions, matching, outlines
;; Homepage: https://github.com/73D1/metarosetta

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

;; An Emacs package introducing support for so-called metalanguage expressions.
;; 
;; Like regular expressions, but in plain English, these enable definitions of
;; arbitrary textual conventions enabling capturing and parsing through
;; matching textual snippets from within the Emacs editing environment.
;; 
;; Matches can be set to automatically produce machine-digestible
;; data structures of parsed information, or to "transpile" the given data
;; to a different textual convention, also defined by another,
;; but data-compatible, metalanguage expression.
;; 
;; For details and language specification, in addition to examples and use cases,
;; please refer to the original package documentation and source org file.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'eieio-base)
(require 'dash)
(require 'org)
(require 'org-ml)

(defclass metarosetta-keychain ()
  ((lastkey
    :initarg :lastkey
    :initform '0
    :type number
    :documentation "The last key generated and assigned within the context of a single keychain instance."
    :reader metarosetta-keychain-lastkey))
  "A key generator helper class.")

(cl-defmethod metarosetta-keychain-generate-key ((keychain metarosetta-keychain))
  "Generate a new key within a provided KEYCHAIN."
  (let ((key (+ 1 (slot-value keychain 'lastkey))))
    (setf (slot-value keychain 'lastkey) key)))

(defclass metarosetta-mlexpression ()
  (
   (mldefinition
    :initarg :mldefinition
    :type list
    :documentation "The metalanguage-specified definition of the expression in context."
    :reader metarosetta-mlexpression-mldefinition)
   (extype
    :type symbol
    :documentation "A symbol specifying the type of the encompassing expression instance. Can be either a :literal, :match or :fractal."
    :reader metarosetta-mlexpression-extype)
   (fractals
    :initform '()
    :type list
    :documentation "A list of metarosetta-expression instances contained within the encompassing expression instance."
    :reader metarosetta-mlexpression-fractals)
   (rkeychain
    :initarg :rkeychain
    :initform (metarosetta-keychain)
    :type metarosetta-keychain
    :documentation "The regex keychain instance managing keys for the encompassing expression tree."
    :reader metarosetta-mlexpression-rkeychain)
   (regex
    :type string
    :documentation "The compiled regular expression of the expression in context."
    :reader metarosetta-mlexpression-regex)
   (regex-key
    :type number
    :documentation "The regex matching group key for the encompassing expression instance."
    :reader metarosetta-mlexpression-regex-key)
   (rinstance
    :type string
    :documentation "The compiled regular expression matching a single instance of a possibly plural-matching expression."
    :reader metarosetta-mlexpression-rinstance)
   (rinstance-key
    :type number
    :documentation "The regex group key for matching a single instance of a possibly plural-matching metalanguage expression in context."
    :reader metarosetta-mlexpression-rinstance-key)
   (rbase
    :type string
    :documentation "The regular expression used as a foundational base in compilation of the match-extracting regular expression."
    :reader metarosetta-mlexpression-rbase)
   (rmatch
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression of the encompassing expression's textual match."
    :reader metarosetta-mlexpression-rmatch)
   (rmatch-key
    :initform 'nil
    :type (or null number)
    :documentation "The regex group key for the encompassing expression's output value."
    :reader metarosetta-mlexpression-rmatch-key)
   (rprefix
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression matching a specified prefix of the encompassing expression instance. Either a regex string or nil."
    :reader metarosetta-mlexpression-rprefix)
   (rsuffix
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression matching a specified suffix of the encompassing expression instance. Either a regex string or nil."
    :reader metarosetta-mlexpression-rsuffix)
   (left-rboundary
    :initform 'nil
    :type (or null string)
    :documentation "The left regex-specific boundary defining the beginning of the match."
    :reader metarosetta-mlexpression-left-rboundary)
   (right-rboundary
    :initform 'nil
    :type (or null string)
    :documentation "The right regex-specific boundary defining the end of the match."
    :reader metarosetta-mlexpression-right-rboundary)
   (rbuffer
    :initform "[[:blank:]]*"
    :type string
    :documentation "The regular expression matching buffer characters surrounding the encompassing expression."
    :reader metarosetta-mlexpression-rbuffer)
   (rbuffer-key
    :type number
    :documentation "The regex group key for the encompassing expression's left buffer match."
    :reader metarosetta-mlexpression-rbuffer-key)
   (key
    :initform 'nil
    :type (or null symbol)
    :documentation "The property key to which the expression output value is assigned, if any. Either a string or nil."
    :reader metarosetta-mlexpression-key)
   (is-uppercase
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches only uppercase words. Either non-nil or nil."
    :reader metarosetta-mlexpression-is-uppercase)
   (is-capitalized
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches only capitalized words. Either non-nil or nil."
    :reader metarosetta-mlexpression-is-capitalized)
   (match-prefix
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the prefix all possible expression matches should have, if any. Either a string or nil."
    :reader metarosetta-mlexpression-match-prefix)
   (match-suffix
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the suffix all possible expression matches should have, if any. Either a string or nil."
    :reader metarosetta-mlexpression-match-suffix)
   (match-substring
    :initform 'nil
    :type (or null string)
    :documentation "Specifies a specific substring all possible expression matches should contain, if any. Either a string or nil."
    :reader metarosetta-mlexpression-match-substring)
   (match-literal
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the literal string that the expression maches exclusively. Either a string or nill."
    :reader metarosetta-mlexpression-match-literal)
   (is-contextual
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is matched elastically depending on neighboring elements. Either non-nil or nil."
    :reader metarosetta-mlexpression-is-contextual)
   (modifier
    :initform 'nil
    :type (or null symbol)
    :documentation "Specifies a symbol referencing a stored modifier function, if any. Either a symbol or nil."
    :reader metarosetta-mlexpression-modifier)
   (modifier-reverse
    :initform 'nil
    :type (or null symbol)
    :documentation "Specifies a symbol referencing a stored modifier function which effectively reverses the modifier in context of the expression, if any. Either symbol or nil."
    :reader metarosetta-mlexpression-modifier-reverse)
   (is-optional
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is optional to match within input text. Either non-nil or nil."
    :reader metarosetta-mlexpression-is-optional)
   (should-ignore
    :initform 'nil
    :documentation "Specifies whether the encompassing expression should be matched but disregarded in output. Either non-nil or nil."
    :reader metarosetta-mlexpression-should-ignore)
   (is-plural
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches plural values or just a single one. Either nil or non-nil."
    :reader metarosetta-mlexpression-is-plural))
  "The Metarosetta Expression object which defines a contextual translational expression used for matching, parsing and structuring data from within conventional text.")

(defvar metarosetta-mlsyntax '())

(cl-defmethod metarosetta-parse-literal ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the :right arg content within ARGS as a literal quote into the MLEXPRESSION instance in context."
  (let ((literal-quote (plist-get args :right)))
    (when (eq literal-quote nil)
      (error "Metalanguage syntax error: Literal expression without quoted content"))
    (setf (slot-value mlexpression 'extype) :literal)
    (setf (slot-value mlexpression 'rbase) (regexp-quote literal-quote))
    (setf (slot-value mlexpression 'match-literal) literal-quote))
  (plist-put args :right nil))

(push '(literal . metarosetta-parse-literal) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-word ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse a word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'extype) :match)
  (setf (slot-value mlexpression 'left-rboundary) "\\<")
  (setf (slot-value mlexpression 'rbase) "[[:word:]]+")
  (setf (slot-value mlexpression 'right-rboundary) "\\>")
  args)

(push '(word . metarosetta-parse-word) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-word-uppercase ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse an uppercase word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setq args (apply 'metarosetta-parse-word mlexpression args))
  (setf (slot-value mlexpression 'rbase) "[A-Z0-9]+")
  (setf (slot-value mlexpression 'is-uppercase) t)
  args)

(push '(WORD . metarosetta-parse-word-uppercase) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-word-capitalized ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse a capitalized word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setq args (apply 'metarosetta-parse-word mlexpression args))
  (setf (slot-value mlexpression 'rbase) "[A-Z0-9][a-z0-9]+")
  (setf (slot-value mlexpression 'is-capitalized) t)
  args)

(push '(Word . metarosetta-parse-word-capitalized) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-word-plurality ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse a plural words expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setq args (apply 'metarosetta-parse-word mlexpression args))
  (setf (slot-value mlexpression 'is-plural) t)
  args)

(push '(words . metarosetta-parse-word-plurality) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-paragraph ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse a paragraph epxression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'extype) :match)
  (setf (slot-value mlexpression 'rbase) ".+?")
  args)

(push '(paragraph . metarosetta-parse-paragraph) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-paragraph-plurality ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse a plural paragraph expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setq args (apply 'metarosetta-parse-paragraph mlexpression args))
  (setf (slot-value mlexpression 'is-plural) t)
  args)

(push '(paragraphs . metarosetta-parse-paragraph-plurality) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-substring ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse quoted text from :right arg within ARGS as matching element substring into the MLEXPRESSION instance in context."
  (let* ((substring-quote (plist-get args :right))
         (rsubstring-quote (regexp-quote substring-quote))
         (rbase (slot-value mlexpression 'rbase)))
    (when (eq substring-quote nil)
      (error "Metalanguage syntax error: Substring match expression without quoted content"))
    (setf (slot-value mlexpression 'rmatch)
          (concat "\\(?:"
                  "\\(?:" "\\(?:" rbase "\\)?" rsubstring-quote "\\)*" rbase "\\(?:" rsubstring-quote "\\(?:" rbase "\\)?" "\\)+"
                  "\\|"
                  "\\(?:" "\\(?:" rbase "\\)?" rsubstring-quote "\\)+" rbase "\\(?:" rsubstring-quote "\\(?:" rbase "\\)?" "\\)*"
                  "\\)"))
    (setf (slot-value mlexpression 'match-substring) substring-quote))
  (plist-put args :right nil))

(push '(with . metarosetta-parse-substring) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-prefix ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse quoted text from :left arg within ARGS as matching element prefix into the MLEXPRESSION instance in context."
  (let ((prefix-quote (plist-get args :left)))
    (when (eq prefix-quote nil)
      (error "Metalanguage syntax error: Prefix match expression without quoted content"))
    (setf (slot-value mlexpression 'rprefix) (regexp-quote prefix-quote))
    (setf (slot-value mlexpression 'match-prefix) prefix-quote))
  (plist-put args :left nil))

(push '(prefixed . metarosetta-parse-prefix) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-suffix ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse quoted text from :left arg within ARGS as matching element suffix into the MLEXPRESSION instance in context."
  (let ((suffix-quote (plist-get args :left)))
    (when (eq suffix-quote nil)
      (error "Metalanguage syntax error: Suffix match expression without quoted content"))
    (setf (slot-value mlexpression 'rsuffix) (regexp-quote suffix-quote))
    (setf (slot-value mlexpression 'match-suffix) suffix-quote))
  (plist-put args :left nil))

(push '(suffixed . metarosetta-parse-suffix) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-contextual ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the contextual specifier into the MLEXPRESSION instance in context. This function utilizes no ARGS."
  (setf (slot-value mlexpression 'is-contextual) t)
  args)

(push '(contextual . metarosetta-parse-contextual) metarosetta-mlsyntax)

(defvar metarosetta-mlsyntax-modifiers '())

(defvar metarosetta-mlsyntax-modifiers-reverse '())

(push '(uppercase . upcase) metarosetta-mlsyntax-modifiers)

(push '(uppercase . downcase) metarosetta-mlsyntax-modifiers-reverse)

(push '(lowercase . downcase) metarosetta-mlsyntax-modifiers)

(push '(lowercase . upcase) metarosetta-mlsyntax-modifiers-reverse)

(cl-defmethod metarosetta-parse-modifier ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the modifier symbol from :right arg within ARGS into the MLEXPRESSION instance in context."
  (let ((modifier-symbol (plist-get args :right)))
    (when (eq modifier-symbol nil)
      (error "Metalanguage syntax error: Modifier expression without contextual argument symbol"))
    (setf (slot-value mlexpression 'modifier)
          (cdr (assq modifier-symbol metarosetta-mlsyntax-modifiers)))
    (setf (slot-value mlexpression 'modifier-reverse)
          (cdr (assq modifier-symbol metarosetta-mlsyntax-modifiers-reverse))))
  (plist-put args :right nil))

(push '(to . metarosetta-parse-modifier) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-optionality ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse expression optionality into the MLEXPRESSION instance in context. This function utilizes no ARGS."
  (setf (slot-value mlexpression 'is-optional) t)
  args)

(push '(optional . metarosetta-parse-optionality) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-key ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the key symbol from :right arg within ARGS into the MLEXPRESSION instance in context."
  (let ((key-symbol (plist-get args :right)))
    (when (eq key-symbol nil)
      (error "Metalanguage syntax error: Key assignment without contextual key symbol"))
    (setf (slot-value mlexpression 'key) key-symbol))
  (plist-put args :right nil))

(push '(as . metarosetta-parse-key) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-ignorable ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the ignorable property into the MLEXPRESSION instance in context. This function utilizes no ARGS."
  (setf (slot-value mlexpression 'should-ignore) t)
  args)

(push '(ignorable . metarosetta-parse-ignorable) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-list ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the list expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-plural) t)
  args)

(push '(list . metarosetta-parse-list) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-element ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the element expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-plural) nil)
  args)

(push '(element . metarosetta-parse-element) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse-of ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the sub-expression from :right arg within ARGS into the MLEXPRESSION instance in context."
  (let ((sub-expression (plist-get args :right)))
    (when (or (eq sub-expression nil) (nlistp sub-expression))
      (error "Metalanguage syntax error: Sub-expression assignment without contextual expression"))
    (metarosetta-parse mlexpression :sub sub-expression))
  (plist-put args :right nil))

(push '(of . metarosetta-parse-of) metarosetta-mlsyntax)

(cl-defmethod metarosetta-parse ((mlexpression metarosetta-mlexpression) &rest args)
  "Parse the metalanguage-specified definition within the MLEXPRESSION instance. Optionally, parse the explicitly-set :sub definition in ARGS instead."
  (let* ((sub-definition (plist-get args :sub))
         (mldefinition (if (eq sub-definition nil)
                           (copy-tree (slot-value mlexpression 'mldefinition))
                         (copy-tree sub-definition)))
         (larg)
         (element)
         (rarg))
    (while (> (length mldefinition) 0)
      (setq element (pop mldefinition)
            rarg (car mldefinition))
      (when (symbolp element)
        ;; The element is a metalanguage keyword, so lookup the corresponding function and parse accordingly
        (let ((leftout-args (funcall (cdr (assq element metarosetta-mlsyntax)) mlexpression :left larg :right rarg)))
          (setq larg nil)
          (when (eq (plist-get leftout-args :right) nil)
            (pop mldefinition))))
      (when (and (listp element) (> (length element) 0))
        ;; The element is a nested fractal expression
        (setf (slot-value mlexpression 'extype) :fractal)
        (let ((fractal-mlexpression (metarosetta-mlexpression :mldefinition element :rkeychain (slot-value mlexpression 'rkeychain))))
          (setf (slot-value mlexpression 'fractals) `(,@(slot-value mlexpression 'fractals) ,fractal-mlexpression))
          (metarosetta-parse fractal-mlexpression))
        (setq larg nil))
      (when (stringp element)
        ;; The element is a quoted string, so just pass it along
        (setq larg element)))))

(cl-defmethod metarosetta-compile ((mlexpression metarosetta-mlexpression))
  "Compile the MLEXPRESSION instance into a regular expression structure."
  (let* ((rkeychain (slot-value mlexpression 'rkeychain))
         (regex)
         (regex-key (metarosetta-keychain-generate-key rkeychain))
         (rinstance)
         (rinstance-key (metarosetta-keychain-generate-key rkeychain))
         (rmatch (slot-value mlexpression 'rmatch))
         (rmatch-key (metarosetta-keychain-generate-key rkeychain))
         (rprefix (slot-value mlexpression 'rprefix))
         (rsuffix (slot-value mlexpression 'rsuffix))
         (left-rboundary (slot-value mlexpression 'left-rboundary))
         (right-rboundary (slot-value mlexpression 'right-rboundary))
         (rbuffer (slot-value mlexpression 'rbuffer))
         (rbuffer-key (metarosetta-keychain-generate-key rkeychain))
         (is-contextual (slot-value mlexpression 'is-contextual))
         (is-optional (slot-value mlexpression 'is-optional))
         (is-plural (slot-value mlexpression 'is-plural)))
    (if (eq (slot-value mlexpression 'extype) :fractal)
        ;; Recursively compile all nested fractal expression instances
        (let ((fractals (slot-value mlexpression 'fractals)))
          ;; Fractal Expressions cannot have end-matches
          (when rmatch
            (error "Metalanguage syntax error: End-matching expressions, like words or paragraphs, must be defined within parentheses"))
          (dolist (fractal fractals)
            (setq rmatch (concat rmatch (metarosetta-compile fractal)))))
      ;; Literal or end Match
      (when (eq rmatch nil)
        (setq rmatch (slot-value mlexpression 'rbase))))
    ;; Compile the total match, instance and expression-encompassing regular expressions
    (setq rmatch (concat "\\(?" (number-to-string rmatch-key) ":" rmatch "\\)"))
    (setq rinstance (concat "\\(?" (number-to-string rinstance-key) ":"
                            "\\(?" (number-to-string rbuffer-key) ":" rbuffer "\\)"
                            (when (not is-contextual)
                              (or rprefix left-rboundary))
                            rmatch
                            (when (not is-contextual)
                              (or rsuffix right-rboundary))
                            "\\)"))
    (setq regex (concat "\\(?" (number-to-string regex-key) ":"
                        rinstance
                        (when is-plural "+")
                        "\\)"
                        (when is-optional "?")))
    (setf (slot-value mlexpression 'rmatch-key) rmatch-key
          (slot-value mlexpression 'rmatch) rmatch
          (slot-value mlexpression 'rbuffer-key) rbuffer-key
          (slot-value mlexpression 'rinstance-key) rinstance-key
          (slot-value mlexpression 'rinstance) rinstance
          (slot-value mlexpression 'regex-key) regex-key
          (slot-value mlexpression 'regex) regex)))

(cl-defmethod metarosetta-process ((mlexpression metarosetta-mlexpression) htext)
  "Process human-readable text HTEXT and return the semantically-significant data structure as defined by the MLEXPRESSION instance."
  (when (metarosetta-mlexpression-should-ignore mlexpression)
    (error "Metalanguage semantic error: Root expressions cannot be ignorable"))
  (let ((exregex (metarosetta-mlexpression-regex mlexpression))
        (exrinstance (metarosetta-mlexpression-rinstance mlexpression))
        (exdata '())
        (case-fold-search nil))
    (when (metarosetta-mlexpression-is-contextual mlexpression)
      ;; Ensure complete matches of contextual expressions
      (setq exregex (concat "^" exregex "$"))
      (when (metarosetta-mlexpression-is-plural mlexpression)
        (error "Metalanguage semantic error: Contextual expressions cannot be plural"))
      (setq exrinstance (concat "^" exrinstance "$")))
    (save-match-data
      (and htext
           (string-match exregex htext)
           ;; Found match for the entirety of the expression
           (let ((extext (match-string (metarosetta-mlexpression-regex-key mlexpression) htext))
                 (pos))
             (save-match-data
               ;; Iterate over all instance occurrences within the expression-matching text
               (while (string-match exrinstance extext pos)
                 (setq pos (match-end 0))
                 ;; Process the exact match as defined by the expression
                 (let ((instance-exdata))
                   ;; Cases where the expression is a :fractal
                   (when (eq (metarosetta-mlexpression-extype mlexpression) :fractal)
                     ;; Recursively process all non-ignorable fractals within
                     (let ((fractals (metarosetta-mlexpression-fractals mlexpression)))
                       (dolist (fractal fractals)
                         (when (not (metarosetta-mlexpression-should-ignore fractal))
                           (let ((fractal-exdata (metarosetta-process fractal (match-string (metarosetta-mlexpression-regex-key fractal) extext))))
                             (when fractal-exdata
                               (setq instance-exdata `(,@instance-exdata ,fractal-exdata))))))))
                   ;; Cases where the expression is a :match
                   (when (eq (metarosetta-mlexpression-extype mlexpression) :match)
                     ;; Just store the end-match, modified if defined as such
                     (let ((match (match-string (metarosetta-mlexpression-rmatch-key mlexpression) extext))
                           (modifier (metarosetta-mlexpression-modifier mlexpression)))
                       (when modifier
                         (setq match (funcall modifier match)))
                       (setq instance-exdata match)))
                   (when instance-exdata
                     (setq exdata `(,@exdata ,instance-exdata)))))
               (when exdata
                 ;; Splice instance data in case of a singular expression
                 (when (not (metarosetta-mlexpression-is-plural mlexpression))
                   (setq exdata (car exdata)))
                 ;; Return the structured data object
                 `(,(or (metarosetta-mlexpression-key mlexpression) :nokey) . ,exdata))))))))

(cl-defmethod metarosetta-update ((mlexpression metarosetta-mlexpression) htext sdata)
  "Process human readable text HTEXT and return the semantically updated text based on the provided SDATA structure, as defined by the MLEXPRESSION instance."
  (let ((exregex (metarosetta-mlexpression-regex mlexpression))
        (exrinstance (metarosetta-mlexpression-rinstance mlexpression))
        (exkey (car sdata))
        (exdata-is-set (cdr sdata))
        (exdata (copy-tree (cdr sdata)))
        (newtext)
        (case-fold-search nil))
    (when (and exdata
               (not (eq exkey
                        (or (metarosetta-mlexpression-key mlexpression) :nokey))))
      (error "Data structure error: Key mismatch"))
    (when (metarosetta-mlexpression-is-contextual mlexpression)
      ;; Ensure complete matches of contextual expressions
      (setq exregex (concat "^" exregex "$"))
      (when (metarosetta-mlexpression-is-plural mlexpression)
        (error "Metalanguage semantic error: Contextual expressions cannot be plural"))
      (setq exrinstance (concat "^" exrinstance "$")))
    (save-match-data
      (and htext
           (string-match exregex htext)
           ;; Found metalanguage expression match
           (let ((extext (match-string (metarosetta-mlexpression-regex-key mlexpression) htext))
                 (pos '()))
             (save-match-data
               (while (or (and (string-match exrinstance extext (car pos))
                               ;; Handle plural expressions, including variations in length between updated and original sets
                               (or (not (metarosetta-mlexpression-is-plural mlexpression))
                                   ;; Expression is plural, but check if there is any updated data to insert
                                   (not exdata-is-set)
                                   ;; Updated list data is set, but only continue if any updated instances are left
                                   ;; Otherwise, just dispose of the remainder of the original
                                   exdata)
                               ;; An instance matched within the original text, update pos and enter iteration
                               (push (match-end 0) pos))
                          (and exdata
                               ;; No instances left within original text, but exdata still holding additional elements
                               (metarosetta-mlexpression-is-plural mlexpression)
                               ;; Reuse the last matched instance from the original text as a template
                               (string-match exrinstance extext (cadr pos))))
                 ;; Update each instance
                 (let ((instance-exdata (if (metarosetta-mlexpression-is-plural mlexpression) (pop exdata) exdata))
                       (instance-newtext))
                   (if (eq (metarosetta-mlexpression-extype mlexpression) :fractal)
                       ;; Recursively update all fractals within
                       (let ((fractals (metarosetta-mlexpression-fractals mlexpression)))
                         (dolist (fractal fractals)
                           (let* ((fractal-exdata (assq (metarosetta-mlexpression-key fractal) instance-exdata))
                                  (fractal-text (match-string (metarosetta-mlexpression-regex-key fractal) extext))
                                  (fractal-newtext (metarosetta-update fractal fractal-text fractal-exdata)))
                             (setq instance-newtext (concat instance-newtext fractal-newtext)))))
                     ;; Update leaf elements
                     (when (eq (metarosetta-mlexpression-extype mlexpression) :match)
                       ;; Update match text, including ignorable matches
                       (let* ((buffer (match-string (metarosetta-mlexpression-rbuffer-key mlexpression) extext))
                              (prefix (metarosetta-mlexpression-match-prefix mlexpression))
                              (suffix (metarosetta-mlexpression-match-suffix mlexpression))
                              (modifier-reverse (metarosetta-mlexpression-modifier-reverse mlexpression))
                              (match (or (when (and modifier-reverse instance-exdata)
                                           (funcall modifier-reverse instance-exdata))
                                         instance-exdata
                                         (match-string (metarosetta-mlexpression-rmatch-key mlexpression) extext))))
                         (setq instance-newtext (concat buffer prefix match suffix))))
                     (when (eq (metarosetta-mlexpression-extype mlexpression) :literal)
                       ;; Just include the literal instance
                       (setq instance-newtext (match-string (metarosetta-mlexpression-rinstance-key mlexpression) extext))))
                   (setq newtext (concat newtext instance-newtext))))
               ;; Return the updated text
               newtext))))))

(defvar metarosetta-configuration-directory nil
  "Metarosetta's active configuration directory.")

(defvar metarosetta-index-connectors '()
  "Metarosetta's global index of registered output connectors by the target syntax type symbol.")

(defvar metarosetta-index-configurations '()
  "Metarosetta's global index of loaded configurations by their respective root metalanguage expression key symbols.")

(defvar metarosetta-index-sources (make-hash-table :test 'equal)
  "Metarosetta's global reverse index of all the tracked matches by their residential file name, each entry holding a cons cell with the file type symbol and the total number of matches within.")

(defvar metarosetta-id-delimiters '("[" . "]")
  "A cons cell defining the left and right Metarosetta match identifier delimiters, respectively.")

(defun metarosetta-id-generate-re (&rest args)
  "Generate a regular expression matching the Metarosetta match identifier pattern. Within ARGS, optionally set the :root-gkey, :match-gkey as well as the :output-gkey regex group key integers."
  (let ((left-delimiter (car metarosetta-id-delimiters))
        (right-delimiter (cdr metarosetta-id-delimiters))
        (root-gkey (plist-get args :root-gkey))
        (match-gkey (plist-get args :match-gkey))
        (output-gkey (plist-get args :output-gkey)))
    (concat "\\" left-delimiter
            "\\(?" (when root-gkey (number-to-string root-gkey)) ":"
            "[[:alpha:]]+"
            "\\)"
            "\\-"
            "\\(?" (when match-gkey (number-to-string match-gkey)) ":"
            "[[:digit:]]+"
            "\\)"
            "\\(?:"
            "\\-"
            "\\(?" (when output-gkey (number-to-string output-gkey)) ":"
            "[[:alpha:]]+"
            "\\)"
            "\\)?"
            "\\" right-delimiter)))

(defun metarosetta-id-parse-match-identifier (match-identifier)
  "Parse the MATCH-IDENTIFIER and return a property list containing the root metalanguage expression key under :root-key, match serial key under :match-serial and the optional output expression key under :output-key. Given an invalid match, return nil."
  (save-match-data
    (and match-identifier
         (string-match (concat "^"
                               (metarosetta-id-generate-re :root-gkey 1
                                                           :match-gkey 2
                                                           :output-gkey 3)
                               "$")
                       match-identifier)
         ;; Provided match identifier is of valid format
         (let* ((root-key (intern (match-string 1 match-identifier)))
                (match-serial (string-to-number (match-string 2 match-identifier)))
                (output-key-string (match-string 3 match-identifier))
                (output-key (when output-key-string
                              (intern output-key-string))))
           ;; Return the parsed identifier
           `(:root-key ,root-key
             :match-serial ,match-serial
             :output-key ,output-key)))))

(defun metarosetta-id-parse-match (full-match)
  "Parse the match identifier portion of the FULL-MATCH, and return a property list containing the root metalanguage expression key under :root-key, match serial key under :match-serial and the optional output expression key under :output-key, and lastly the core match under :core-match. Given an invalid match, return nil."
  (let ((full-match-regex (concat "^"
                                  ;; Match the core portion of the full match
                                  "\\(?1:.+\\)"
                                  " "
                                  ;; Match the identifier portion of the full match
                                  (metarosetta-id-generate-re :root-gkey 2
                                                              :match-gkey 3
                                                              :output-gkey 4)
                                  "$")))
    (save-match-data
      (and full-match
           (string-match full-match-regex full-match)
           ;; Provided full match is of valid format
           (let* ((core-match (match-string 1 full-match))
                  (root-key (intern (match-string 2 full-match)))
                  (match-serial (string-to-number (match-string 3 full-match)))
                  (output-key-string (match-string 4 full-match))
                  (output-key (when output-key-string
                                (intern output-key-string))))
             ;; Return the parsed data structure
             `(:root-key ,root-key
               :match-serial ,match-serial
               :output-key ,output-key
               :core-match ,core-match))))))

(defun metarosetta-id-serialize-match-identifier (&rest args)
  "Serialize the match identifier to string form from ARGS containing the root expression key symbol under :root-key, the match serial key integer under :match-serial and an optional output expression key symbol under :output-key."
  (let ((root-key (plist-get args :root-key))
        (match-serial (plist-get args :match-serial))
        (output-key (plist-get args :output-key))
        (left-delimiter (car metarosetta-id-delimiters))
        (right-delimiter (cdr metarosetta-id-delimiters)))
    ;; Return the match identifier in string form
    (concat left-delimiter
            (symbol-name root-key)
            "-"
            (number-to-string match-serial)
            (when output-key
              (concat "-"
                      (symbol-name output-key)))
            right-delimiter)))

(defun metarosetta-id-serialize-match (&rest args)
  "Serialize the full match string by composing the match identifier from the :root-key, :match-serial and an optional :output-key within ARGS and ultimately appending it to the :core-match in string form."
  (concat (plist-get args :core-match)
          " "
          (apply 'metarosetta-id-serialize-match-identifier args)))

(defclass metarosetta-org-expression ()
  ((mlexpression
    :type metarosetta-mlexpression
    :documentation "The actual metalanguage expression of the org expression object in context.")
   (key
    :type symbol
    :documentation "The symbol uniquely representing the expression in context. All expression matches contained within are prefixed with this key."
    :reader metarosetta-org-expression-key)
   (match-type
    :type symbol
    :documentation "The symbol of the specific metarosetta-org-match subclass defining all the tracked matches within this expression object.")
   (matches
    :type list
    :documentation "A serial.match alist containing the org match objects corresponding to all the processed and tracked matches of the expression in context."))
  "The Metarosetta org configuration object representing a specific metalanguage expression."
  :abstract t)

(defclass metarosetta-org-match ()
  ((raw-match
    :initarg :raw-match
    :type string
    :documentation "The full textual match in context."
    :reader metarosetta-org-match-raw
    :writer metarosetta-org-match-raw-set)
   (parsed-match
    :initarg :parsed-match
    :type list
    :documentation "The parsed match structure, generated by processing the textual match through the metalanguage expression in context."
    :reader metarosetta-org-match-parsed
    :writer metarosetta-org-match-parsed-set)
   (match-hash
    :initarg :match-hash
    :type number
    :documentation "An integer hash of the raw match in context. This is used for tracking any potential updates to the match."
    :reader metarosetta-org-match-hash
    :writer metarosetta-org-match-hash-set)
   (root-key
    :initarg :root-key
    :type symbol
    :documentation "The key symbol of the root configuration object defining the match origin."
    :reader metarosetta-org-match-root-key)
   (serial
    :initarg :serial
    :type number
    :documentation "The serial number of the match in context, in scope of the matching expression."
    :reader metarosetta-org-match-serial)
   (last-updated
    :initarg :last-updated
    :type string
    :documentation "The human-readable string of a timestamp when the match in context was last updated."
    :writer metarosetta-org-match-last-updated-set)
   (op-type
    :initarg :op-type
    :type symbol
    :documentation "A keyword symbol specifying the type of the last operation done on the match in context. Can either be :created, :downloaded or :uploaded."
    :writer metarosetta-org-match-op-type-set))
  "The Metarosetta org configuration object representing a single match of a given metalanguage expression."
  :abstract t)

(defclass metarosetta-org-expression-root (metarosetta-org-expression)
  ((match-type
    :initform 'metarosetta-org-match-original)
   (match-keychain
    :type metarosetta-keychain
    :documentation "The keychain instance responsible for generating unique original match keys in scope of the org expression object in context."))
  "The Metarosetta org configuration object representing the root metalanguage expression in context of its containing org file.")

(defclass metarosetta-org-match-original (metarosetta-org-match)
  ((source-filename
    :initarg :source-filename
    :type string
    :documentation "The filename of the source file where the match in context originally resides."
    :reader metarosetta-org-match-original-source-filename))
  "The Metarosetta org configuration object representing a single original match of its defining root metalanguage expression.")

(cl-defmethod metarosetta-org-expression-root-match-source-filename ((oexpression metarosetta-org-expression-root) match-serial)
  "Given the provided MATCH-SERIAL key, return the source filename of the corresponding original match in context of the OEXPRESSION Metarosetta root expression configuration object. If no such match exists, return nil."
  (let ((match (cdr (assq match-serial
                          (slot-value oexpression 'matches)))))
    ;; Return source filename if match found
    (when match
      (metarosetta-org-match-original-source-filename match))))

(defclass metarosetta-org-expression-output (metarosetta-org-expression)
  ((match-type
    :initform 'metarosetta-org-match-output)
   (target-type
    :type symbol
    :documentation "A symbol denoting the type of the target file or endpoint where the match in context should be appended to, or sent to.")
   (target-endpoint-template
    :type string
    :documentation "The template of the target filename, or URI, where the match in context should be added to. In addition to literal elements along the path, $-prefixed expression key symbols can be used to interpolate processed expression elements into the path itself.")
   (target-section-template
    :type string
    :documentation "The template of the '/'-delimited section path defining the exact section within the target under which the match should reside or be sent to. In addition to literal sections along the path, $-prefixed expression key symbols can be used to interpolate expression elements into the path itself.")
   (template
    :type string
    :documentation "A string used as an output template based on which the expression in context will generate the output text itself."))
  "The Metarosetta org configuration object representing an output expression in context of its containing org file.")

(defclass metarosetta-org-match-output (metarosetta-org-match)
  ((output-key
    :initarg :output-key
    :type symbol
    :documentation "The key symbol of the output configuration object defining the match output."
    :reader metarosetta-org-match-output-key)
   (target-type
    :initarg :target-type
    :type symbol
    :documentation "A symbol denoting the type of the target file or endpoint where the match in context should be appended to, or sent to.")
   (target-endpoint
    :initarg :target-endpoint
    :type string
    :documentation "The literal filename, or URI, of the target where the match in context should be appended to, or sent to."
    :reader metarosetta-org-match-output-target-endpoint
    :writer metarosetta-org-match-output-target-endpoint-set)
   (target-section
    :initarg :target-section
    :type string
    :documentation "The full section path defining the exact section within the target under which the match should reside or be sent to."
    :reader metarosetta-org-match-output-target-section))
  "The Metarosetta org configuration object representing an output match defined by its encompassing output expression.")

(defclass metarosetta-org-config ()
  ((source-filename
    :initarg :source-filename
    :type string
    :documentation "The filename of the source org file for the configuration in context."
    :reader metarosetta-org-config-source-filename)
   (root-expression
    :type metarosetta-org-expression-root
    :documentation "The root expression configuration object in context of the configuration set.")
   (output-expressions
    :type list
    :documentation "An alist of output expression configuration objects by their respective expression keys in context of the configuration set."))
  "The Metarosetta org configuration object ")

(cl-defmethod metarosetta-org-config-key ((oconfig metarosetta-org-config))
  "Return the unique key symbol defining the OCONFIG Metarosetta org configuration object."
  (metarosetta-org-expression-key (slot-value oconfig 'root-expression)))

(cl-defmethod metarosetta-org-config-original-match-source-filename ((oconfig metarosetta-org-config) match-serial)
  "Given the provided MATCH-SERIAL key, return the source filename of the corresponding original match in context of the root expression within the OCONFIG Metarosetta configuration set object. If no such match exists, return nil."
  (metarosetta-org-expression-root-match-source-filename (slot-value oconfig 'root-expression) match-serial))

(cl-defmethod metarosetta-org-parse ((oexpression metarosetta-org-expression) oelement)
  "Parse and compile the metalanguage expression, along with other properties, defined by the org-ml headline element OELEMENT into the OEXPRESSION instance. Recursively parse all match elements contained within OELEMENT."
  ;; Parse the metalanguage expression
  (let* ((mldefinition-string (car (org-ml-get-property :title oelement)))
         (mldefinition (car (read-from-string (concat "(" mldefinition-string ")"))))
         (mlexpression (metarosetta-mlexpression :mldefinition mldefinition)))
    ;; Parse and compile the loaded metalanguage expression, so it's ready for textual processing
    (metarosetta-parse mlexpression)
    (metarosetta-compile mlexpression)
    ;; Finally, store the initialized metalanguage expression into the configuration object
    (setf (slot-value oexpression 'mlexpression) mlexpression))
  ;; Parse the expression's key symbol
  (let ((key (intern (org-ml-headline-get-node-property "KEY" oelement))))
    (setf (slot-value oexpression 'key) key))
  ;; Recursively parse all tracked matches
  (let ((match-type (slot-value oexpression 'match-type)))
    (setf (slot-value oexpression 'matches)
          (mapcar (lambda (match-oelement)
                    (let ((omatch (metarosetta-org-parse (make-instance match-type) match-oelement)))
                      `(,(metarosetta-org-match-serial omatch) . ,omatch)))
                  (org-ml-headline-get-subheadlines oelement))))
  ;; Return the parsed object
  oexpression)

(cl-defmethod metarosetta-org-serialize ((oexpression metarosetta-org-expression))
  "Serialize the OEXPRESSION into an org-ml headline element. Also, recursively serialize all the contained match objects."
  (let* ((mlexpression (slot-value oexpression 'mlexpression))
         (key (slot-value oexpression 'key))
         (matches (slot-value oexpression 'matches))
         (mldefinition (slot-value mlexpression 'mldefinition))
         ;; Serialize the metalanguage definition to string form
         (mldefinition-string (mapconcat 'prin1-to-string mldefinition " "))
         ;; Serialize the expression key's symbol to string form
         (key-string (symbol-name key))
         ;; Create the org-ml headline element with the serialized ml definition as title
         (oelement (org-ml-build-headline :level 1 :title `(,mldefinition-string))))
    ;; Append the expression key within the headline element property drawer
    (setq oelement (org-ml-headline-set-node-property "KEY" key-string oelement))
    ;; Recursively serialize contained matches and set them as element subheadlines
    (setq oelement (org-ml-headline-set-subheadlines (mapcar (lambda (match-pair)
                                                               (let ((match (cdr match-pair)))
                                                                 (metarosetta-org-serialize match)))
                                                             matches)
                                                     oelement))
    ;; Return the serialized org-ml element
    oelement))

(cl-defmethod metarosetta-org-parse ((omatch metarosetta-org-match) oelement)
  "Parse the textual match and its corresponding metadata defined by the org-ml headline element OELEMENT into the OMATCH instance."
  ;; Parse the raw textual match
  (let ((raw-match (car (org-ml-get-property :title oelement))))
    (setf (slot-value omatch 'raw-match) raw-match))
  ;; Parse the processed match structure from the contained source block
  (let* ((source-block (cadr (org-ml-headline-get-section oelement)))
         (parsed-match (car (read-from-string (org-ml-get-property :value source-block)))))
    (setf (slot-value omatch 'parsed-match) parsed-match))
  ;; Parse the integer hash of the raw match used for comparison purposes
  (let ((match-hash (string-to-number (org-ml-headline-get-node-property "HASH" oelement))))
    (setf (slot-value omatch 'match-hash) match-hash))
  ;; Set the match root key
  (let ((root-key (intern (org-ml-headline-get-node-property "ROOT-KEY" oelement))))
    (setf (slot-value omatch 'root-key) root-key))
  ;; Set the actual match identifier
  (let ((serial (string-to-number (org-ml-headline-get-node-property "SERIAL" oelement))))
    (setf (slot-value omatch 'serial) serial))
  ;; Set the last updated metadata property
  (let ((last-updated (org-ml-headline-get-node-property "LAST-UPDATED" oelement)))
    (setf (slot-value omatch 'last-updated) last-updated))
  ;; Set the operation type keyword in context of the last update
  (let ((op-type (intern (org-ml-headline-get-node-property "OPERATION-TYPE" oelement))))
    (setf (slot-value omatch 'op-type) op-type))
  ;; Return the parsed object
  omatch)

(cl-defmethod metarosetta-org-serialize ((omatch metarosetta-org-match))
  "Serialize the OMATCH into an org-ml headline element."
  (let* ((raw-match (slot-value omatch 'raw-match))
         (parsed-match (slot-value omatch 'parsed-match))
         (match-hash (slot-value omatch 'match-hash))
         (root-key (slot-value omatch 'root-key))
         (serial (slot-value omatch 'serial))
         (last-updated (slot-value omatch 'last-updated))
         (op-type (slot-value omatch 'op-type))
         ;; Create the org-ml headline element with the raw match as its title
         (oelement (org-ml-build-headline :level 2 :title `(,raw-match)))
         ;; Serialize the parsed match Lisp structure into an org source block
         (source-block (org-ml-build-src-block :language "emacs-lisp"
                                               :value (prin1-to-string parsed-match))))
    ;; Set the serialized source block as the org element's inner section
    (setq oelement (org-ml-headline-set-section `(,source-block) oelement))
    ;; Append the match hash and identifier, as well as last updated and operation type metadata properties within the headline element property drawer
    (setq oelement (->> (org-ml-headline-set-node-property "HASH" (number-to-string match-hash) oelement)
                        (org-ml-headline-set-node-property "ROOT-KEY" (symbol-name root-key))
                        (org-ml-headline-set-node-property "SERIAL" (number-to-string serial))
                        (org-ml-headline-set-node-property "LAST-UPDATED" last-updated)
                        (org-ml-headline-set-node-property "OPERATION-TYPE" (symbol-name op-type))))
    ;; Return the serialized org-ml element
    oelement))

(cl-defmethod metarosetta-org-parse ((oexpression metarosetta-org-expression-root) oelement)
  "Parse and compile the root metalanguage expression, along with other properties, defined by the org-ml headline element OELEMENT into the OEXPRESSION instance. Recursively parse all original match elements contained within OELEMENT."
  ;; First parse the base expression properties, including the metalanguage expression as well as child matches
  (cl-call-next-method)
  ;; Parse the last serial match key, and use it to initialize the keychain in context of the expression
  (let ((last-match-key (string-to-number (or (org-ml-headline-get-node-property "LAST-MATCH-KEY" oelement)
                                              ;; If no last key property recorded, just initialize with an empty string resulting in the integer 0
                                              ""))))
    (setf (slot-value oexpression 'match-keychain) (metarosetta-keychain :lastkey last-match-key)))
  ;; Return the parsed object
  oexpression)

(cl-defmethod metarosetta-org-serialize ((oexpression metarosetta-org-expression-root))
  "Serialize the root OEXPRESSION into an org-ml headline element. ALso, recursively serialize all the contained original match objects."
  (let* ((match-keychain (slot-value oexpression 'match-keychain))
         (last-match-key (metarosetta-keychain-lastkey match-keychain))
         ;; Serialize the base properties first
         (oelement (cl-call-next-method)))
    ;; Append the root expression-specific properties within the headline element property drawer
    (setq oelement (org-ml-headline-set-node-property "LAST-MATCH-KEY" (number-to-string last-match-key) oelement))
    ;; Return the serialized org-ml element
    oelement))

(cl-defmethod metarosetta-org-parse ((omatch metarosetta-org-match-original) oelement)
  "Parse the textual original match and its corresponding metadata defined by the org-ml headline element OELEMENT into the OMATCH instance."
  ;; Parse the base properties first
  (cl-call-next-method)
  ;; Parse the source filename where the original match is tracked from
  (let ((source-filename (org-ml-headline-get-node-property "SOURCE-FILE" oelement)))
    (setf (slot-value omatch 'source-filename) source-filename)
    ;; Update the reverse index of sources
    (let ((index-entry (gethash source-filename
                                metarosetta-index-sources
                                '(:root . 0))))
      (puthash source-filename
               `(,(car index-entry) . ,(1+ (cdr index-entry)))
               metarosetta-index-sources)))
  ;; Return the parsed object
  omatch)

(cl-defmethod metarosetta-org-serialize ((omatch metarosetta-org-match-original))
  "Serialize the original match object OMATCH into an org-ml headline element."
  (let ((source-filename (slot-value omatch 'source-filename))
        ;; Serialize the base properties first
        (oelement (cl-call-next-method)))
    ;; Append the source filename within the headline element property drawer
    (setq oelement (org-ml-headline-set-node-property "SOURCE-FILE" source-filename oelement))
    ;; Return the serialized org-ml element
    oelement))

(cl-defmethod metarosetta-org-parse ((oexpression metarosetta-org-expression-output) oelement)
  "Parse and compile the metalanguage expression, along with other output expression properties, defined by the org-ml headline element OELEMENT into the OEXPRESSION instance. Recursively parse all match output elements contained within OELEMENT."
  ;; First parse the base expression properties, including the metalanguage expression as well as child matches
  (cl-call-next-method)
  ;; Parse the target type symbol, referencing the corresponding output connector
  (let ((target-type (intern (org-ml-headline-get-node-property "TARGET-TYPE" oelement))))
    (setf (slot-value oexpression 'target-type) target-type))
  ;; Parse the target endpoint template
  (let ((target-endpoint-template (org-ml-headline-get-node-property "TARGET-ENDPOINT-TEMPLATE" oelement)))
    (setf (slot-value oexpression 'target-endpoint-template) target-endpoint-template))
  ;; Parse the target section template
  (let ((target-section-template (org-ml-headline-get-node-property "TARGET-SECTION-TEMPLATE" oelement)))
    (setf (slot-value oexpression 'target-section-template) target-section-template))
  ;; Parse the output template string
  (let ((template (org-ml-headline-get-node-property "TEMPLATE" oelement)))
    (setf (slot-value oexpression 'template) template))
  ;; Return the parsed object
  oexpression)

(cl-defmethod metarosetta-org-serialize ((oexpression metarosetta-org-expression-output))
  "Serialize the output OEXPRESSION into an org-ml headline element. Also, recursively serialize all the contained match objects."
  (let ((target-type (slot-value oexpression 'target-type))
        (target-endpoint-template (slot-value oexpression 'target-endpoint-template))
        (target-section-template (slot-value oexpression 'target-section-template))
        (template (slot-value oexpression 'template))
        ;; Serialize the base properties first
        (oelement (cl-call-next-method)))
    ;; Append all the essential output expression properties within the headline element property drawer
    (setq oelement (->> (org-ml-headline-set-node-property "TARGET-TYPE" (symbol-name target-type) oelement)
                        (org-ml-headline-set-node-property "TARGET-ENDPOINT-TEMPLATE" target-endpoint-template)
                        (org-ml-headline-set-node-property "TARGET-SECTION-TEMPLATE" target-section-template)
                        (org-ml-headline-set-node-property "TEMPLATE" template)))
    ;; Return the serialized org-ml element
    oelement))

(cl-defmethod metarosetta-org-parse ((omatch metarosetta-org-match-output) oelement)
  "Parse the textual output match and its corresponding metadata defined by the org-ml headline element OELEMENT into the OMATCH instance."
  ;; Parse the base properties first
  (cl-call-next-method)
  ;; Set the match output key
  (let ((output-key (intern (org-ml-headline-get-node-property "OUTPUT-KEY" oelement))))
    (setf (slot-value omatch 'output-key) output-key))
  ;; Parse the target type as well as endpoint where the output match is placed to
  (let ((target-type (intern (org-ml-headline-get-node-property "TARGET-TYPE" oelement)))
        (target-endpoint (org-ml-headline-get-node-property "TARGET-ENDPOINT" oelement)))
    (setf (slot-value omatch 'target-type) target-type)
    (setf (slot-value omatch 'target-endpoint) target-endpoint)
    ;; Update the reverse index of sources
    (let ((index-entry (gethash target-endpoint
                                metarosetta-index-sources
                                `(,target-type . 0))))
      (puthash target-endpoint
               `(,(car index-entry) . ,(1+ (cdr index-entry)))
               metarosetta-index-sources)))
  ;; Parse the target section path defining the section within which the output match is placed
  (let ((target-section (org-ml-headline-get-node-property "TARGET-SECTION" oelement)))
    (setf (slot-value omatch 'target-section) target-section))
  ;; Return the parsed object
  omatch)

(cl-defmethod metarosetta-org-serialize ((omatch metarosetta-org-match-output))
  "Serialize the output match object OMATCH into an org-ml headline element."
  (let ((output-key (slot-value omatch 'output-key))
        (target-type (slot-value omatch 'target-type))
        (target-endpoint (slot-value omatch 'target-endpoint))
        (target-section (slot-value omatch 'target-section))
        ;; Serialize the base properties first
        (oelement (cl-call-next-method)))
    ;; Append the target endpoint as well as the section path within the headline element property drawer
    (setq oelement (->> (org-ml-headline-set-node-property "OUTPUT-KEY" (symbol-name output-key) oelement)
                        (org-ml-headline-set-node-property "TARGET-TYPE" (symbol-name target-type))
                        (org-ml-headline-set-node-property "TARGET-ENDPOINT" target-endpoint)
                        (org-ml-headline-set-node-property "TARGET-SECTION" target-section)))
    ;; Return the serialized org-ml element
    oelement))

(cl-defmethod metarosetta-org-parse ((oconfig metarosetta-org-config) otree)
  "Parse the configuration tree defined by the org-ml subtree OTREE into the OCONFIG instance."
  ;; Parse the root and output expressions respectively
  (let ((root-oelement (car otree))
        (output-oelements (cdr otree)))
    ;; Parse the single root expression object
    (setf (slot-value oconfig 'root-expression)
          (metarosetta-org-parse (metarosetta-org-expression-root) root-oelement))
    ;; Parse and map all the output expression objects contained within the configuration tree
    (setf (slot-value oconfig 'output-expressions)
          (mapcar (lambda (output-oelement)
                    (let ((output-oexpression (metarosetta-org-parse (metarosetta-org-expression-output) output-oelement)))
                      `(,(metarosetta-org-expression-key output-oexpression) . ,output-oexpression)))
                  output-oelements)))
  ;; Return the parsed object
  oconfig)

(cl-defmethod metarosetta-org-serialize ((oconfig metarosetta-org-config))
  "Serialize the configuration set object OCONFIG into a corresponding org-ml subtree."
  (let ((root-expression (slot-value oconfig 'root-expression))
        (output-expressions (slot-value oconfig 'output-expressions)))
    ;; Return the serialized org-ml subtree
    `(,(metarosetta-org-serialize root-expression)
      ,@(mapcar (lambda (output-expression-pair)
                  (let ((output-expression (cdr output-expression-pair)))
                    (metarosetta-org-serialize output-expression)))
                output-expressions))))

(cl-defmethod metarosetta-org-expression-root-add ((oexpression metarosetta-org-expression-root) raw-match-candidate source-filename)
  "Match the RAW-MATCH-CANDIDATE from SOURCE-FILENAME against the metalanguage expression in context of the provided OEXPRESSION Metarosetta configuration object. If successful, add the new match into configuration and return it. Otherwise return nil."
  (let* ((root-key (slot-value oexpression 'key))
         (mlexpression (slot-value oexpression 'mlexpression))
         (match-keychain (slot-value oexpression 'match-keychain))
         (parsed-match (metarosetta-process mlexpression raw-match-candidate))
         new-match)
    ;; Proceed only if match correctly processed
    (when parsed-match
      ;; Create the new match configuration object
      (setq new-match (metarosetta-org-match-original :raw-match raw-match-candidate
                                                      :parsed-match parsed-match
                                                      ;; Generate and save an equality-check-suitable hash
                                                      :match-hash (sxhash-equal raw-match-candidate)
                                                      :root-key root-key
                                                      ;; Generate a new serial for the new match
                                                      :serial (metarosetta-keychain-generate-key match-keychain)
                                                      :last-updated (current-time-string)
                                                      :op-type :created
                                                      :source-filename source-filename))
      ;; Add the match to the top of the matches association list, by its serial key
      (push `(,(metarosetta-org-match-serial new-match) . ,new-match)
            (slot-value oexpression 'matches))
      ;; Add the match to the reverse index by the source file name
      (let ((index-entry (gethash source-filename
                                  metarosetta-index-sources
                                  '(:root . 0))))
        (puthash source-filename
                 `(,(car index-entry) . ,(1+ (cdr index-entry)))
                 metarosetta-index-sources))
      ;; Return the newly added match
      new-match)))

(cl-defmethod metarosetta-org-expression-output-add ((oexpression metarosetta-org-expression-output) original-match)
  "Based on the provided ORIGINAL-MATCH, generate a new output match by interpolating the original's parsed data into the template in context of the OEXPRESSION Metarosetta configuration object, add the match into configuration and finally return it."
  (let* ((output-key (slot-value oexpression 'key))
         (mlexpression (slot-value oexpression 'mlexpression))
         (target-type (slot-value oexpression 'target-type))
         (connector (cdr (assq target-type metarosetta-index-connectors)))
         (target-endpoint-template (slot-value oexpression 'target-endpoint-template))
         (target-endpoint "")
         (target-section-template (slot-value oexpression 'target-section-template))
         (target-section "")
         (template (slot-value oexpression 'template))
         (raw-match (metarosetta-update mlexpression template (metarosetta-org-match-parsed original-match)))
         (parsed-match (metarosetta-process mlexpression raw-match))
         new-match)
    ;; Interpolate data into endpoint and section templates first
    (cl-labels ((interpolate-into-path (path)
                                       (when (and path
                                                  (> (length path) 0))
                                         ;; Use regular expression to split the path between static and dynamic parts
                                         (save-match-data
                                           (or (when (string-match "^\\(?1:\\(?:/?[^/]+\\)*\\)\\(?2:/\\)?\\$\\(?3:[[:word:]]+\\)\\(?4:.*\\)$" path)
                                                 ;; Replace the path element's key symbol with the corresponding data value
                                                 (let* ((static-path (match-string 1 path))
                                                        ;; Grab the possible separator, depending on key location within the path
                                                        (path-element-separator (match-string 2 path))
                                                        (path-element-key (intern (match-string 3 path)))
                                                        ;; By convention, path keys should reside just under the root metalanguage expression key
                                                        (path-element (cdr (assq path-element-key (cdr (metarosetta-org-match-parsed original-match)))))
                                                        (rest-of-path (match-string 4 path)))
                                                   ;; If path element is a list, convert it to path
                                                   (when (listp path-element)
                                                     (setq path-element (string-join path-element "/")))
                                                   ;; Recursively parse through all path element key symbols
                                                   (concat static-path
                                                           path-element-separator path-element
                                                           (interpolate-into-path rest-of-path))))
                                               ;; No dynamic path elements left
                                               path)))))
      (setq target-endpoint (interpolate-into-path target-endpoint-template)
            target-section (interpolate-into-path target-section-template)))
    ;; Create the new output match configuration object
    (setq new-match (metarosetta-org-match-output :raw-match raw-match
                                                  :parsed-match parsed-match
                                                  :match-hash (sxhash-equal raw-match)
                                                  :root-key (metarosetta-org-match-root-key original-match)
                                                  :serial (metarosetta-org-match-serial original-match)
                                                  :output-key output-key
                                                  :last-updated (current-time-string)
                                                  :op-type :created
                                                  :target-type target-type
                                                  :target-endpoint target-endpoint
                                                  :target-section target-section))
    ;; Add the match to the top of the matches association list, by its serial key
    (push `(,(metarosetta-org-match-serial new-match) . ,new-match)
          (slot-value oexpression 'matches))
    ;; Push out the new output match through the connector, and get the ultimate endpoint
    (setq target-endpoint (metarosetta-out-add connector
                                               target-endpoint target-section
                                               `(:root-key ,(metarosetta-org-match-root-key new-match)
                                                 :match-serial ,(metarosetta-org-match-serial new-match)
                                                 :output-key ,(metarosetta-org-match-output-key new-match)
                                                 :core-match ,(metarosetta-org-match-raw new-match))))
    ;; Update the endpoint within the match
    (metarosetta-org-match-output-target-endpoint-set new-match target-endpoint)
    ;; Add the match to the reverse index by the output endpoint name
    (let ((index-entry (gethash target-endpoint
                                metarosetta-index-sources
                                `(,target-type . 0))))
      (puthash target-endpoint
               `(,(car index-entry) . ,(1+ (cdr index-entry)))
               metarosetta-index-sources))
    ;; Return the newly added output match
    new-match))

(cl-defmethod metarosetta-org-config-add ((oconfig metarosetta-org-config) raw-match-candidate source-filename)
  "Try to add the RAW-MATCH-CANDIDATE from SOURCE-FILENAME into the OCONFIG Metarosetta org configuration. If it is successfully added, return the newly added original match property list containing the :root-key, :match-serial and :core-match. Otherwise return nil."
  (let* ((root-oexpression (slot-value oconfig 'root-expression))
         (output-oexpressions (slot-value oconfig 'output-expressions))
         (added-match (metarosetta-org-expression-root-add root-oexpression raw-match-candidate source-filename)))
    ;; Proceed only if match successfully added to root configuration object
    (when added-match
      ;; Pass along the added original match to all the configured output expressions
      (mapc (lambda (output-oexpression-pair)
              (let ((output-oexpression (cdr output-oexpression-pair)))
                (metarosetta-org-expression-output-add output-oexpression added-match)))
            output-oexpressions)
      ;; Return the newly added original match
      `(:root-key ,(metarosetta-org-match-root-key added-match)
        :match-serial ,(metarosetta-org-match-serial added-match)
        :core-match ,(metarosetta-org-match-raw added-match)))))

(cl-defmethod metarosetta-org-expression-root-update-from-source ((oexpression metarosetta-org-expression-root) possibly-updated-match-plist)
  "If the core match value of the POSSIBLY-UPDATED-MATCH-PLIST is indeed an updated version of a corresponding match in context of the OEXPRESSION Metarosetta root expression configuration object, update the match configuration object and return it. Otherwise return nil."
  (let* ((root-key (slot-value oexpression 'key))
         (mlexpression (slot-value oexpression 'mlexpression))
         (match-root-key (plist-get possibly-updated-match-plist :root-key))
         (match-serial (plist-get possibly-updated-match-plist :match-serial))
         (raw-match (plist-get possibly-updated-match-plist :core-match))
         (existing-match (cdr (assq match-serial (slot-value oexpression 'matches)))))
    ;; Proceed only if the match is in context of the root expression configuration
    (when (eq root-key match-root-key)
      (let ((match-hash (sxhash-equal raw-match)))
        ;; Check if the match has indeed been updated
        (when (not (eq match-hash
                       (metarosetta-org-match-hash existing-match)))
          ;; Match has been updated
          (let ((parsed-match (metarosetta-process mlexpression raw-match)))
            (when (not parsed-match)
              ;; The newly updated match is no longer syntactically compatible
              (error "Match update error: %s is no longer syntactically compatible with its metalanguage expression"
                     (metarosetta-id-serialize-match-identifier :root-key match-root-key
                                                                :match-serial match-serial)))
            ;; Store and process the newly updated match values
            (metarosetta-org-match-raw-set existing-match raw-match)
            (metarosetta-org-match-parsed-set existing-match parsed-match)
            (metarosetta-org-match-hash-set existing-match match-hash)
            (metarosetta-org-match-last-updated-set existing-match (current-time-string))
            (metarosetta-org-match-op-type-set existing-match :downloaded))
          ;; Return the updated match
          existing-match)))))

(cl-defmethod metarosetta-org-expression-output-update-from-original ((oexpression metarosetta-org-expression-output) original-match)
  "Based on the updated values of the provided ORIGINAL-MATCH, update the corresponding output match in context of the OEXPRESSION Metarosetta output expression configuration object and return the updated output match."
  (let* ((mlexpression (slot-value oexpression 'mlexpression))
         (target-type (slot-value oexpression 'target-type))
         (connector (cdr (assq target-type metarosetta-index-connectors)))
         (template (slot-value oexpression 'template))
         (existing-match (cdr (assq (metarosetta-org-match-serial original-match)
                                    (slot-value oexpression 'matches))))
         (target-endpoint (metarosetta-org-match-output-target-endpoint existing-match))
         (target-section (metarosetta-org-match-output-target-section existing-match)))
    ;; Update and process the newly updated values
    (metarosetta-org-match-raw-set existing-match
                                   (metarosetta-update mlexpression template (metarosetta-org-match-parsed original-match)))
    (metarosetta-org-match-parsed-set existing-match
                                      (metarosetta-process mlexpression (metarosetta-org-match-raw existing-match)))
    (metarosetta-org-match-hash-set existing-match
                                    (sxhash-equal (metarosetta-org-match-raw existing-match)))
    (metarosetta-org-match-last-updated-set existing-match (current-time-string))
    (metarosetta-org-match-op-type-set existing-match :uploaded)
    ;; Propagate all the updates through the connector
    (metarosetta-out-update connector
                            target-endpoint target-section
                            `(:root-key ,(metarosetta-org-match-root-key existing-match)
                              :match-serial ,(metarosetta-org-match-serial existing-match)
                              :output-key ,(metarosetta-org-match-output-key existing-match)
                              :core-match ,(metarosetta-org-match-raw existing-match)))
    ;; Return the updated match
    existing-match))

(cl-defmethod metarosetta-org-config-update-from-source ((oconfig metarosetta-org-config) possibly-updated-original-match-plist)
  "Based on the possibly updated values of the POSSIBLY-UPDATED-ORIGINAL-MATCH-PLIST, update the corresponding original as well as output match objects in context of the OCONFIG Metarosetta org configuration. If the match is indeed updated, return the updated property list of the original match. Otherwise return nil."
  (let* ((root-oexpression (slot-value oconfig 'root-expression))
         (output-oexpressions (slot-value oconfig 'output-expressions))
         (updated-match (metarosetta-org-expression-root-update-from-source root-oexpression possibly-updated-original-match-plist)))
    ;; Proceed only if original match actually updated
    (when updated-match
      ;; Pass along the updated original match to all the configured output expressions
      (mapc (lambda (output-oexpression-pair)
              (let ((output-oexpression (cdr output-oexpression-pair)))
                (metarosetta-org-expression-output-update-from-original output-oexpression updated-match)))
            output-oexpressions)
      ;; Return the updated original match
      `(:root-key ,(metarosetta-org-match-root-key updated-match)
        :match-serial ,(metarosetta-org-match-serial updated-match)
        :core-match ,(metarosetta-org-match-raw updated-match)))))

(cl-defmethod metarosetta-org-expression-root-update-from-output ((oexpression metarosetta-org-expression-root) output-match)
  "Based on the updated values of the provided OUTPUT-MATCH, update the corresponding original match in context of the OEXPRESSION Metarosetta root expression configuration object and return the updated original match."
  (let* ((mlexpression (slot-value oexpression 'mlexpression))
         (existing-match (cdr (assq (metarosetta-org-match-serial output-match)
                                    (slot-value oexpression 'matches))))
         (source-filename (slot-value existing-match 'source-filename)))
    ;; Update and process the newly updated values
    (metarosetta-org-match-raw-set existing-match
                                   (metarosetta-update mlexpression
                                                       (metarosetta-org-match-raw existing-match)
                                                       (metarosetta-org-match-parsed output-match)))
    (metarosetta-org-match-parsed-set existing-match
                                      (metarosetta-process mlexpression (metarosetta-org-match-raw existing-match)))
    (metarosetta-org-match-hash-set existing-match
                                    (sxhash-equal (metarosetta-org-match-raw existing-match)))
    (metarosetta-org-match-last-updated-set existing-match (current-time-string))
    (metarosetta-org-match-op-type-set existing-match :uploaded)
    ;; Propagate the updates to the source file itself
    (metarosetta-source-update source-filename
                               `(:root-key ,(metarosetta-org-match-root-key existing-match)
                                 :match-serial ,(metarosetta-org-match-serial existing-match)
                                 :core-match ,(metarosetta-org-match-raw existing-match)))
    ;; Return the updated match
    existing-match))

(cl-defmethod metarosetta-org-expression-output-update-from-endpoint ((oexpression metarosetta-org-expression-output) possibly-updated-match-plist)
  "If the core match value of the POSSIBLY-UPDATED-MATCH-PLIST is indeed an updated version of a corresponding match in context of the OEXPRESSION Metarosetta output expression configuration object, update the match configuration object and return it. Otherwise return nil."
  (let* ((output-key (slot-value oexpression 'key))
         (mlexpression (slot-value oexpression 'mlexpression))
         (match-root-key (plist-get possibly-updated-match-plist :root-key))
         (match-output-key (plist-get possibly-updated-match-plist :output-key))
         (match-serial (plist-get possibly-updated-match-plist :match-serial))
         (raw-match (plist-get possibly-updated-match-plist :core-match))
         (existing-match (cdr (assq match-serial (slot-value oexpression 'matches)))))
    ;; Proceed only if the match is in context of the output expression configuration
    (when (eq output-key match-output-key)
      (let ((match-hash (sxhash-equal raw-match)))
        ;; Check if the hash has been changed
        (when (not (eq match-hash
                       (metarosetta-org-match-hash existing-match)))
          ;; Match has indeed been updated
          (let ((parsed-match (metarosetta-process mlexpression raw-match)))
            (when (not parsed-match)
              (error "Output match update error: %s is no longer syntactically compatible with its metalanguage expression"
                     (metarosetta-id-serialize-match-identifier :root-key match-root-key
                                                                :match-serial match-serial
                                                                :output-key match-output-key)))
            ;; Store and process the newly updated match values
            (metarosetta-org-match-raw-set existing-match raw-match)
            (metarosetta-org-match-parsed-set existing-match parsed-match)
            (metarosetta-org-match-hash-set existing-match match-hash)
            (metarosetta-org-match-last-updated-set existing-match (current-time-string))
            (metarosetta-org-match-op-type-set existing-match :downloaded))
          ;; Return the updated output match
          existing-match)))))

(cl-defmethod metarosetta-org-config-update-from-endpoint ((oconfig metarosetta-org-config) possibly-updated-output-match-plist)
  "Based on the possibly updated values of the POSSIBLY-UPDATED-OUTPUT-MATCH-PLIST, update the corresponding output match in addition to its upstream original, as well as all the other eventually co-existing output match objects in context of the OCONFIG Metarosetta org configuration. If the match is indeed updated, return the updated property list of the output match. Othwerwise return nil."
  (let* ((output-key (plist-get possibly-updated-output-match-plist :output-key))
         (root-oexpression (slot-value oconfig 'root-expression))
         (output-oexpressions (slot-value oconfig 'output-expressions))
         (contextual-output-oexpression (cdr (assq output-key output-oexpressions)))
         (updated-output-match (metarosetta-org-expression-output-update-from-endpoint contextual-output-oexpression possibly-updated-output-match-plist)))
    ;; Proceed only if the output match in context is actually updated
    (when updated-output-match
      ;; First update the original match
      (let ((updated-original-match (metarosetta-org-expression-root-update-from-output root-oexpression updated-output-match)))
        ;; Now pass along the updated original match to any configured output expressions other than the one in context
        (mapc (lambda (output-oexpression-pair)
                (let ((output-oexpression-key (car output-oexpression-pair))
                      (output-oexpression (cdr output-oexpression-pair)))
                  (when (not (eq output-oexpression-key output-key))
                    (metarosetta-org-expression-output-update-from-original output-oexpression updated-original-match))))
              output-oexpressions))
      ;; Return the updated output match in context
      `(:root-key ,(metarosetta-org-match-root-key updated-output-match)
        :match-serial ,(metarosetta-org-match-serial updated-output-match)
        :output-key ,(metarosetta-org-match-output-key updated-output-match)
        :core-match ,(metarosetta-org-match-raw updated-output-match)))))

(defun metarosetta-source-read (source-file number-of-matches)
  "From the SOURCE-FILE, find and return NUMBER-OF-MATCHES of the tracked Metarosetta metalanguage match instances in their full form."
  (let ((match-id-re (metarosetta-id-generate-re))
        (matches '()))
    (with-temp-buffer
      (insert-file-contents source-file)
      ;; Start search from the beginning
      (goto-char (point-min))
      ;; Search for the specified number of matches
      (while (> number-of-matches 0)
        (save-match-data
          ;; Find the next match by searching for the identifier pattern
          (re-search-forward (concat match-id-re "$"))
          ;; Extract the raw match
          (let ((raw-match (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
            ;; Push the parsed match to the resulting list
            (push (metarosetta-id-parse-match raw-match)
                  matches)))
        ;; Decrease the number of matches left to read
        (setq number-of-matches (1- number-of-matches))))
    ;; Return the matches
    matches))

(defun metarosetta-source-update (source-file match-plist)
  "Given the original :core-match, along with its :root-key and :match-serial from within MATCH-PLIST, update the match in-place within its SOURCE-FILE."
  (let ((match-identifier (apply 'metarosetta-id-serialize-match-identifier match-plist))
        (raw-match (apply 'metarosetta-id-serialize-match match-plist)))
    (with-temp-file source-file
      (insert-file-contents source-file)
      ;; Star search from the beginning
      (goto-char (point-min))
      ;; Find the line of the match in context
      (re-search-forward (concat (regexp-quote match-identifier) "$"))
      ;; Place point at correct indentation
      (beginning-of-line)
      (re-search-forward "^[[:blank:]]*"
                         ;; Limit search to line at point
                         (line-end-position)
                         ;; In case of no indentation just move on
                         t)
      ;; Delete the current match string
      (delete-region (point) (line-end-position))
      ;; Insert the new match string
      (insert raw-match))
    ;; Return affirmatively
    t))

(defclass metarosetta-out ()
  ((syntax-type
    :initarg :syntax-type
    :type symbol
    :documentation "A symbol defining the syntax type the output connector in context conforms to."
    :reader metarosetta-out-syntax-type))
  "The foundational output connector object all Metarosetta connectors should extend on."
  :abstract t)

(cl-defgeneric metarosetta-out-add ((connector metarosetta-out) endpoint section output-match-plist)
  "Via the output CONNECTOR, add the output match defined by the OUTPUT-MATCH-PLIST to the provided ENDPOINT within the specified SECTION. If successful, return the endpoint where added. In cases of symlinks or link forwarding setups, this can vary from ENDPOINT.")

(cl-defgeneric metarosetta-out-read ((connector metarosetta-out) endpoint number-of-matches)
  "Via the output CONNECTOR, read NUMBER-OF-MATCHES from the specified ENDPOINT across all sections.")

(cl-defgeneric metarosetta-out-update ((connector metarosetta-out) endpoint section output-match-plist)
  "Via the output CONNECTOR, update the output match defined by the OUTPUT-MATCH-PLIST located in the specified ENDPOINT within a specific SECTION. If successful, return the endpoint where updated. In cases of symlinks or link forwarding setups, this can vary from ENDPOINT.")

(defclass metarosetta-out-to-structured-text (metarosetta-out)
  ((heading-mark
    :initarg :heading-mark
    :type string
    :documentation "A string representing the heading mark in a given structure syntax.")
   (item-mark
    :initarg :item-mark
    :type string
    :documentation "A string representing the list item mark in a given structure syntax."))
  "A Metarosetta output connector object which serves as an agnostic base for various output file connectors dealing with structured text, such as Markdown or org-mode.")

(cl-defmethod metarosetta-out-to-structured-text-goto-section ((connector metarosetta-out-to-structured-text) section)
  "The structured text output CONNECTOR's helper method to set the point, in context of the current buffer, to the beginning of the target section defined by the SECTION path. Return a cons cell containing the starting and ending points, respectively."
  (let ((heading-mark (slot-value connector 'heading-mark))
        section-heading-mark
        (headings (split-string section "/")))
    ;; Assuming the context of a current buffer, start search from the beginning
    (goto-char (point-min))
    ;; Navigate to targeted section and get the section's heading mark
    (setq section-heading-mark
          (cl-reduce (lambda (last-heading-mark heading)
                       ;; Compile the current heading mark, based on heading depth
                       (let* ((current-heading-mark (concat last-heading-mark heading-mark))
                              (full-heading (concat current-heading-mark " " heading)))
                         ;; All sections along the path are expected to be in place, i.e. none are implicitly created
                         (search-forward full-heading)
                         ;; Push the current heading mark to the next iteration
                         current-heading-mark))
                     headings
                     :initial-value ""))
    ;; Recursively get the end position of the section at point, and return both points
    `(,(point) . ,(cl-labels ((section-end-position (current-heading-mark)
                                                    (when (> (length current-heading-mark) 0)
                                                      (save-excursion
                                                        (let ((parent-end-position (section-end-position (substring current-heading-mark 0 -1))))
                                                          (or (re-search-forward (concat "^[[:blank:]]*"
                                                                                         (regexp-quote current-heading-mark))
                                                                                 parent-end-position
                                                                                 t)
                                                              parent-end-position))))))
                    (section-end-position section-heading-mark)))))

(cl-defmethod metarosetta-out-add ((connector metarosetta-out-to-structured-text) endpoint section output-match-plist)
  "Via the structured text output CONNECTOR, append the output match defined by the OUTPUT-MATCH-PLIST to the ENDPOINT file within the specified SECTION. If successful, return the actual endpoint where added. In cases of symlinks, this can vary from ENDPOINT."
  (let* (section-bounds
         (item-mark (slot-value connector 'item-mark))
         (full-item (concat item-mark " " (apply 'metarosetta-id-serialize-match output-match-plist))))
    ;; Follow through the symlink, if such, and get the absolute file name
    (setq endpoint (file-truename endpoint))
    (with-temp-file endpoint
      (insert-file-contents endpoint)
      ;; Navigate to section in context, and get section bounds
      (setq section-bounds (metarosetta-out-to-structured-text-goto-section connector section))
      ;; Navigate to the end of the list within the target section
      (while (search-forward item-mark
                             ;; Limit the search to the encompassing section only
                             (cdr section-bounds)
                             ;; When search eventually fails, just return nil and exit the loop
                             t))
      ;; Insert the new list item after the last one
      (end-of-line)
      (newline-and-indent)
      (insert full-item))
    ;; Return with the endpoint file name
    endpoint))

(cl-defmethod metarosetta-out-read ((connector metarosetta-out-to-structured-text) endpoint number-of-matches)
  "Via the structured text output CONNECTOR, read NUMBER-OF-MATCHES from the specified ENDPOINT across all sections."
  (let ((item-mark (slot-value connector 'item-mark))
        (match-id-re (metarosetta-id-generate-re))
        (matches '()))
    (with-temp-buffer
      (insert-file-contents endpoint)
      ;; Start search from the beginning
      (goto-char (point-min))
      ;; Search for the specified number of matches
      (while (> number-of-matches 0)
        (save-match-data
          ;; Find the next match line by searching for the identifier pattern
          (re-search-forward (concat match-id-re "$"))
          ;; Set point at the beginning of the actual match
          (beginning-of-line)
          (search-forward (concat item-mark " ")
                          (line-end-position))
          ;; Extract the raw match
          (let ((raw-match (buffer-substring-no-properties (point) (line-end-position))))
            ;; Push the parsed match to the resulting list
            (push (metarosetta-id-parse-match raw-match)
                  matches)))
        ;; Decrease the number of matches left to read
        (setq number-of-matches (1- number-of-matches))))
    ;; Return the matches
    matches))

(cl-defmethod metarosetta-out-update ((connector metarosetta-out-to-structured-text) endpoint section output-match-plist)
  "Via the structured text output CONNECTOR, update the match defined by the OUTPUT-MATCH-PLIST located in the specified ENDPOINT within a specific SECTION. If successful, return the actual endpoint where added. In cases of symlinks, this can vary from ENDPOINT."
  (let (section-bounds
        (item-mark (slot-value connector 'item-mark))
        (match-identifier (apply 'metarosetta-id-serialize-match-identifier output-match-plist))
        (raw-match (apply 'metarosetta-id-serialize-match output-match-plist)))
    ;; Follow through the symlink, if such, and get the absolute file name
    (setq endpoint (file-truename endpoint))
    (with-temp-file endpoint
      (insert-file-contents endpoint)
      ;; Navigate to section in context, and get section bounds
      (setq section-bounds (metarosetta-out-to-structured-text-goto-section connector section))
      ;; Search for the item line containing the match ID
      (re-search-forward (concat (regexp-quote match-identifier) "$")
                      ;; Limit the search to the encompassing section only
                      (cdr section-bounds))
      ;; Set point at the beginning of the item's line
      (beginning-of-line)
      ;; Move past the item mark
      (search-forward (concat item-mark " ")
                      (line-end-position))
      ;; Delete the current item
      (delete-region (point) (line-end-position))
      ;; Insert the updated match at point
      (insert raw-match))
    ;; Return with the endpoint file name
    endpoint))

(defun metarosetta-register-connectors (&rest connectors)
  "Register one or more Metarosetta output CONNECTORS."
  (setq metarosetta-index-connectors
        (mapcar (lambda (connector)
                  ;; Check if the connector is of the proper class
                  (when (not (object-of-class-p connector 'metarosetta-out))
                    (error "Invalid output connector error: All output connectors must inherit from the metarosetta-out class"))
                  ;; Create an alist element for the connector indexed by its type symbol
                  `(,(metarosetta-out-syntax-type connector) . ,connector))
                connectors)))

(defun metarosetta-load (configuration-directory)
  "Load all the Metarosetta org configuration files located within the provided CONFIGURATION-DIRECTORY."
  ;; First set the configuration directory
  (setq metarosetta-configuration-directory configuration-directory)
  ;; Load, parse and initialize all Metarosetta configurations
  (let ((configuration-files (directory-files configuration-directory
                                              ;; Get the full path
                                              'full
                                              ;; Return only org files
                                              ".+\\.org$")))
    ;; Parse all configuration files and add them to the global index by their respective keys
    (setq metarosetta-index-configurations
          (mapcar (lambda (configuration-file)
                    (with-temp-buffer
                      (insert-file-contents configuration-file)
                      (let ((oconfig (metarosetta-org-parse (metarosetta-org-config :source-filename configuration-file)
                                                            (org-ml-parse-subtrees 'all))))
                        ;; Repport successful load
                        (message "Metarosetta successfully loaded %s" configuration-file)
                        ;; Return configuration pair
                        `(,(metarosetta-org-config-key oconfig) . ,oconfig))))
                  configuration-files))))

(defun metarosetta-save ()
  "Save all the indexed Metarosetta org configuration files to the set configuration directory."
  ;; Save each configuration
  (mapc (lambda (configuration-pair)
          (let* ((configuration (cdr configuration-pair))
                 (configuration-file (metarosetta-org-config-source-filename configuration)))
            (with-temp-file configuration-file
              (insert-file-contents configuration-file)
              ;; Skip over any non-heading data
              (goto-char (point-min))
              (re-search-forward "^*")
              (goto-char (1- (point)))
              ;; Delete any currently saved configuration data
              (delete-region (point) (point-max))
              ;; Insert the serialized org data
              (insert (string-join (mapcar 'org-ml-to-string
                                           (metarosetta-org-serialize configuration))
                                   ;; Join all headline elements directly
                                   "")))
            ;; Report successful save
            (message "Metarosetta successfully saved %s" configuration-file)))
        metarosetta-index-configurations))

(defun metarosetta-match ()
  "Try to add the text from line at point to any compatible Metarosetta configuration currently active and tracking."
  (interactive)
  (let ((source-filename (buffer-file-name)))
    ;; If the current buffer is visiting a valid file, grab the current line's text to match
    (when source-filename
      (let ((text-to-match (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
        ;; Only proceed if there's text to actually process
        (when (> (length text-to-match) 0)
          (let ((configurations metarosetta-index-configurations))
            ;; Iterate over all indexed configurations until one matches
            (while (and configurations
                        (not (let* ((configuration (cdar configurations))
                                    ;; Try to match the current configuration
                                    (match-plist (metarosetta-org-config-add configuration text-to-match source-filename)))
                               ;; If matched, update the text at point
                               (when match-plist
                                 ;; Place point at correct indentation
                                 (beginning-of-line)
                                 (re-search-forward "^[[:blank:]]*"
                                                    ;; Limit seatch to line at point
                                                    (line-end-position)
                                                    ;; In case of no indentation, move on
                                                    t)
                                 ;; Delete the current text
                                 (delete-region (point) (line-end-position))
                                 ;; Insert the full match string
                                 (insert (apply 'metarosetta-id-serialize-match match-plist))
                                 ;; Report the successful match to the user
                                 (message "Metarosetta successfully matched text at point.")
                                 ;; Explicitly return affirmatively
                                 t))))
              ;; Move on to the next configuration
              (setq configurations (cdr configurations)))
            ;; Report if no match found
            (when (not configurations)
              (message "Metarosetta failed to match text at point."))))))))

(defun metarosetta-sync ()
  "Synchronize all the possibly updated Metarosetta match instances within the current buffer."
  (interactive)
  (let ((source-filename (buffer-file-name)))
    ;; If the current buffer is visiting a valid file, check the reverse index for tracked matches
    (when source-filename
      (let ((source-index-item (gethash source-filename
                                        metarosetta-index-sources)))
        ;; Only proceed if file in context is indexed
        (when source-index-item
          (let* ((source-type (car source-index-item))
                 (number-of-matches (cdr source-index-item))
                 ;; Fetch all the matches depending on source type
                 (matches (if (eq source-type :root)
                              ;; Fetch source matches
                              (metarosetta-source-read source-filename number-of-matches)
                            ;; Fetch output matches using the corresponding connector
                            (let ((connector (cdr (assq source-type metarosetta-index-connectors))))
                              (metarosetta-out-read connector source-filename number-of-matches)))))
            ;; Iterate over each match and update configuration if needed
            (mapc (lambda (match-plist)
                    (let* ((configuration (cdr (assq (plist-get match-plist :root-key)
                                                     metarosetta-index-configurations)))
                           ;; Depending on match type, update downstream or upstream
                           (updated-match-plist (if (not (plist-get match-plist :output-key))
                                                    ;; An original match, update downstream
                                                    (metarosetta-org-config-update-from-source configuration match-plist)
                                                  ;; An output match, update upstream
                                                  (metarosetta-org-config-update-from-endpoint configuration match-plist))))
                      ;; Report if the match is indeed updated
                      (when updated-match-plist
                        (message "Metarosetta successfully updated match %s."
                                 (apply 'metarosetta-id-serialize-match-identifier updated-match-plist)))))
                  matches)))))))

(defun metarosetta-jump ()
  "With current buffer visiting an output endpoint, and current point set on a valid output match, jump to the original match at its source file. If a buffer visiting the source exists, switch there, otherwise create a new buffer."
  (interactive)
  (let ((match-id-plist (save-excursion
                          ;; Start identifier search at the beginning of the line
                          (beginning-of-line)
                          (save-match-data
                            ;; Search for the identifier pattern and parse it
                            (and (re-search-forward (concat (metarosetta-id-generate-re) "$")
                                                    ;; Limit search to current line only
                                                    (line-end-position)
                                                    ;; Return nil if no identifier found
                                                    t)
                                 ;; Identifier pattern found, parse it
                                 (metarosetta-id-parse-match-identifier (match-string 0)))))))
    ;; If match identifier found, look up the source file name
    (when match-id-plist
      (let* ((configuration (cdr (assq (plist-get match-id-plist :root-key)
                                       metarosetta-index-configurations)))
             (source-filename (metarosetta-org-config-original-match-source-filename configuration
                                                                                     (plist-get match-id-plist :match-serial))))
        ;; Jump to source filename buffer
        (find-file source-filename)
        ;; Set point to original match in context
        (goto-char (point-min))
        (let ((source-match-id (metarosetta-id-serialize-match-identifier :root-key (plist-get match-id-plist :root-key)
                                                                          :match-serial (plist-get match-id-plist :match-serial))))
          (save-match-data
            (re-search-forward (concat (regexp-quote source-match-id) "$"))))))))

(provide 'metarosetta)

;;; metarosetta.el ends here
