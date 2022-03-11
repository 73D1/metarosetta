;;; metarosetta.el --- Extract, structure and transpile contextual data from conventional text -*- lexical-binding: t; -*-

;; Author: Bruno Tedeschi <me@btedeschi.com>

;;; Commentary:

;; An Emacs package introducing support for so-called metalanguage expressions.
;; 
;; Like regular expressions, but in English, these enable defining arbitrary textual conventions enabling capturing and parsing through matching textual snippets from within the Emacs editing environment.
;; 
;; Matches can be set to automatically produce machine-digestible data structures of parsed information, or to "transpile" the given data to a different textual convention, also defined by another, but data-compatible, metalanguage expression.
;; 
;; For details and language specification, in addition to examples and use cases, please refer to the original package documentation and source org file.

;;; Code:

(require 'eieio)
(require 'eieio-base)
(require 'org)

(defclass mrosetta-keychain ()
  ((lastkey
    :initarg :lastkey
    :initform '0
    :type number
    :documentation "The last key generated and assigned within the context of a single keychain instance."
    :reader mrosetta-keychain-lastkey))
  "A key generator helper class.")

(cl-defmethod mrosetta-keychain-generate-key ((keychain mrosetta-keychain))
  "Generate a new key within a provided KEYCHAIN."
  (let ((key (+ 1 (slot-value keychain 'lastkey))))
    (setf (slot-value keychain 'lastkey) key)))

(defclass mrosetta-mlexpression ()
  (
   (mldefinition
    :initarg :mldefinition
    :type list
    :documentation "The metalanguage-specified definition of the expression in context."
    :reader mrosetta-mlexpression-mldefinition)
   (extype
    :type symbol
    :documentation "A symbol specifying the type of the encompassing expression instance. Can be either a :literal, :match or :fractal."
    :reader mrosetta-mlexpression-extype)
   (fractals
    :initform '()
    :type list
    :documentation "A list of mrosetta-expression instances contained within the encompassing expression instance."
    :reader mrosetta-mlexpression-fractals)
   (rkeychain
    :initarg :rkeychain
    :initform (mrosetta-keychain)
    :type mrosetta-keychain
    :documentation "The regex keychain instance managing keys for the encompassing expression tree."
    :reader mrosetta-mlexpression-rkeychain)
   (regex
    :type string
    :documentation "The compiled regular expression of the expression in context."
    :reader mrosetta-mlexpression-regex)
   (regex-key
    :type number
    :documentation "The regex matching group key for the encompassing expression instance."
    :reader mrosetta-mlexpression-regex-key)
   (rinstance
    :type string
    :documentation "The compiled regular expression matching a single instance of a possibly plural-matching expression."
    :reader mrosetta-mlexpression-rinstance)
   (rinstance-key
    :type number
    :documentation "The regex group key for matching a single instance of a possibly plural-matching metalanguage expression in context."
    :reader mrosetta-mlexpression-rinstance-key)
   (rbase
    :type string
    :documentation "The regular expression used as a foundational base in compilation of the match-extracting regular expression."
    :reader mrosetta-mlexpression-rbase)
   (rmatch
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression of the encompassing expression's textual match."
    :reader mrosetta-mlexpression-rmatch)
   (rmatch-key
    :initform 'nil
    :type (or null number)
    :documentation "The regex group key for the encompassing expression's output value."
    :reader mrosetta-mlexpression-rmatch-key)
   (rprefix
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression matching a specified prefix of the encompassing expression instance. Either a regex string or nil."
    :reader mrosetta-mlexpression-rprefix)
   (rsuffix
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression matching a specified suffix of the encompassing expression instance. Either a regex string or nil."
    :reader mrosetta-mlexpression-rsuffix)
   (left-rboundary
    :initform 'nil
    :type (or null string)
    :documentation "The left regex-specific boundary defining the beginning of the match."
    :reader mrosetta-mlexpression-left-rboundary)
   (right-rboundary
    :initform 'nil
    :type (or null string)
    :documentation "The right regex-specific boundary defining the end of the match."
    :reader mrosetta-mlexpression-right-rboundary)
   (rbuffer
    :initform "[[:blank:]]*"
    :type string
    :documentation "The regular expression matching buffer characters surrounding the encompassing expression."
    :reader mrosetta-mlexpression-rbuffer)
   (rbuffer-key
    :type number
    :documentation "The regex group key for the encompassing expression's left buffer match."
    :reader mrosetta-mlexpression-rbuffer-key)
   (key
    :initform 'nil
    :type (or null symbol)
    :documentation "The property key to which the expression output value is assigned, if any. Either a string or nil."
    :reader mrosetta-mlexpression-key)
   (is-uppercase
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches only uppercase words. Either non-nil or nil."
    :reader mrosetta-mlexpression-is-uppercase)
   (is-capitalized
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches only capitalized words. Either non-nil or nil."
    :reader mrosetta-mlexpression-is-capitalized)
   (match-prefix
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the prefix all possible expression matches should have, if any. Either a string or nil."
    :reader mrosetta-mlexpression-match-prefix)
   (match-suffix
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the suffix all possible expression matches should have, if any. Either a string or nil."
    :reader mrosetta-mlexpression-match-suffix)
   (match-substring
    :initform 'nil
    :type (or null string)
    :documentation "Specifies a specific substring all possible expression matches should contain, if any. Either a string or nil."
    :reader mrosetta-mlexpression-match-substring)
   (match-literal
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the literal string that the expression maches exclusively. Either a string or nill."
    :reader mrosetta-mlexpression-match-literal)
   (is-contextual
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is matched elastically depending on neighboring elements. Either non-nil or nil."
    :reader mrosetta-mlexpression-is-contextual)
   (modifier
    :initform 'nil
    :type (or null symbol)
    :documentation "Specifies a symbol referencing a stored modifier function, if any. Either a symbol or nil."
    :reader mrosetta-mlexpression-modifier)
   (is-optional
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is optional to match within input text. Either non-nil or nil."
    :reader mrosetta-mlexpression-is-optional)
   (should-ignore
    :initform 'nil
    :documentation "Specifies whether the encompassing expression should be matched but disregarded in output. Either non-nil or nil."
    :reader mrosetta-mlexpression-should-ignore)
   (is-plural
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches plural values or just a single one. Either nil or non-nil."
    :reader mrosetta-mlexpression-is-plural)
  )
  "The Metarosetta Expression object which defines a contextual translational expression used for matching, parsing and structuring data from within conventional text.")

(defvar mrosetta-mlsyntax '())

(cl-defmethod mrosetta-parse-literal ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the :right arg content within ARGS as a literal quote into the MLEXPRESSION instance in context."
  (let ((literal-quote (plist-get args :right)))
    (when (eq literal-quote nil)
      (error "Metalanguage syntax error: Literal expression without quoted content"))
    (setf (slot-value mlexpression 'extype) :literal)
    (setf (slot-value mlexpression 'rbase) (regexp-quote literal-quote))
    (setf (slot-value mlexpression 'match-literal) literal-quote))
  (plist-put args :right nil))

(push '(literal . mrosetta-parse-literal) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-word ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'extype) :match)
  (setf (slot-value mlexpression 'left-rboundary) "\\<")
  (setf (slot-value mlexpression 'rbase) "[[:word:]]+")
  (setf (slot-value mlexpression 'right-rboundary) "\\>")
  args)

(push '(word . mrosetta-parse-word) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-word-uppercase ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse an uppercase word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setq args (apply 'mrosetta-parse-word mlexpression args))
  (setf (slot-value mlexpression 'rbase) "[A-Z0-9]+")
  (setf (slot-value mlexpression 'is-uppercase) t)
  args)

(push '(WORD . mrosetta-parse-word-uppercase) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-word-capitalized ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a capitalized word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setq args (apply 'mrosetta-parse-word mlexpression args))
  (setf (slot-value mlexpression 'rbase) "[A-Z0-9][a-z0-9]+")
  (setf (slot-value mlexpression 'is-capitalized) t)
  args)

(push '(Word . mrosetta-parse-word-capitalized) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-word-plurality ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a plural words expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setq args (apply 'mrosetta-parse-word mlexpression args))
  (setf (slot-value mlexpression 'is-plural) t)
  args)

(push '(words . mrosetta-parse-word-plurality) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-paragraph ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a paragraph epxression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'extype) :match)
  (setf (slot-value mlexpression 'rbase) ".+?")
  args)

(push '(paragraph . mrosetta-parse-paragraph) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-paragraph-plurality ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a plural paragraph expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setq args (apply 'mrosetta-parse-paragraph mlexpression args))
  (setf (slot-value mlexpression 'is-plural) t)
  args)

(push '(paragraphs . mrosetta-parse-paragraph-plurality) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-substring ((mlexpression mrosetta-mlexpression) &rest args)
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

(push '(with . mrosetta-parse-substring) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-prefix ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse quoted text from :left arg within ARGS as matching element prefix into the MLEXPRESSION instance in context."
  (let ((prefix-quote (plist-get args :left)))
    (when (eq prefix-quote nil)
      (error "Metalanguage syntax error: Prefix match expression without quoted content"))
    (setf (slot-value mlexpression 'rprefix) (regexp-quote prefix-quote))
    (setf (slot-value mlexpression 'match-prefix) prefix-quote))
  (plist-put args :left nil))

(push '(prefixed . mrosetta-parse-prefix) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-suffix ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse quoted text from :left arg within ARGS as matching element suffix into the MLEXPRESSION instance in context."
  (let ((suffix-quote (plist-get args :left)))
    (when (eq suffix-quote nil)
      (error "Metalanguage syntax error: Suffix match expression without quoted content"))
    (setf (slot-value mlexpression 'rsuffix) (regexp-quote suffix-quote))
    (setf (slot-value mlexpression 'match-suffix) suffix-quote))
  (plist-put args :left nil))

(push '(suffixed . mrosetta-parse-suffix) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-contextual ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the contextual specifier into the MLEXPRESSION instance in context. This function utilizes no ARGS."
  (setf (slot-value mlexpression 'is-contextual) t)
  args)

(push '(contextual . mrosetta-parse-contextual) mrosetta-mlsyntax)

(defvar mrosetta-mlsyntax-modifiers '())

(push '(uppercase . upcase) mrosetta-mlsyntax-modifiers)

(push '(lowercase . downcase) mrosetta-mlsyntax-modifiers)

(cl-defmethod mrosetta-parse-modifier ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the modifier symbol from :right arg within ARGS into the MLEXPRESSION instance in context."
  (let ((modifier-symbol (plist-get args :right)))
    (when (eq modifier-symbol nil)
      (error "Metalanguage syntax error: Modifier expression without contextual argument symbol"))
    (setf (slot-value mlexpression 'modifier)
          (cdr (assq modifier-symbol mrosetta-mlsyntax-modifiers))))
  (plist-put args :right nil))

(push '(to . mrosetta-parse-modifier) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-optionality ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse expression optionality into the MLEXPRESSION instance in context. This function utilizes no ARGS."
  (setf (slot-value mlexpression 'is-optional) t)
  args)

(push '(optional . mrosetta-parse-optionality) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-key ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the key symbol from :right arg within ARGS into the MLEXPRESSION instance in context."
  (let ((key-symbol (plist-get args :right)))
    (when (eq key-symbol nil)
      (error "Metalanguage syntax error: Key assignment without contextual key symbol"))
    (setf (slot-value mlexpression 'key) key-symbol))
  (plist-put args :right nil))

(push '(as . mrosetta-parse-key) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-ignorable ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the ignorable property into the MLEXPRESSION instance in context. This function utilizes no ARGS."
  (setf (slot-value mlexpression 'should-ignore) t)
  args)

(push '(ignorable . mrosetta-parse-ignorable) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-list ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the list expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-plural) t)
  args)

(push '(list . mrosetta-parse-list) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-element ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the element expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-plural) nil)
  args)

(push '(element . mrosetta-parse-element) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-of ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the sub-expression from :right arg within ARGS into the MLEXPRESSION instance in context."
  (let ((sub-expression (plist-get args :right)))
    (when (or (eq sub-expression nil) (nlistp sub-expression))
      (error "Metalanguage syntax error: Sub-expression assignment without contextual expression"))
    (mrosetta-parse mlexpression :sub sub-expression))
  (plist-put args :right nil))

(push '(of . mrosetta-parse-of) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse ((mlexpression mrosetta-mlexpression) &rest args)
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
        (let ((leftout-args (funcall (cdr (assq element mrosetta-mlsyntax)) mlexpression :left larg :right rarg)))
          (setq larg nil)
          (when (eq (plist-get leftout-args :right) nil)
            (pop mldefinition))))
      (when (and (listp element) (> (length element) 0))
        ;; The element is a nested fractal expression
        (setf (slot-value mlexpression 'extype) :fractal)
        (let ((fractal-mlexpression (mrosetta-mlexpression :mldefinition element :rkeychain (slot-value mlexpression 'rkeychain))))
          (setf (slot-value mlexpression 'fractals) `(,@(slot-value mlexpression 'fractals) ,fractal-mlexpression))
          (mrosetta-parse fractal-mlexpression))
        (setq larg nil))
      (when (stringp element)
        ;; The element is a quoted string, so just pass it along
        (setq larg element)))))

(cl-defmethod mrosetta-compile ((mlexpression mrosetta-mlexpression))
  "Compile the MLEXPRESSION instance into a regular expression structure."
  (let* ((rkeychain (slot-value mlexpression 'rkeychain))
         (regex)
         (regex-key (mrosetta-keychain-generate-key rkeychain))
         (rinstance)
         (rinstance-key (mrosetta-keychain-generate-key rkeychain))
         (rmatch (slot-value mlexpression 'rmatch))
         (rmatch-key (mrosetta-keychain-generate-key rkeychain))
         (rprefix (slot-value mlexpression 'rprefix))
         (rsuffix (slot-value mlexpression 'rsuffix))
         (left-rboundary (slot-value mlexpression 'left-rboundary))
         (right-rboundary (slot-value mlexpression 'right-rboundary))
         (rbuffer (slot-value mlexpression 'rbuffer))
         (rbuffer-key (mrosetta-keychain-generate-key rkeychain))
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
            (setq rmatch (concat rmatch (mrosetta-compile fractal)))))
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

(cl-defmethod mrosetta-process ((mlexpression mrosetta-mlexpression) &rest args)
  "Process human-readable text of the :text or :inner string within ARGS and return the semantically-significant data structure as defined by the MLEXPRESSION instance."
  (when (mrosetta-mlexpression-should-ignore mlexpression)
    (error "Metalanguage semantic error: Root expressions cannot be ignorable"))
  (let ((htext (or (plist-get args :text)
                   (plist-get args :inner)))
        (exregex (mrosetta-mlexpression-regex mlexpression))
        (exdata '())
        (case-fold-search nil))
    (when (mrosetta-mlexpression-is-contextual mlexpression)
      ;; Ensure complete matches of contextual expressions
      (setq exregex (concat "^" exregex "$")))
    (save-match-data
      (and htext
           (string-match exregex htext)
           ;; Found match for the entirety of the expression
           (let ((extext (match-string (mrosetta-mlexpression-regex-key mlexpression) htext))
                 (pos))
             (save-match-data
               ;; Iterate over all instance occurrences within the expression-matching text
               (while (string-match (mrosetta-mlexpression-rinstance mlexpression) extext pos)
                 (setq pos (match-end 0))
                 ;; Process the exact match as defined by the expression
                 (let ((instance-exdata))
                   ;; Cases where the expression is a :fractal
                   (when (eq (mrosetta-mlexpression-extype mlexpression) :fractal)
                     ;; Recursively process all non-ignorable fractals within
                     (let ((fractals (mrosetta-mlexpression-fractals mlexpression)))
                       (dolist (fractal fractals)
                         (when (not (mrosetta-mlexpression-should-ignore mlexpression))
                           (let ((fractal-exdata (mrosetta-process fractal :inner (match-string (mrosetta-mlexpression-regex-key fractal) extext))))
                             (when fractal-exdata
                               (setq instance-exdata `(,@instance-exdata ,fractal-exdata))))))))
                   ;; Cases where the expression is a :match
                   (when (eq (mrosetta-mlexpression-extype mlexpression) :match)
                     ;; Just store the end-match, modified if defined as such
                     (let ((match (match-string (mrosetta-mlexpression-rmatch-key mlexpression) extext))
                           (modifier (mrosetta-mlexpression-modifier mlexpression)))
                       (when modifier
                         (setq match (funcall modifier match)))
                       (setq instance-exdata match)))
                   (when instance-exdata
                     (setq exdata `(,@exdata ,instance-exdata)))))
               (when exdata
                 ;; Splice instance data in case of a singular expression
                 (when (not (mrosetta-mlexpression-is-plural mlexpression))
                   (setq exdata (car exdata)))
                 ;; Return the structured data object
                 `(,(or (mrosetta-mlexpression-key mlexpression) :nokey) . ,exdata))))))))

(cl-defmethod mrosetta-update ((mlexpression mrosetta-mlexpression) &rest args)
  "Process human readable text of the :text or :inner string and return the semantically updated text based on the provided :sdata structure within ARGS, as defined by the MLEXPRESSION instance."
  (let ((htext (or (plist-get args :text)
                   (plist-get args :inner)))
        (exregex (mrosetta-mlexpression-regex mlexpression))
        (exkey (car (plist-get args :sdata)))
        (exdata-is-set (cdr (plist-get args :sdata)))
        (exdata (copy-tree (cdr (plist-get args :sdata))))
        (newtext)
        (case-fold-search nil))
    (when (and exdata
               (not (eq exkey
                        (or (mrosetta-mlexpression-key mlexpression) :nokey))))
      (error "Data structure error: Key mismatch"))
    (when (mrosetta-mlexpression-is-contextual mlexpression)
      ;; Ensure complete matches of contextual expressions
      (setq exregex (concat "^" exregex "$")))
    (save-match-data
      (and htext
           (string-match exregex htext)
           ;; Found metalanguage expression match
           (let ((extext (match-string (mrosetta-mlexpression-regex-key mlexpression) htext))
                 (pos '()))
             (save-match-data
               (while (or (and (string-match (mrosetta-mlexpression-rinstance mlexpression) extext (car pos))
                               ;; Handle plural expressions, including variations in length between updated and original sets
                               (or (not (mrosetta-mlexpression-is-plural mlexpression))
                                   ;; Expression is plural, but check if there is any updated data to insert
                                   (not exdata-is-set)
                                   ;; Updated list data is set, but only continue if any updated instances are left
                                   ;; Otherwise, just dispose of the remainder of the original
                                   exdata)
                               ;; An instance matched within the original text, update pos and enter iteration
                               (push (match-end 0) pos))
                          (and exdata
                               ;; No instances left within original text, but exdata still holding additional elements
                               (mrosetta-mlexpression-is-plural mlexpression)
                               ;; Reuse the last matched instance from the original text as a template
                               (string-match (mrosetta-mlexpression-rinstance mlexpression) extext (cadr pos))))
                 ;; Update each instance
                 (let ((instance-exdata (if (mrosetta-mlexpression-is-plural mlexpression) (pop exdata) exdata))
                       (instance-newtext))
                   (if (eq (mrosetta-mlexpression-extype mlexpression) :fractal)
                       ;; Recursively update all fractals within
                       (let ((fractals (mrosetta-mlexpression-fractals mlexpression)))
                         (dolist (fractal fractals)
                           (let* ((fractal-exdata (assq (mrosetta-mlexpression-key fractal) instance-exdata))
                                  (fractal-text (match-string (mrosetta-mlexpression-regex-key fractal) extext))
                                  (fractal-newtext (mrosetta-update fractal :inner fractal-text :sdata fractal-exdata)))
                             (setq instance-newtext (concat instance-newtext fractal-newtext)))))
                     ;; Update leaf elements
                     (when (eq (mrosetta-mlexpression-extype mlexpression) :match)
                       ;; Update match text, including ignorable matches
                       (let ((buffer (match-string (mrosetta-mlexpression-rbuffer-key mlexpression) extext))
                             (prefix (mrosetta-mlexpression-match-prefix mlexpression))
                             (suffix (mrosetta-mlexpression-match-suffix mlexpression))
                             (match (or instance-exdata
                                        (match-string (mrosetta-mlexpression-rmatch-key mlexpression) extext))))
                         (setq instance-newtext (concat buffer prefix match suffix))))
                     (when (eq (mrosetta-mlexpression-extype mlexpression) :literal)
                       ;; Just include the literal instance
                       (setq instance-newtext (match-string (mrosetta-mlexpression-rinstance-key mlexpression) extext))))
                   (setq newtext (concat newtext instance-newtext))))
               ;; Return the updated text
               newtext))))))

(provide 'metarosetta)

;;; metarosetta.el ends here
