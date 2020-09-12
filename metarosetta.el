;;; metarosetta.el --- A semantically-driven interconnectivity framework -*- lexical-binding: t -*-

;; Author: Bruno Tedeschi <me@btedeschi.com>

;;; Commentary:

;; This package adds Metarosetta expression language support to Org-mode.
;; 
;; It enables in-context definition of semantic protocols, within Org-mode, which automatically translate human-input text into a machine-digestible API-compatible structure particular to a given context.
;; 
;; For details and language specification, please refer to the original package documentation and source org file.

;;; Code:

(require 'eieio)

(defclass mrosetta-keychain ()
  ((lastkey
    :initform '0
    :type number
    :documentation "The last key generated and assigned to a group within the context of a single keychain instance."
    :reader mrosetta-lastkey))
  "A regex group key generator.")

(cl-defmethod mrosetta-generate-regex-key ((keychain mrosetta-keychain))
  "Generate a new key within a provided KEYCHAIN."
  (let ((key (+ 1 (slot-value keychain 'lastkey))))
    (setf (slot-value keychain 'lastkey) key)))

(defclass mrosetta-mlexpression ()
  (
   (mldefinition
    :initarg :mldefinition
    :initform (error "Cannot create an expression without a definition!")
    :type list
    :documentation "The metalanguage-specified definition of the expression in context."
    :reader mrosetta-mldefinition)
   (extype
    :type symbol
    :documentation "A symbol specifying the type of the encompassing expression instance. Can be either a :literal, :match or :fractal."
    :reader mrosetta-extype)
   (fractals
    :initform '()
    :type list
    :documentation "A list of mrosetta-expression instances contained within the encompassing expression instance."
    :reader mrosetta-fractals)
   (rkeychain
    :initarg :rkeychain
    :initform (mrosetta-keychain)
    :type mrosetta-keychain
    :documentation "The regex keychain instance managing keys for the encompassing expression tree."
    :reader mrosetta-rkeychain)
   (regex
    :type string
    :documentation "The compiled regular expression of the expression in context."
    :reader mrosetta-regex)
   (regex-key
    :type number
    :documentation "The regex matching group key for the encompassing expression instance."
    :reader mrosetta-regex-key)
   (rinstance
    :type string
    :documentation "The compiled regular expression matching a single instance of a possibly plural-matching expression."
    :reader mrosetta-rinstance)
   (rinstance-key
    :type number
    :documentation "The regex group key for matching a single instance of a possibly plural-matching metalanguage expression in context."
    :reader mrosetta-rinstance-key)
   (rbase
    :type string
    :documentation "The regular expression used as a foundational base in compilation of the match-extracting regular expression."
    :reader mrosetta-rbase)
   (rmatch
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression of the encompassing expression's semantic match."
    :reader mrosetta-rmatch)
   (rmatch-key
    :initform 'nil
    :type (or null number)
    :documentation "The regex group key for the encompassing expression's output value match."
    :reader mrosetta-rmatch-key)
   (rprefix
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression matching a specified prefix of the encompassing expression instance. Either a regex string or nil."
    :reader mrosetta-rprefix)
   (rprefix-key
    :initform 'nil
    :type (or null number)
    :documentation "The regex group key for the encompassing expression's prefix match. Either a group number or nil."
    :reader mrosetta-rprefix-key)
   (rsuffix
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression matching a specified suffix of the encompassing expression instance. Either a regex string or nil."
    :reader mrosetta-rsuffix)
   (rsuffix-key
    :initform 'nil
    :type (or null number)
    :documentation "The regex group key for the encompassing expression's suffix match. Either a group number or nil."
    :reader mrosetta-rsuffix-key)
   (key
    :initform 'nil
    :type (or null string)
    :documentation "The property key to which the expression output value is assigned, if any. Either a string or nil."
    :reader mrosetta-key)
   (is-uppercase
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches only uppercase words. Either non-nil or nil."
    :reader mrosetta-is-uppercase)
   (is-capitalized
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches only capitalized words. Either non-nil or nil."
    :reader mrosetta-is-capitalized)
   (match-prefix
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the prefix all possible expression matches should have, if any. Either a string or nil."
    :reader mrosetta-match-prefix)
   (match-suffix
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the suffix all possible expression matches should have, if any. Either a string or nil."
    :reader mrosetta-match-suffix)
   (match-substring
    :initform 'nil
    :type (or null string)
    :documentation "Specifies a specific substring all possible expression matches should contain, if any. Either a string or nil."
    :reader mrosetta-match-substring)
   (match-literal
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the literal string that the expression maches exclusively. Either a string or nill."
    :reader mrosetta-match-literal)
   (modifier
    :initform 'nil
    :type (or null symbol)
    :documentation "Specifies a symbol referencing a stored modifier function, if any. Either a symbol or nil."
    :reader mrosetta-modifier)
   (is-optional
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is optional to match within input text. Either non-nil or nil."
    :reader mrosetta-is-optional)
   (is-contextual
    :initform 'nil
    :documentation "Specifies whether the encompassing expression should be considered as contextual only. Either non-nil or nil."
    :reader mrosetta-is-contextual)
   (is-plural
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches plural values or just a single one. Either nil or non-nil."
    :reader mrosetta-is-plural)
  )
  "The Metarosetta Expression object used to define a contextual translational expression for semantic processing.")

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
  (setf (slot-value mlexpression 'rbase) "[[:word:]]+")
  args)

(push '(word . mrosetta-parse-word) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-word-uppercase ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse an uppercase word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'extype) :match)
  (setf (slot-value mlexpression 'rbase) "[A-Z0-9]+")
  (setf (slot-value mlexpression 'is-uppercase) t)
  args)

(push '(WORD . mrosetta-parse-word-uppercase) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-word-capitalized ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a capitalized word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'extype) :match)
  (setf (slot-value mlexpression 'rbase) "[A-Z0-9][a-z0-9]+")
  (setf (slot-value mlexpression 'is-capitalized) t)
  args)

(push '(Word . mrosetta-parse-word-capitalized) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-word-plurality ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a plural words expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-plural) t)
  (mrosetta-parse-word mlexpression args))

(push '(words . mrosetta-parse-word-plurality) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-paragraph ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a paragraph epxression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'extype) :match)
  (setf (slot-value mlexpression 'rbase) ".+")
  args)

(push '(paragraph . mrosetta-parse-paragraph) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-paragraph-plurality ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a plural paragraph expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-plural) t)
  (mrosetta-parse-paragraph mlexpression args))

(push '(paragraphs . mrosetta-parse-paragraph-plurality) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-substring ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse quoted text from :right arg within ARGS as matching element substring into the MLEXPRESSION instance in context."
  (let* ((substring-quote (plist-get args :right))
         (rsubstring-quote (regexp-quote substring-quote))
         (rbase (slot-value mlexpression 'rbase)))
    (when (eq substring-quote nil)
      (error "Metalanguage syntax error: Substring match expression without quoted content"))
    (setf (slot-value mlexpression 'rmatch)
          (concat "\(?:"
                  "\(?:" rsubstring-quote "\)?" rbase "\(?:" rsubstring-quote "\(?" rbase "\)?" "\)+"
                  "\|"
                  "\(?:" "\(?:" rbase "\)?" rsubstring-quote "\)+" rbase "\(?:" rsubstring-quote "\)?"
                  "\)"))
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

(cl-defmethod mrosetta-parse-contextuality ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse expression contextuality into the MLEXPRESSION instance in context. This function utilizes no ARGS."
  (setf (slot-value mlexpression 'is-contextual) t)
  args)

(push '(contextual . mrosetta-parse-contextuality) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-list ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the list epxression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-plural) t)
  args)

(push '(list . mrosetta-parse-list) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse-of ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the sub-expression from :right arg within ARGS into the MLEXPRESSION instance in context."
  (let ((sub-expression (plist-get args :right)))
    (when (or (eq sub-expression nil) (nlistp sub-expression))
      (error "Metalanguage syntax error: Sub-expression assignment without contextual expression"))
    (mrosetta-parse mlexpression sub-expression))
  (plist-put args :right nil))

(push '(of . mrosetta-parse-of) mrosetta-mlsyntax)

(cl-defmethod mrosetta-parse ((mlexpression mrosetta-mlexpression) &optional sub-definition)
  "Parse the metalanguage-specified definition within the MLEXPRESSION instance. Optionally, parse the explicitly-set SUB-DEFINITION instead."
  (let ((mldefinition (if (eq sub-definition nil)
                          (copy-tree (slot-value mlexpression 'mldefinition))
                        (copy-tree sub-definition)))
        (larg nil)
        (element nil)
        (rarg nil))
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
         (regex-key (mrosetta-generate-regex-key rkeychain))
         (rinstance)
         (rinstance-key (mrosetta-generate-regex-key rkeychain))
         (rmatch (slot-value mlexpression 'rmatch))
         (rmatch-key (mrosetta-generate-regex-key rkeychain))
         (rprefix (slot-value mlexpression 'rprefix))
         (rsuffix (slot-value mlexpression 'rsuffix))
         (is-optional (slot-value mlexpression 'is-optional))
         (is-plural (slot-value mlexpression 'is-plural)))
    (if (eq (slot-value mlexpression 'extype) :fractal)
        ;; Fractal Expressions cannot have end-matches
        (when rmatch
          (error "Metalanguage syntax error: End-matching expressions, like words or paragraphs, must be defined with parentheses"))
        ;; Recursively compile all nested fractal expression instances
        (let ((fractals (slot-value mlexpression 'fractals)))
          (dolist (fractal fractals)
            (setq rmatch (concat rmatch "[[:blank:]]+" (mrosetta-compile fractal)))))
      ;; Literal or end Match
      (when (eq rmatch nil)
        (setq rmatch (slot-value mlexpression 'rbase))))
    ;; Compile the total match, instance and expression-encompassing regular expressions
    (setq rmatch (concat "\(?" rmatch-key ":" rmatch "\)"))
    (setq rinstance (concat "\(?" rinstance-key ":" rprefix rmatch rsuffix "\)"))
    (setq regex (concat "\(?" regex-key ":"
                        rinstance
                        (when is-plural "+")
                        "\)"
                        (when is-optional "?")))
    (setf (slot-value mlexpression 'rmatch-key) rmatch-key
          (slot-value mlexpression 'rmatch) rmatch
          (slot-value mlexpression 'rinstance-key) rinstance-key
          (slot-value mlexpression 'rinstance) rinstance
          (slot-value mlexpression 'regex-key) regex-key
          (slot-value mlexpression 'regex) regex)))

(cl-defmethod mrosetta-process ((mlexpression mrosetta-mlexpression) htext)
  "Process human-readable text within the HTEXT string and return the semantic data structure as defined by the MLEXPRESSION instance."
  (let ((sdata '()))
    (save-match-data
      (and (string-match (mrosetta-regex mlexpression) htext)
           ;; Found match for the entirety of the expression
           (let ((mtext (match-string (mrosetta-regex-key mlexpression) htext))
                 (pos))
             (save-match-data
               ;; Iterate over all instance occurrences within the matching expression text
               (while (string-match (mrosetta-rinstance mlexpression) mtext pos)
                 (setq pos (match-end 0))
                 ;; Process the exact match as defined by the expression
                 (let ((instance-sdata)
                       (match (match-string (mrosetta-rmatch-key mlexpression) mtext)))
                   ;; Cases where the expression is a :fractal
                   (when (eq (mrosetta-extype mlexpression) :fractal)
                     ;; Recursively process all fractals within
                     (let ((fractals (mrosetta-fractals mlexpression)))
                       (dolist (fractal fractals)
                         (let ((fractal-sdata (mrosetta-process fractal (match-string (mrosetta-regex-key fractal) mtext))))
                           (when fractal-sdata
                             (setq instance-sdata `(,@instance-sdata ,fractal-sdata)))))))
                   ;; Cases where the expression is a :match
                   (when (and (eq (mrosetta-extype mlexpression) :match)
                              (not (mrosetta-is-contextual mlexpression)))
                     ;; Just store the semantic end-match
                     (setq instance-sdata match))
                   (setq sdata `(,@sdata ,instance-sdata))))))))
    (when (> (length sdata) 0)
      ;; Splice instance data in case of a singular expression
      (when (not (mrosetta-is-plural mlexpression))
        (setq sdata (car sdata)))
      ;; Return the structured semantic data object
      `(,(or (mrosetta-key mlexpression) :nokey) . ,sdata))))

(provide 'metarosetta)

;;; metarosetta.el ends here
