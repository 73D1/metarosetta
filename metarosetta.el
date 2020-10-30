;;; metarosetta.el --- A semantically-driven interconnectivity framework -*- lexical-binding: t; -*-

;; Author: Bruno Tedeschi <me@btedeschi.com>

;;; Commentary:

;; This package adds Metarosetta expression language support to Org-mode.
;; 
;; It enables in-context definition of semantic protocols, within Org-mode, which automatically translate human-input text into a machine-digestible API-compatible structure particular to a given context.
;; 
;; For details and language specification, please refer to the original package documentation and source org file.

;;; Code:

(require 'eieio)
(require 'eieio-base)
(require 'url)
(require 'url-http)
(require 'web-server)
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
    :documentation "The regular expression of the encompassing expression's semantic match."
    :reader mrosetta-mlexpression-rmatch)
   (rmatch-key
    :initform 'nil
    :type (or null number)
    :documentation "The regex group key for the encompassing expression's output value match."
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
   (left-rbuffer-key
    :type number
    :documentation "The regex group key for the encompassing expression's left buffer match."
    :reader mrosetta-mlexpression-left-rbuffer-key)
   (right-rbuffer-key
    :type number
    :documentation "The regex group key for the encompassing expression's right buffer match."
    :reader mrosetta-mlexpression-right-rbuffer-key)
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
  (setf (slot-value mlexpression 'rbase) (concat ".+?"))
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
                  "\\(?:" rsubstring-quote "\\)?" rbase "\\(?:" rsubstring-quote "\\(?:" rbase "\\)?" "\\)+"
                  "\\|"
                  "\\(?:" "\\(?:" rbase "\\)?" rsubstring-quote "\\)+" rbase "\\(?:" rsubstring-quote "\\)?"
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
  "Parse the list epxression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
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
         (left-rbuffer-key (mrosetta-keychain-generate-key rkeychain))
         (right-rbuffer-key (mrosetta-keychain-generate-key rkeychain))
         (is-contextual (slot-value mlexpression 'is-contextual))
         (is-optional (slot-value mlexpression 'is-optional))
         (is-plural (slot-value mlexpression 'is-plural)))
    (if (eq (slot-value mlexpression 'extype) :fractal)
        ;; Recursively compile all nested fractal expression instances
        (let ((fractals (slot-value mlexpression 'fractals)))
          ;; Fractal Expressions cannot have end-matches
          (when rmatch
            (error "Metalanguage syntax error: End-matching expressions, like words or paragraphs, must be defined with parentheses"))
          (dolist (fractal fractals)
            (setq rmatch (concat rmatch (mrosetta-compile fractal)))))
      ;; Literal or end Match
      (when (eq rmatch nil)
        (setq rmatch (slot-value mlexpression 'rbase))))
    ;; Compile the total match, instance and expression-encompassing regular expressions
    (setq rmatch (concat "\\(?" (number-to-string rmatch-key) ":" rmatch "\\)"))
    (setq rinstance (concat "\\(?" (number-to-string rinstance-key) ":"
                            (when (not is-contextual)
                              (concat "\\(?" (number-to-string left-rbuffer-key) ":" rbuffer "\\)"
                                      (or rprefix left-rboundary)))
                            rmatch
                            (when (not is-contextual)
                              (concat (or rsuffix right-rboundary)
                                      "\\(?" (number-to-string right-rbuffer-key) ":" rbuffer "\\)"))
                            "\\)"))
    (setq regex (concat "\\(?" (number-to-string regex-key) ":"
                        rinstance
                        (when is-plural "+")
                        "\\)"
                        (when is-optional "?")))
    (setf (slot-value mlexpression 'rmatch-key) rmatch-key
          (slot-value mlexpression 'rmatch) rmatch
          (slot-value mlexpression 'left-rbuffer-key) left-rbuffer-key
          (slot-value mlexpression 'right-rbuffer-key) right-rbuffer-key
          (slot-value mlexpression 'rinstance-key) rinstance-key
          (slot-value mlexpression 'rinstance) rinstance
          (slot-value mlexpression 'regex-key) regex-key
          (slot-value mlexpression 'regex) regex)))

(cl-defmethod mrosetta-process ((mlexpression mrosetta-mlexpression) &rest args)
  "Process human-readable text within the :text or :inner string within ARGS and return the semantic data structure as defined by the MLEXPRESSION instance."
  (let ((htext (or (plist-get args :text)
                   (plist-get args :inner)))
        (is-inner (plist-get args :inner))
        (exdata '())
        (case-fold-search nil))
    (or (when (and (mrosetta-mlexpression-is-contextual mlexpression) is-inner)
          ;; Return the full inner-text matched within the parent expression if not marked as ignorable
          (when (and (eq (mrosetta-mlexpression-extype mlexpression) :match)
                     (not (mrosetta-mlexpression-should-ignore mlexpression)))
            `(,(or (mrosetta-mlexpression-key mlexpression) :nokey) . ,htext)))
        (save-match-data
          (and htext
               (string-match (mrosetta-mlexpression-regex mlexpression) htext)
               ;; Found match for the entirety of the expression
               (let ((extext (match-string (mrosetta-mlexpression-regex-key mlexpression) htext))
                     (pos))
                 (save-match-data
                   ;; Iterate over all instance occurrences within the matching expression text
                   (while (string-match (mrosetta-mlexpression-rinstance mlexpression) extext pos)
                     (setq pos (match-end 0))
                     ;; Process the exact match as defined by the expression
                     (let ((instance-exdata))
                       ;; Cases where the expression is a :fractal
                       (when (eq (mrosetta-mlexpression-extype mlexpression) :fractal)
                         ;; Recursively process all fractals within
                         (let ((fractals (mrosetta-mlexpression-fractals mlexpression)))
                           (dolist (fractal fractals)
                             (let ((fractal-exdata (mrosetta-process fractal :inner (match-string (mrosetta-mlexpression-regex-key fractal) extext))))
                               (when fractal-exdata
                                 (setq instance-exdata `(,@instance-exdata ,fractal-exdata)))))))
                       ;; Cases where the expression is a :match
                       (when (and (eq (mrosetta-mlexpression-extype mlexpression) :match)
                                  (not (mrosetta-mlexpression-should-ignore mlexpression)))
                         ;; Just store the semantic end-match, modified if defined as such
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
                     ;; Return the structured semantic data object
                     `(,(or (mrosetta-mlexpression-key mlexpression) :nokey) . ,exdata)))))))))

(cl-defmethod mrosetta-update ((mlexpression mrosetta-mlexpression) &rest args)
  "Process human readable text within the :text or :inner string and return the semantically updated text with the provided :sdata structure within ARGS, as defined by the MLEXPRESSION instance."
  (let ((htext (or (plist-get args :text)
                   (plist-get args :inner)))
        (exkey (car (plist-get args :sdata)))
        (exdata-set (when (cdr (plist-get args :sdata))
                      t))
        (exdata (copy-tree (cdr (plist-get args :sdata))))
        (is-inner (plist-get args :inner))
        ;; Inter-instance buffer persistence for list length mutability
        (is-mutating)
        (intra-buffers '())
        (newtext)
        (case-fold-search nil))
    (when (and exdata
               (not (eq exkey
                        (or (mrosetta-mlexpression-key mlexpression) :nokey))))
      (error "Data structure error: Key mismatch"))
    (or (when (and (mrosetta-mlexpression-is-contextual mlexpression) is-inner)
          ;; Return the full inner-text matched within the parent expression or the updated text passed in
          (or exdata htext))
        (save-match-data
          (and htext
               (string-match (mrosetta-mlexpression-regex mlexpression) htext)
               ;; Found metalanguage expression match
               (let ((extext (match-string (mrosetta-mlexpression-regex-key mlexpression) htext))
                     (pos '()))
                 (save-match-data
                   (while (or (and (string-match (mrosetta-mlexpression-rinstance mlexpression) extext (car pos))
                                   ;; Handle lists of mutable lengths
                                   (or (not (mrosetta-mlexpression-is-plural mlexpression))
                                       ;; Leave unset lists in updated semantic data
                                       (not exdata-set)
                                       ;; Updated list, possibly with less elements than original
                                       exdata)
                                   ;; An instance matched within the original text, update pos
                                   (push (match-end 0) pos))
                              (and exdata
                                   ;; No instances left within original text, but exdata still holding plural elements
                                   (mrosetta-mlexpression-is-plural mlexpression)
                                   ;; Reuse the last instance of original text as template
                                   (string-match (mrosetta-mlexpression-rinstance mlexpression) extext (cadr pos))
                                   ;; Mark as mutation
                                   (setq is-mutating t)))
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
                         ;; Update end-elements
                         (let ((left-buffer (match-string (mrosetta-mlexpression-left-rbuffer-key mlexpression) extext))
                               (right-buffer (match-string (mrosetta-mlexpression-right-rbuffer-key mlexpression) extext)))
                           (when (eq (mrosetta-mlexpression-extype mlexpression) :match)
                             ;; Manage potential list mutation
                             (when (mrosetta-mlexpression-is-plural mlexpression)
                               ;; Inject buffer in cases of list length mutations, and no right buffer at last element
                               (when (and is-mutating
                                          (string-empty-p left-buffer)
                                          (string-empty-p (car intra-buffers)))
                                 (setq left-buffer (car (last intra-buffers))))
                               ;; Persist inter-instance buffer
                               (push right-buffer intra-buffers))
                             ;; Update match text, including ignorable matches
                             (let ((prefix (mrosetta-mlexpression-match-prefix mlexpression))
                                   (suffix (mrosetta-mlexpression-match-suffix mlexpression))
                                   (match (or instance-exdata
                                              (match-string (mrosetta-mlexpression-rmatch-key mlexpression) extext))))
                               (setq instance-newtext (concat left-buffer prefix match suffix right-buffer))))
                           (when (eq (mrosetta-mlexpression-extype mlexpression) :literal)
                             ;; Include the literal, with surrounding buffer
                             (let ((literal (mrosetta-mlexpression-match-literal mlexpression)))
                               (setq instance-newtext (concat left-buffer literal right-buffer))))))
                       (setq newtext (concat newtext instance-newtext))))
                   ;; Return the updated text
                   newtext)))))))

(defun mrosetta-connector-payload-compile (payload)
  "Compile the provided PAYLOAD Lisp object before serialization."
  (or (and (listp payload)
           ;; Alist, Alist Item or Collection
           (or (and (listp (car payload))
                    (symbolp (caar payload))
                    ;; Alist
                    (mapcar (lambda (item)
                              (mrosetta-connector-payload-compile item))
                            payload))
               (and (symbolp (car payload))
                    ;; Alist Item
                    `(,(car payload) . ,(mrosetta-connector-payload-compile (cdr payload))))
               ;; Collection
               (vconcat (mapcar (lambda (item)
                                  (mrosetta-connector-payload-compile item))
                                payload))))
      ;; End Value
      payload))

(defun mrosetta-connector-payload-decompile (payload)
  "Decompile the provided PAYLOAD Lisp object after deserialization."
  (or (and (listp payload)
           ;; Alist or Alist Item
           (or (and (listp (car payload))
                    (symbolp (caar payload))
                    ;; Alist
                    (mapcar (lambda (item)
                              (mrosetta-connector-payload-decompile item))
                            payload))
               (and (symbolp (car payload))
                    ;; Alist Item
                    `(,(car payload) . ,(mrosetta-connector-payload-decompile (cdr payload))))
               ;; Unpredicted state
               (error "Payload decompilation error: Deserialized lists can only be property-value pairs, or alists")))
      (and (vectorp payload)
           ;; Collection
           (mapcar (lambda (item)
                     (mrosetta-connector-payload-decompile item))
                   payload))
      ;; End Value
      payload))

(defun mrosetta-connector-http-request (url &rest url-args)
  "Send the provided query paramaters set by :qparams within URL-ARGS in alist form, and :payload Lisp object as serialized JSON payload, if respectively provided, to the URL endpoint, specifying the :verb as :GET or :POST, as well as the connection :headers in alist form. Upon completion, call the :callback function with the :success status, :status-code and returned :payload object, if any, or :status-message in case of an error."
  (let ((full-url (concat url
                          ;; Apppend the query section if :qparams specified
                          (when (plist-get url-args :qparams)
                            (concat "?"
                                    ;; Map all the provided query parameters along with their corresponding values
                                    (mapconcat (lambda (parampair)
                                                 (let ((param (car parampair))
                                                       (value (cdr parampair)))
                                                   (concat param "=" value)))
                                               (plist-get url-args :qparams)
                                               "&")))))
        (url-request-method (substring (symbol-name (plist-get url-args :verb)) 1))
        (url-request-extra-headers (if (plist-get url-args :payload)
                                       ;; Payload exists, append appropriate header
                                       `(("Content-Type" . "application/json") ,@(plist-get url-args :headers))
                                     (plist-get url-args :headers)))
        (url-request-data (when (plist-get url-args :payload)
                            (json-serialize (mrosetta-connector-payload-compile (plist-get url-args :payload)))))
        (url-callback (plist-get url-args :callback)))
    ;; Retrieve asynchronously
    (url-retrieve full-url
                  (lambda (_status)
                    (let (success
                          status-code
                          status-message
                          payload)
                      ;; Start at the beginning of the buffer
                      (goto-char (point-min))
                      ;; Set the status code
                      (setq status-code (url-http-symbol-value-in-buffer 'url-http-response-status (current-buffer)))
                      ;; Skip the headers section, delimited by an empty line
                      (re-search-forward "^$")
                      ;; Parse the payload section
                      (setq payload (mrosetta-connector-payload-decompile (json-parse-buffer :object-type 'alist)))
                      ;; Parse the status code
                      (if (eq (/ status-code 100) 2)
                          ;; If 2xx signal success
                          (setq success t)
                        ;; Else, set the status message from the payload
                        (setq status-message (cdr (assq 'message payload))))
                      ;; Call back the callback function with the response data
                      (funcall url-callback
                               :success success
                               :status-code status-code
                               :status-message status-message
                               :payload payload))
                    ;; Kill the response buffer
                    (kill-buffer)))))

(defclass mrosetta-connector-http-listener ()
  ((hostname
    :initarg :hostname
    :type string
    :documentation "The host name of the server."
    :reader mrosetta-connector-http-listener-hostname)
   (port
    :initarg :port
    :type number
    :documentation "The port at which the web-server should accept requests."
    :reader mrosetta-connector-http-listener-port)
   (procotocl
    :initarg :protocol
    :initform "https"
    :type string
    :documentation "The protocol accepted for incoming requests. Note that this is only of informational nature, as TLS termination is done outside of Emacs."
    :reader mrosetta-connector-http-listener-protocol)
   (server
    :initform 'nil
    :type (or null ws-server)
    :documentation "A ws-server instance containing all instance-registered listeners.")
   (keychain
    :initform (mrosetta-keychain)
    :type mrosetta-keychain
    :documentation "A keychain to manage assigned endpoint ids.")
   (endpoints
    :initform '()
    :type list
    :documentation "A list of all instance-registered listeners.")
   (is-alive
    :initform 'nil
    :documentation "The state of the server in context, t if running, nil otherwise."
    :reader mrosetta-connector-http-listener-is-alive))
  "An object defining a specific web-server with corresponding handlers at specific paths.")

(cl-defmethod mrosetta-connector-http-listener-start ((listener mrosetta-connector-http-listener))
  "Start the LISTENER."
  (when (not (slot-value listener 'is-alive))
    (let ((port (slot-value listener 'port))
          (endpoints (slot-value listener 'endpoints)))
      ;; Start the server if one or more endpoints registered
      (when endpoints
        (setf (slot-value listener 'server)
              (ws-start (mapcar (lambda (endpointpair)
                                  (cdr endpointpair))
                                endpoints)
                        port))
        ;; Set and return t as is-alive status
        (setf (slot-value listener 'is-alive) t)))))

(cl-defmethod mrosetta-connector-http-listener-stop ((listener mrosetta-connector-http-listener))
  "Stop the LISTENER."
  (when (slot-value listener 'is-alive)
    ;; Stop the server
    (ws-stop (slot-value listener 'server))
    (setf (slot-value listener 'server) nil)
    ;; Set the is-alive status and return success of stopping
    (not (setf (slot-value listener 'is-alive) nil))))

(cl-defmethod mrosetta-connector-http-listener-endpoint-set ((listener mrosetta-connector-http-listener) path &rest endpoint-args)
  "Set an endpoint to the LISTENER on the provided PATH, for the provided :verb within the ENDPOINT-ARGS. If :id provided, update an existing entpoint. Upon a received request, call back the :callback function with the request query parameters set by :qparams in alist form, and the :payload Lisp object parsed from the incoming JSON payload. Headers are passed in alist form and set by :headers. The callback function should return a response-defining property list containing a :status-code, :headers if any, and :payload if needed or :status-message if an error occurred. Return the created endpoint's id."
  (let ((keychain (slot-value listener 'keychain))
        (endpoint-id (plist-get endpoint-args :id))
        (verb (plist-get endpoint-args :verb))
        (listener-callback (plist-get endpoint-args :callback)))
    (if endpoint-id
        ;; Clear the existing endpoint
        (setf (slot-value listener 'endpoints)
              (assq-delete-all endpoint-id (slot-value listener 'endpoints)))
      ;; Generate a new endpoint id
      (setq endpoint-id (mrosetta-keychain-generate-key keychain)))
    ;; Create and save the endpoint
    (push `(,endpoint-id . ((,verb . ,path) . ,(lambda (request)
                                                 (let ((rprocess (ws-process request))
                                                       (rheaders (mapcar (lambda (html-headerpair)
                                                                           (let ((headerkey (car html-headerpair))
                                                                                 (headervalue (cdr html-headerpair)))
                                                                             `(,(capitalize (substring (symbol-name headerkey) 1)) . ,headervalue)))
                                                                         (cl-remove-if-not (lambda (headerpair)
                                                                                             (symbolp (car headerpair)))
                                                                                           (ws-headers request))))
                                                       (rparams (cl-remove-if-not (lambda (headerpair)
                                                                                    (stringp (car headerpair)))
                                                                                  (ws-headers request)))
                                                       (rbody (mrosetta-connector-payload-decompile (json-parse-string (ws-body request)
                                                                                                                       :object-type 'alist)))
                                                       (response '()))
                                                   ;; Call back the callback function with set arguments
                                                   (setq response (funcall listener-callback
                                                                           :headers rheaders
                                                                           :qparams rparams
                                                                           :payload rbody))
                                                   ;; Compile and write response
                                                   (let ((res-status-code (plist-get response :status-code))
                                                         (res-status-message (plist-get response :status-message))
                                                         (res-headers (plist-get response :headers))
                                                         (res-payload (plist-get response :payload)))
                                                     ;; If status message exists, compile it into a payload
                                                     (when res-status-message
                                                       (setq res-payload `(message . ,res-status-message)))
                                                     ;; If payload exists, append appropriate header
                                                     (when res-payload
                                                       (setq res-headers `(("Content-Type" . "application/json") . ,@res-headers)))
                                                     ;; Set response status code and headers
                                                     (apply #'ws-response-header rprocess res-status-code res-headers)
                                                     ;; Send the response
                                                     (process-send-string rprocess
                                                                          (json-serialize (mrosetta-connector-payload-compile res-payload))))))))
          (slot-value listener 'endpoints))
    ;; Return the set endpoint id
    endpoint-id))

(defclass mrosetta-connector-webhook-biting ()
  ((url
    :initarg :url
    :type string
    :documentation "The endpoint url of the encompassing webhook instance."
    :reader mrosetta-connector-webhook-biting-url
    :writer mrosetta-connector-webhook-biting-url-set))
  "An outgoing, callable, webhook object to handle sending extracted semantic data of a given match.")

(cl-defmethod mrosetta-connector-webhook-bite ((webhook mrosetta-connector-webhook-biting) sdata bite-callback)
  "Call on the WEBHOOK with the provided SDATA of a given Metalanguage expression match. Upon completion call back the BITE-CALLBACK with a success status of either t or nil, and an optional :message if unsuccessful."
  (let ((bite-url (slot-value webhook 'url)))
    ;; Create the HTTP request and register callback
    (mrosetta-connector-http-request bite-url
                                     :verb :POST
                                     ;; sdata is by Mrosetta fractal convention an alist *item*, i.e. a cons cell
                                     :payload `(,sdata)
                                     :callback (lambda (&rest cbargs)
                                                 (let ((success (plist-get cbargs :success))
                                                       (msg (plist-get cbargs :status-message)))
                                                   (funcall bite-callback success :message msg))))))

(defclass mrosetta-connector-webhook-biting-am-alive (mrosetta-connector-webhook-biting)
  ()
  "A subclasss of the biting webhook specialized for notifying remote biters of this server's alive status.")

(cl-defmethod mrosetta-connector-webhook-bite ((webhook mrosetta-connector-webhook-biting-am-alive) status bite-callback)
  "Call on the WEBHOOK to notify of the alive STATUS of t or nil. Upon completion, call back the BITE-CALLBACK with a success status of either t or nil, and an optional :message if unsuccessful."
  (cl-call-next-method webhook
                       `(am-alive . ,(if status "yes" "no"))
                       bite-callback))

(defclass mrosetta-connector-webhook-catching ()
  ((path
    :initarg :path
    :type string
    :documentation "The relative path which to listen on for the encompassing webhook catches."
    :reader mrosetta-connector-webhook-catching-path
    :writer mrosetta-connector-webhook-catching-path-set)
   (id
    :initform 'nil
    :type (or null number)
    :documentation "The listener endpoint id assigned to the webhook in context."))
  "An incoming webhook object to handle receiving semantic data updates for a given match.")

(cl-defmethod mrosetta-connector-webhook-catching-url ((webhook mrosetta-connector-webhook-catching) listener)
  "Return the full endpoint URL of the WEBHOOK in context, within the provided LISTENER."
  (concat (mrosetta-connector-http-listener-protocol listener) "://"
          (mrosetta-connector-http-listener-hostname listener)
          "/" (slot-value webhook 'path)))

(cl-defmethod mrosetta-connector-webhook-catch ((webhook mrosetta-connector-webhook-catching) listener catch-callback)
  "Register a CATCH-CALLBACK for the WEBHOOK in context with the provided LISTENER. When hook caught, call back the callback function with a list of updated semantic data objects corresponding to tracked Metalanguage expression match instances. The callback should return :success of t if semantic data valid and update successful, or nil if not followed by an optional :message to the sender."
  (let ((catch-path (slot-value webhook 'path)))
    ;; Set the corresponding endpoint on the listener
    (setf (slot-value webhook 'id)
          (mrosetta-connector-http-listener-endpoint-set listener catch-path
                                                         :verb :POST
                                                         :id (slot-value webhook 'id)
                                                         :callback (lambda (&rest cbargs)
                                                                     (let* ((sdata (mapcar (lambda (payload-item)
                                                                                             ;; Convert alist to fractal cons cell
                                                                                             (car payload-item))
                                                                                           (plist-get cbargs :payload)))
                                                                            (res-args (funcall catch-callback sdata))
                                                                            (res-success (plist-get res-args :success))
                                                                            (res-message (plist-get res-args :message)))
                                                                       `(:status-code ,(if res-success 200 400)
                                                                         :status-message ,res-message)))))))

(defun mrosetta-context-org-heading-get ()
  "Get the full org heading at point as plain string, excluding the leading asterisks."
  (let ((curr-pos (line-beginning-position)))
    ;; Skip the leading asterisks and spaces
    (while (or (char-equal (char-after curr-pos) ?*)
               (char-equal (char-after curr-pos) ?\s))
      (setq curr-pos (1+ curr-pos)))
    ;; Get rest of heading content as string
    (buffer-substring-no-properties curr-pos (line-end-position))))

(defun mrosetta-context-org-heading-set (heading-text)
  "Set the full text of the org heading at point to HEADING-TEXT, while preserving the leading asterisks."
  ;; Start at the beginning of set line
  (goto-char (line-beginning-position))
  ;; Skip the leading asterisks and spaces
  (while (or (char-equal (following-char) ?*)
             (char-equal (following-char) ?\s))
    (forward-char))
  ;; Delete the rest of heading content
  (delete-region (point) (line-end-position))
  ;; Insert the new content at point
  (insert heading-text))

(defun mrosetta-context-org-heading-find (org-file property-name property-value heading-callback)
  "Find a specific org heading within ORG-FILE, matched by PROPERTY-VALUE of PROPERTY-NAME. If found, call back the HEADING-CALLBACK with point at the beginning of the heading line."
  (org-map-entries heading-callback
                   (concat "+" property-name "=" "\"" property-value "\"")
                   `(,org-file)))

(defclass mrosetta-context-org-collection ()
  ((keychain
    :initarg :keychain
    :initform (mrosetta-keychain)
    :type mrosetta-keychain
    :documentation "The keychain instance used to generate item keys within the scope of the encompassing collection instance.")
   (item-class
    :initarg :item-class
    :type symbol
    :documentation "The containing items' class symbol.")
   (items
    :initform '()
    :type list
    :documentation "An alist of items, mapped by id, contained within the encompassing collection instance."
    :reader mrosetta-context-org-collection-items))
  "A collection manager of tracked org entry items within a specific scope. Must be subclassed with items of specific type.")

(cl-defmethod mrosetta-context-org-collection-set ((collection mrosetta-context-org-collection) item)
  "Add or reset the ITEM within the managed org COLLECTION."
  (when (not (eq (slot-value collection 'item-class)
                 (eieio-object-class item)))
    (error "Collection class mismatch error: The provided item is incompatible with the encompassing collection"))
  (let ((item-id (or (mrosetta-context-org-entry-id item)
                     (mrosetta-keychain-generate-key (slot-value collection 'keychain)))))
    (mrosetta-context-org-entry-id-set item item-id)
    ;; Add the new item to collection, potentially replacing an existing one
    (setf (slot-value collection 'items) (assq-delete-all item-id (slot-value collection 'items)))
    (push `(,item-id . ,item) (slot-value collection 'items))
    item))

(cl-defmethod mrosetta-context-org-collection-get ((collection mrosetta-context-org-collection) item-id)
  "Get an item from the COLLECTION defined by the provided ITEM-ID. Return the item or nil if none present."
  (cdr (assq item-id (slot-value collection 'items))))

(defclass mrosetta-context-org-entry ()
  ((id
    :initarg :id
    :initform 'nil
    :type (or null number)
    :documentation "The entry identifier within the scope of its encompassing collection."
    :reader mrosetta-context-org-entry-id
    :writer mrosetta-context-org-entry-id-set)
   (org-file
    :initarg :org-file
    :type string
    :documentation "The org file within which the encompassing entry is set."
    :reader mrosetta-context-org-entry-file))
  "An org entry reference matched within the Metarosetta framework."
  :abstract t)

(defclass mrosetta-context-org-mlexpression (mrosetta-context-org-entry)
  ((mldefinition
    :initarg :mldefinition
    :type list
    :documentation "The metalanguage definition referring to the org entry in context."
    :reader mrosetta-context-org-mlexpression-mldefinition
    :writer mrosetta-context-org-mlexpression-mldefinition-set)
   (biting-hook
    :initform 'nil
    :type (or null mrosetta-connector-webhook-biting)
    :documentation "The outgoing webhook to bite when sending out extracted structured semantic data."
    :reader mrosetta-context-org-mlexpression-biting-hook
    :writer mrosetta-context-org-mlexpression-biting-hook-set)
   (biting-hook-compilation
    :initarg :biting-hook-compilation
    :initform 'nil
    :type (or null string)
    :documentation "The compiled value to persist which defines the biting webhook object in context.")
   (am-alive-hook
    :initform 'nil
    :type (or null mrosetta-connector-webhook-biting-am-alive)
    :documentation "A dedicated outgoing webhook to notify the biting end of the incoming hook of this server's alive status. No need to bite on hooks when the catcher is down."
    :reader mrosetta-context-org-mlexpression-am-alive-hook
    :writer mrosetta-context-org-mlexpression-am-alive-hook-set)
   (am-alive-hook-compilation
    :initarg :am-alive-hook-compilation
    :initform 'nil
    :type (or null string)
    :documentation "The compiled value to persist which defines the am alive webhook object in context.")
   (catching-hook
    :initform 'nil
    :type (or null mrosetta-connector-webhook-catching)
    :documentation "The incoming webhook to catch when receiving semantic data to update tracked matches with."
    :reader mrosetta-context-org-mlexpression-catching-hook
    :writer mrosetta-context-org-mlexpression-catching-hook-set)
   (catching-hook-compilation
    :initarg :catching-hook-compilation
    :initform 'nil
    :type (or null string)
    :documentation "The compiled value to persist which defines the catching webhook object in context.")
   (match-collection
    :initarg :match-collection
    :initform (mrosetta-context-org-collection :item-class 'mrosetta-context-org-match)
    :type mrosetta-context-org-collection
    :documentation "The managed collection of all current matches corresponding to the metalanguage expression in context."
    :reader mrosetta-context-org-mlexpression-match-collection)
   (match-compilation
    :initarg :match-compilation
    :initform '()
    :type (list-of mrosetta-context-org-match)
    :documentation "The compiled list corresponding to the collection of matches. Used only for (de)serialization."))
  "An org entry referencing a particular metalanguage definition.")

(defclass mrosetta-context-org-match (mrosetta-context-org-entry)
  ((sync-id
    :initarg :sync-id
    :initform '0
    :type number
    :documentation "A synchronization id specifying the exact version of the match. Each update, from any side, increments the sync id."
    :reader mrosetta-context-org-match-sync-id
    :writer mrosetta-context-org-match-sync-id-set))
  "An org entry referencing a specific match in context of a particular metalanguage definition.")

(cl-defmethod mrosetta-context-org-match-sync-update ((match mrosetta-context-org-match))
  "Update the sync id of the MATCH."
  (setf (slot-value match 'sync-id) (1+ (slot-value match 'sync-id))))

(defclass mrosetta-context-org-db (eieio-persistent)
  ((file :initarg :file)
   (mlexpression-collection
    :initarg :mlexpression-collection
    :initform (mrosetta-context-org-collection :item-class 'mrosetta-context-org-mlexpression)
    :type mrosetta-context-org-collection
    :documentation "A managed collection of all defined and tracked metalanguage expressions in scope of the Metarosetta package."
    :reader mrosetta-context-org-db-mlexpression-collection)
   (mlexpression-compilation
    :initarg :mlexpression-compilation
    :initform '()
    :type (list-of mrosetta-context-org-mlexpression)
    :documentation "The compiled list corresponding to the collection of mlexpressions. Used only for (de)serialization."))
  "The root index object for all metalanguage definitions and matches within the org context.")

(cl-defgeneric mrosetta-context-org-compile (context-org-object)
  "Compile the CONTEXT-ORG-OBJECT into a compatible format for serialization to disk.")

(cl-defmethod mrosetta-context-org-compile ((collection mrosetta-context-org-collection))
  "Recursively compile the items contained within the COLLECTION. Return the compiled list."
  (mapcar (lambda (item-pair)
            (let ((item (cdr item-pair)))
              (mrosetta-context-org-compile item)))
          (slot-value collection 'items)))

(cl-defmethod mrosetta-context-org-compile ((entry mrosetta-context-org-entry))
  "Compile the ENTRY for serialization to disk."
  ;; Nothing to compile, all slots are directly compatible
  entry)

(cl-defmethod mrosetta-context-org-compile ((mlexpression-entry mrosetta-context-org-mlexpression))
  "Compile the MLEXPRESSION-ENTRY for serialization to disk."
  ;; Compile registered webhooks
  (setf (slot-value mlexpression-entry 'biting-hook-compilation)
        (let ((hook (slot-value mlexpression-entry 'biting-hook)))
          (when hook
            (mrosetta-connector-webhook-biting-url hook))))
  (setf (slot-value mlexpression-entry 'am-alive-hook-compilation)
        (let ((hook (slot-value mlexpression-entry 'am-alive-hook)))
          (when hook
            (mrosetta-connector-webhook-biting-url hook))))
  (setf (slot-value mlexpression-entry 'catching-hook-compilation)
        (let ((hook (slot-value mlexpression-entry 'catching-hook)))
          (when hook
            (mrosetta-connector-webhook-catching-path hook))))
  ;; Compile the contained match collection
  (setf (slot-value mlexpression-entry 'match-compilation)
        (mrosetta-context-org-compile (slot-value mlexpression-entry 'match-collection)))
  (cl-call-next-method mlexpression-entry))

(cl-defmethod mrosetta-context-org-compile ((db mrosetta-context-org-db))
  "Compile the root DB entry object for serialization to disk."
  (setf (slot-value db 'mlexpression-compilation)
        (mrosetta-context-org-compile (slot-value db 'mlexpression-collection)))
  db)

(cl-defgeneric mrosetta-context-org-decompile (context-org-object &optional compilation)
  "Decompile the contents of the CONTEXT-ORG-OBJECT from the provided COMPILATION, or an inner slot if structured as such.")

(cl-defmethod mrosetta-context-org-decompile ((collection mrosetta-context-org-collection) &optional compilation)
  "Recursively decompile the COMPILATION of items into the COLLECTION."
  (setf (slot-value collection 'items)
        (mapcar (lambda (item)
                  `(,(mrosetta-context-org-entry-id item) . ,(mrosetta-context-org-decompile item)))
                compilation))
  collection)

(cl-defmethod mrosetta-context-org-decompile ((entry mrosetta-context-org-entry) &optional _compilation)
  "Decompile the contents of the ENTRY from a compilation slot within, not the provided COMPILATION argument."
  ;; Nothing to decompile, all slots are directly usable
  entry)

(cl-defmethod mrosetta-context-org-decompile ((mlexpression-entry mrosetta-context-org-mlexpression) &optional _compilation)
  "Decompile the contents of the MLEXPRESSION-ENTRY from a compilation slot within, not the provided COMPILATION argument."
  ;; Decompile registered webhooks, if any
  (setf (slot-value mlexpression-entry 'biting-hook)
        (let ((hook-compilation (slot-value mlexpression-entry 'biting-hook-compilation)))
          (when hook-compilation
            (mrosetta-connector-webhook-biting :url hook-compilation))))
  (setf (slot-value mlexpression-entry 'am-alive-hook)
        (let ((hook-compilation (slot-value mlexpression-entry 'am-alive-hook-compilation)))
          (when hook-compilation
            (mrosetta-connector-webhook-biting-am-alive :url hook-compilation))))
  (setf (slot-value mlexpression-entry 'catching-hook)
        (let ((hook-compilation (slot-value mlexpression-entry 'catching-hook-compilation)))
          (when hook-compilation
            (mrosetta-connector-webhook-catching :path hook-compilation))))
  ;; Decompile the contained match collection
  (mrosetta-context-org-decompile (slot-value mlexpression-entry 'match-collection)
                                  (slot-value mlexpression-entry 'match-compilation))
  ;; Clear the compiled elements after decompilation
  (setf (slot-value mlexpression-entry 'biting-hook-compilation) nil)
  (setf (slot-value mlexpression-entry 'am-alive-hook-compilation) nil)
  (setf (slot-value mlexpression-entry 'catching-hook-compilation) '())
  (setf (slot-value mlexpression-entry 'match-compilation) '())
  (cl-call-next-method mlexpression-entry))

(cl-defmethod mrosetta-context-org-decompile ((db mrosetta-context-org-db) &optional _compilation)
  "Decompile the contents of DB from a compilation slot within, not the provided COMPILATION argument."
  (mrosetta-context-org-decompile (slot-value db 'mlexpression-collection)
                                  (slot-value db 'mlexpression-compilation))
  ;; Clear the compiled list after decompilation
  (setf (slot-value db 'mlexpression-compilation) '())
  db)

(defclass mrosetta-context-org ()
  ((index
    :initform 'nil
    :type (or null mrosetta-context-org-db)
    :documentation "The active datastore of the current Metarosetta context, containing all defined metalagnuage definitions with their respectively tracked matches."
    :reader mrosetta-context-org-index)
   (mlexpressions
    :initform '()
    :type list
    :documentation "The current session's cache containing all compiled metalanguage expressions active in current context."
    :reader mrosetta-context-org-mlexpressions)
   (listener
    :initform 'nil
    :type (or null mrosetta-connector-http-listener)
    :documentation "The encompassing context's web-server instance listening on tracked metalanguage expressions' incoming semantic data webhooks."
    :reader mrosetta-context-org-listener))
  "The Metarosetta org context object. Handles all Metarosetta-related operations within the org context.")

(cl-defmethod mrosetta-context-org-load ((context mrosetta-context-org) index-file)
  "Load the index datastore from INDEX-FILE, or create a new one if none exists, and compile all indexed Metalanguage expressions within the CONTEXT."
  ;; Load the index datastore
  (let ((index (setf (slot-value context 'index) (or (when (file-exists-p index-file)
                                                       (eieio-persistent-read index-file 'mrosetta-context-org-db))
                                                     (mrosetta-context-org-db :file index-file)))))
    ;; Decompile index entries
    (mrosetta-context-org-decompile index)
    ;; Initialize the registered Metalanguage expression instances
    (setf (slot-value context 'mlexpressions)
          (mapcar (lambda (org-mlexpression-pair)
                    (let* ((org-mlexpression-id (car org-mlexpression-pair))
                           (org-mlexpression-entry (cdr org-mlexpression-pair))
                           (mlexpression (mrosetta-mlexpression :mldefinition (mrosetta-context-org-mlexpression-mldefinition org-mlexpression-entry))))
                      ;; Parse and compile the metalanguage expression
                      (mrosetta-parse mlexpression)
                      (mrosetta-compile mlexpression)
                      ;; Return the metalanguage expression pair
                      `(,org-mlexpression-id . ,mlexpression)))
                  (mrosetta-context-org-collection-items (mrosetta-context-org-db-mlexpression-collection index)))))
  ;; Just return the loaded context
  context)

(cl-defmethod mrosetta-context-org-save ((context mrosetta-context-org))
  "Save CONTEXT index to the file defined within."
  (let ((index (slot-value context 'index)))
    ;; Compile index entries into serializable form
    (mrosetta-context-org-compile index)
    ;; Save the index datastore
    (eieio-persistent-save index)))

(cl-defmethod mrosetta-context-org-start-listening ((context mrosetta-context-org) hostname port)
  "Start listening for incoming semantic data at HOSTNAME on PORT within CONTEXT. Note that the host name is for notational purposes only."
  (let ((mlexpression-index (mrosetta-context-org-db-mlexpression-collection (slot-value context 'index)))
        (am-alive-hooks '())
        (listener (setf (slot-value context 'listener)
                        (mrosetta-connector-http-listener :hostname hostname :port port))))
    ;; Set all endpoints within the index
    (dolist (mlexpression-index-entry-pair (mrosetta-context-org-collection-items mlexpression-index))
      (let* ((mlexpression-id (car mlexpression-index-entry-pair))
             (mlexpression-index-entry (cdr mlexpression-index-entry-pair))
             (am-alive-hook (mrosetta-context-org-mlexpression-am-alive-hook mlexpression-index-entry))
             (catching-hook (mrosetta-context-org-mlexpression-catching-hook mlexpression-index-entry)))
        ;; Register catcher
        (mrosetta-connector-webhook-catch catching-hook
                                          listener
                                          (lambda (sdata)
                                            ;; Update headings within the context of the Metalanguage expression
                                            (mrosetta-context-org-update-headings context mlexpression-id sdata)
                                            ;; Confirm successful reception of data
                                            `(:success t)))
        ;; Queue am-alive hooks
        (push `(,mlexpression-id . ,am-alive-hook) am-alive-hooks)))
    ;; Start listener
    (mrosetta-connector-http-listener-start listener)
    ;; Broadcast alive status
    (mapc (lambda (hookpair)
            (let ((mlexpression-id (car hookpair))
                  (am-alive-hook (cdr hookpair)))
              (when am-alive-hook
                (mrosetta-connector-webhook-bite am-alive-hook t
                                                 (lambda (did-succeed &rest cbargs)
                                                   (if did-succeed
                                                       (message "Mrosetta broadcasted availability for match updates of Metalanguage expression with id %s!"
                                                                (number-to-string mlexpression-id))
                                                     (let ((msg (plist-get cbargs :message)))
                                                       (message "Mrosetta broadcast of availability error for Metalanguage expression with id %s: %s"
                                                                (number-to-string mlexpression-id) msg))))))))
          am-alive-hooks)
    ;; Notify the user
    (message "Metarosetta context listener started successfully!")))

(cl-defmethod mrosetta-context-org-stop-listening ((context mrosetta-context-org))
  "Stop listening for incoming semantic data within CONTEXT."
  (let ((mlexpression-index (mrosetta-context-org-db-mlexpression-collection (slot-value context 'index)))
        (listener (slot-value context 'listener)))
    ;; Broadcast alive status
    (dolist (mlexpression-index-entry-pair (mrosetta-context-org-collection-items mlexpression-index))
      (let* ((mlexpressoin-id (car mlexpression-index-entry-pair))
             (mlexpression-index-entry (cdr mlexpression-index-entry-pair))
             (am-alive-hook (mrosetta-context-org-mlexpression-am-alive-hook mlexpression-index-entry)))
        (when am-alive-hook
          (mrosetta-connector-webhook-bite am-alive-hook nil
                                           (lambda (did-succeed &rest cbargs)
                                             (if did-succeed
                                                 (message "Mrosetta broadcasted availability termination for match updates of Metalanguage epxression with id %s!"
                                                          (number-to-string mlexpressoin-id))
                                               (let ((msg (plist-get cbargs :message)))
                                                 (message "Mrosetta broadcast of availability termination error for Metalanguage expression with id %s: %s"
                                                          (number-to-string mlexpressoin-id) msg))))))))
    ;; Stop listener
    (mrosetta-connector-http-listener-stop listener)
    ;; Notify the user
    (message "Metarosetta context listener stopped successfully!")))

(cl-defmethod mrosetta-context-org-process-heading ((context mrosetta-context-org))
  "Process the heading at point by the provided Metarosetta CONTEXT."
  (let ((heading-text (mrosetta-context-org-heading-get)))
    (and heading-text
         (or (and (string-match "#mrosetta[[:blank:]]+\\(.+\\)" heading-text)
                  ;; Process the Metarosetta Metalanguage expression definition
                  (let* ((input-text (match-string 1 heading-text))
                         (mldefinition (car (read-from-string (concat "(" input-text ")"))))
                         (mlexpression-id (let ((id-property (org-entry-get (point) "mrosetta-mlexpression-id")))
                                            (when (and id-property
                                                       (not (string-empty-p id-property)))
                                              (string-to-number id-property))))
                         (biting-hook-url (let ((hook-url (org-entry-get (point) "mrosetta-mlexpression-bite-url")))
                                            (when (and hook-url
                                                       (not (string-empty-p hook-url)))
                                              hook-url)))
                         (am-alive-hook-url (let ((hook-url (org-entry-get (point) "mrosetta-mlexpression-am-alive-url")))
                                              (when (and hook-url
                                                         (not (string-empty-p hook-url)))
                                                hook-url)))
                         (catching-hook-url (let ((hook-url (org-entry-get (point) "mrosetta-mlexpression-catch-url")))
                                              (when (and hook-url
                                                         (not (string-empty-p hook-url)))
                                                hook-url)))
                         (mlexpression-index (mrosetta-context-org-db-mlexpression-collection (mrosetta-context-org-index context)))
                         (mlexpression-index-entry (or (let ((entry (mrosetta-context-org-collection-get mlexpression-index mlexpression-id)))
                                                         (when entry
                                                           (mrosetta-context-org-mlexpression-mldefinition-set entry mldefinition)
                                                           entry))
                                                       (mrosetta-context-org-mlexpression :org-file (buffer-file-name)
                                                                                          :mldefinition mldefinition)))
                         (mlexpression (mrosetta-mlexpression :mldefinition mldefinition))
                         (listener (mrosetta-context-org-listener context)))
                    (prog1 t ;; Regardless of the processing result, the match itself is valid and should return as such
                      ;; Index the entry
                      (setq mlexpression-index-entry (mrosetta-context-org-collection-set mlexpression-index mlexpression-index-entry))
                      (setq mlexpression-id (mrosetta-context-org-entry-id mlexpression-index-entry))
                      ;; Set weebhooks within the index entry, if any
                      ;; Set remote biting hook URL
                      (when biting-hook-url
                        (let ((hook (mrosetta-context-org-mlexpression-biting-hook mlexpression-index-entry)))
                          (if hook
                              (mrosetta-connector-webhook-biting-url-set hook biting-hook-url)
                            (mrosetta-context-org-mlexpression-biting-hook-set mlexpression-index-entry
                                                                               (mrosetta-connector-webhook-biting :url biting-hook-url)))))
                      ;; Set remote am-alive hook URL
                      (when am-alive-hook-url
                        (let ((hook (mrosetta-context-org-mlexpression-am-alive-hook mlexpression-index-entry)))
                          (if hook
                              (mrosetta-connector-webhook-biting-url-set hook biting-hook-url)
                            (mrosetta-context-org-mlexpression-am-alive-hook-set mlexpression-index-entry
                                                                                 (mrosetta-connector-webhook-biting-am-alive :url am-alive-hook-url)))))
                      ;; Parse the local catching hook path or generate a new one, and setup the webhook accordingly
                      (when listener
                        (let ((catching-hook-path (or (and catching-hook-url
                                                           (save-match-data
                                                             (and (string-match "http\\(?:s\\)?://.+/\\(.+\\)" catching-hook-url)
                                                                  (match-string 1 catching-hook-url))))
                                                      (mapconcat (lambda (char)
                                                                   (char-to-string (+ char
                                                                                      (random (1+ (- ?z char))))))
                                                                 (make-list 17 ?a)
                                                                 "")))
                              (hook (mrosetta-context-org-mlexpression-catching-hook mlexpression-index-entry)))
                          (if hook
                              (mrosetta-connector-webhook-catching-path-set hook catching-hook-path)
                            (setq hook (mrosetta-connector-webhook-catching :path catching-hook-path))
                            (mrosetta-context-org-mlexpression-catching-hook-set mlexpression-index-entry hook))
                          ;; Stop the listener
                          (mrosetta-connector-http-listener-stop listener)
                          ;; Register the webhook
                          (mrosetta-connector-webhook-catch hook
                                                            listener
                                                            (lambda (sdata)
                                                              ;; Update headings within the context of the Metalanguage expression
                                                              (mrosetta-context-org-update-headings context mlexpression-id sdata)
                                                              ;; Confirm successful reception of data
                                                              ;; (Parse info not included within the response for simplicity's sake)
                                                              `(:success t)))
                          ;; Start the listener
                          (mrosetta-connector-http-listener-start listener)
                          ;; Update the webhook url
                          (setq catching-hook-url (mrosetta-connector-webhook-catching-url hook listener))))
                      ;; Parse and compile the Metalanguage expression
                      (mrosetta-parse mlexpression)
                      (mrosetta-compile mlexpression)
                      ;; Cache the Metalanguage expression, replacing the previous entry if needed
                      (setf (slot-value context 'mlexpressions) (assq-delete-all mlexpression-id (slot-value context 'mlexpressions)))
                      (push `(,mlexpression-id . ,mlexpression) (slot-value context 'mlexpressions))
                      ;; Populate the org entry istelf with Metarosetta properties
                      ;; Expression ID
                      (org-entry-put (point) "mrosetta-mlexpression-id" (number-to-string mlexpression-id))
                      ;; Webhooks
                      (org-entry-put (point) "mrosetta-mlexpression-bite-url" biting-hook-url)
                      (org-entry-put (point) "mrosetta-mlexpression-am-alive-url" am-alive-hook-url)
                      (org-entry-put (point) "mrosetta-mlexpression-catch-url" catching-hook-url)
                      ;; Notify the user
                      (message "Mrosetta Metalanguage expression processed successfully!"))))
             (let* (did-match
                    (mlexpression-cache (mrosetta-context-org-mlexpressions context))
                    (mlexpression-ids (mapcar (lambda (mlexpression-pair) (car mlexpression-pair))
                                              mlexpression-cache)))
               ;; Check if provided heading matches any of the active Metalanguage expressions
               (while (let ((mlexpression-id (pop mlexpression-ids)))
                        (and mlexpression-id
                             (let* ((mlexpression (cdr (assq mlexpression-id mlexpression-cache)))
                                    (sdata (mrosetta-process mlexpression :text heading-text)))
                               ;; If matched, bite webhook in context and stop iterating
                               ;; If not, continue to the next possible Metalanguage expression match
                               (not (and sdata
                                         (let* ((mlexpression-index-entry (mrosetta-context-org-collection-get (mrosetta-context-org-db-mlexpression-collection (mrosetta-context-org-index context))
                                                                                                               mlexpression-id))
                                                (biting-hook (mrosetta-context-org-mlexpression-biting-hook mlexpression-index-entry))
                                                (mlexpression-match-index (mrosetta-context-org-mlexpression-match-collection mlexpression-index-entry))
                                                (match-id (let ((id-property (org-entry-get (point) "mrosetta-match-id")))
                                                            (when (and id-property
                                                                       (not (string-empty-p id-property)))
                                                              (string-to-number id-property))))
                                                (match-index-entry (or (mrosetta-context-org-collection-get mlexpression-match-index match-id)
                                                                       (mrosetta-context-org-match :org-file (buffer-file-name))))
                                                (match-sync-id (mrosetta-context-org-match-sync-update match-index-entry)))
                                           (prog1 t ;; Regardless if the bite actually succeeded, the match itself is valid and should return as such
                                             ;; Add index entry, if new
                                             (setq match-index-entry (mrosetta-context-org-collection-set mlexpression-match-index match-index-entry))
                                             (setq match-id (mrosetta-context-org-entry-id match-index-entry))
                                             ;; Add sync metadata
                                             (setq sdata `(,(car sdata) . ((org-id . ,match-id)
                                                                           (org-sync-id . ,match-sync-id)
                                                                           ,@(cdr sdata))))
                                             ;; Bite the webhook and send the processed semantic data
                                             (mrosetta-connector-webhook-bite biting-hook
                                                                              sdata
                                                                              (lambda (did-succeed &rest cbargs)
                                                                                (if did-succeed
                                                                                    (message "Mrosetta Metalanguage match synced successfully!")
                                                                                  (let ((msg (plist-get cbargs :message)))
                                                                                    (message "Mrosetta webhook bite error: %s" msg)))))
                                             ;; Update the org entry itself
                                             (org-entry-put (point) "mrosetta-match-id" (number-to-string match-id))
                                             (org-entry-put (point) "mrosetta-match-sync-id" (number-to-string match-sync-id))))
                                         (setq did-match t)))))))
               ;; Since while always returns nil, surface out the processing result
               did-match)))))

(cl-defmethod mrosetta-context-org-update-headings ((context mrosetta-context-org) mlexpression-id sdata)
  "Update all tracked org heading entries of a given MLEXPRESSION-ID within CONTEXT with semantic data contained within SDATA. SDATA is a list of one or more match structures."
  (let* ((mlexpression-index (mrosetta-context-org-db-mlexpression-collection (mrosetta-context-org-index context)))
         (mlexpression-index-entry (mrosetta-context-org-collection-get mlexpression-index mlexpression-id))
         (mlexpression-match-index (mrosetta-context-org-mlexpression-match-collection mlexpression-index-entry))
         (mlexpression-cache (mrosetta-context-org-mlexpressions context))
         (mlexpression (cdr (assq mlexpression-id mlexpression-cache))))
    ;; Iterate over all instances within sdata and update corresponding entries
    (dolist (instance-sdata sdata)
      (let* ((exdata (cdr instance-sdata))
             (match-id (cdr (assq 'org-id exdata)))
             (match-sync-id (cdr (assq 'org-sync-id exdata)))
             (match-index-entry (mrosetta-context-org-collection-get mlexpression-match-index match-id))
             (match-org-file (mrosetta-context-org-entry-file match-index-entry)))
        ;; Find and update the corresponding org heading
        (mrosetta-context-org-heading-find match-org-file
                                           "mrosetta-match-id"
                                           (number-to-string match-id)
                                           (lambda ()
                                             (let* ((original-text (mrosetta-context-org-heading-get))
                                                    (updated-text (mrosetta-update mlexpression
                                                                                   :text original-text
                                                                                   :sdata instance-sdata)))
                                               ;; Update the heading text
                                               (mrosetta-context-org-heading-set updated-text)
                                               ;; Update the heading properties
                                               (org-entry-put (point) "mrosetta-match-sync-id" (number-to-string match-sync-id))
                                               ;; Update the match index entry
                                               (mrosetta-context-org-match-sync-id-set match-index-entry match-sync-id)
                                               ;; Notify the user
                                               (message "Mrosetta Metalanguage match with id %s updated in file %s!"
                                                        (number-to-string match-id)
                                                        match-org-file))))))))

(provide 'metarosetta)

;;; metarosetta.el ends here
