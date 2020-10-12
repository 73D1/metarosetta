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
(require 'eieio-base)
(require 'org)
(require 'request)

(defclass mrosetta-keychain ()
  ((lastkey
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
  (setf (slot-value mlexpression 'is-plural) t)
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
        (exdata (copy-tree (cdr (plist-get args :sdata))))
        (is-inner (plist-get args :inner))
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
                     (pos))
                 (save-match-data
                   (while (string-match (mrosetta-mlexpression-rinstance mlexpression) extext pos)
                     (setq pos (match-end 0))
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

(defclass mrosetta-connector ()
  ()
  "A base class defining the general interface used to send/receive structured data defined by Metarosetta expressions."
  :abstract t)

(cl-defmethod mrosetta-connector-parameters ((connector-class (subclass mrosetta-connector)))
  "Get the connector-specific expression parameters required for the CONNECTOR-CLASS. Return a list of parameter keywords."
  (error "Connector implementation error: Method mrosetta-connector-parameters not implemented in `%s'" (symbol-name (eieio-class-name connector-class))))

(cl-defmethod mrosetta-connector-send ((connector mrosetta-connector) _key _sdata _callback _cparameters)
  "Using the specified CONNECTOR, send the SDATA containing a list of processed text instances with specified connector-specific CPARAMETERS. The KEY defines the property symbol considered as an instance key, wherein the same-id instances are overwritten. Call back the CALLBACK function when done."
  (error "Connector implementation error: Method mrosetta-connector-send not implemented in `%s'" (symbol-name (eieio-object-class-name connector))))

(cl-defmethod mrosetta-connector-receive ((connector mrosetta-connector) _key _value _callback _cparameters)
  "Using the specified CONNECTOR, receive structured data entries whose KEY equals to VALUE with specified connector-specific CPARAMETERS. Call back the CALLBACK function when done."
  (error "Connector implementation error: Method mrosetta-connector-receive not implemented in `%s'" (symbol-name (eieio-object-class-name connector))))

(defclass mrosetta-connector-coda (mrosetta-connector)
  ((token
    :initarg :token
    :type string
    :documentation "The bearer token used to authenticate against the Coda API."
    :reader mrosetta-connector-coda-token)))

(cl-defmethod mrosetta-connector-parameters ((_connector-class (subclass mrosetta-connector-coda)))
  "Return the expression-related connector parameters of the Coda CONNETOR-CLASS."
  `(:doc-id
    :table-id))

(cl-defmethod mrosetta-connector-status-code-handlers ((_connector-class (subclass mrosetta-connector-coda)) callback)
  "In context of the CONNETOR-CLASS, by using the provided CALLBACK, generate the standard Coda API status code handlers in form of an alist."
  `((400 . ,(lambda (&rest _) (funcall callback nil :message "Parameters and/or payload invalid!")))
    (401 . ,(lambda (&rest _) (funcall callback nil :message "API token is invalid or has expired!")))
    (403 . ,(lambda (&rest _) (funcall callback nil :message "Beyond API token authorization scope!")))
    (404 . ,(lambda (&rest _) (funcall callback nil :message "Resource could not be located!")))
    (429 . ,(lambda (&rest _) (funcall callback nil :message "Sent too many requests!")))))

(cl-defmethod mrosetta-connector-send ((connector mrosetta-connector-coda) key sdata callback cparameters)
  "Using the specified Coda CONNECTOR, send the SDATA list of processed textual instances, defined by the KEY property, with specified CPARAMETERS. Specify CALLBACK function for response callback."
  (let ((token (mrosetta-connector-coda-token connector))
        (doc-id (plist-get cparameters :doc-id))
        (table-id (plist-get cparameters :table-id))
        (payload `(("rows" . [])
                   ("keyColumns" . ,(vector (symbol-name key))))))
    ;; Process the semantic data into a compatible payload structure, iterating over all provided instances within the semantic data structure
    (setf (cdr (assoc "rows" payload)) (vconcat (mapcar (lambda (instance)
                                                          ;; Map all property-value pairs to a payload-compatible format
                                                          `(("cells" . ,(vconcat (mapcar (lambda (pvpair)
                                                                                           `(("column" . ,(symbol-name (car pvpair)))
                                                                                             ("value" . ,(let ((value (cdr pvpair)))
                                                                                                           (if (listp value)
                                                                                                               (vconcat value)
                                                                                                             value)))))
                                                                                         instance)))))
                                                        sdata)))
    ;; Send the data
    (request
      (concat "https://coda.io/apis/v1/docs/" doc-id "/tables/" table-id "/rows")
      :type "POST"
      :headers `(("Authorization" . ,(concat "Bearer " token))
                 ("Content-type" . "application/json"))
      :data (json-serialize payload)
      :parser 'json-parse-string
      :status-code `((202 . ,(lambda (&rest _) (funcall callback t)))
                     ,@(mrosetta-connector-status-code-handlers (eieio-object-class connector) callback)))))

(cl-defmethod mrosetta-connector-receive ((connector mrosetta-connector-coda) key value callback cparameters)
  "Using the specified Coda CONNECTOR, receive semantic data based on the provided KEY property and corresponding VALUE, with specified CPARAMETERS. Specify CALLBACK function for response callback containing the requested data."
  (let* ((token (mrosetta-connector-coda-token connector))
         (doc-id (plist-get cparameters :doc-id))
         (table-id (plist-get cparameters :table-id))
         ;; Method-internal connector parameters
         (page-token (plist-get cparameters :page-token))
         (sdata (or (plist-get cparameters :sdata) '())))
    ;; Receive the data
    (request
      (concat "https://coda.io/apis/v1/docs/" doc-id "/tables/" table-id "/rows")
      :type "GET"
      :headers `(("Authorization" . ,(concat "Bearer " token)))
      :params (if page-token
                  ;; Fetch the next page of the request in current context
                  `(("pageToken" . ,page-token))
                ;; Create a new request based on parameter criteria
                `(("query" . ,(concat "\"" (symbol-name key) "\""
                                      ":"
                                      (when (stringp value) "\"")
                                      value
                                      (when (stringp value) "\"")))
                  ("useColumnNames" . "true")
                  ("valueFormat" . "simpleWithArrays")))
      :parser 'json-parse-string
      :status-code `((200 . ,(lambda (&rest response)
                               (let* ((data (plist-get response :data))
                                      (payload-items (gethash "items" data))
                                      (next-page-token (gethash "nextPageToken" data)))
                                 ;; Compile the current payload into the sdata object
                                 (and payload-items
                                      (setq sdata `(,@sdata ,@(mapcar (lambda (payload-item)
                                                                        (let ((instance-data '()))
                                                                          (maphash (lambda (key value)
                                                                                     (setq instance-data `(,@instance-data
                                                                                                           (,(intern key) . ,(if (vectorp value) `(,@value) value)))))
                                                                                   (gethash "values" payload-item))
                                                                          instance-data))
                                                                      payload-items))))
                                 (or (and next-page-token
                                          ;; Move on to the next page of data
                                          (mrosetta-connector-receive connector key value callback `(:doc-id ,doc-id
                                                                                                     :table-id ,table-id
                                                                                                     :page-token ,next-page-token
                                                                                                     :sdata ,sdata)))
                                     ;; No more pages, return the compiled data
                                     (funcall callback sdata)))))
                     ,@(mrosetta-connector-status-code-handlers (eieio-object-class connector) callback)))))

(defclass mrosetta-context-org-collection ()
  ((keychain
    :initform (mrosetta-keychain)
    :type mrosetta-keychain
    :documentation "The keychain instance used to generate item keys within the scope of the encompassing collection instance."
    :reader mrosetta-context-org-collection-keychain)
   (items
    :initform '()
    :type list
    :documentation "An alist of items contained within the encompassing collection instance."
    :reader mrosetta-context-org-collection-items))
  "A collection manager of tracked org entry items within a specific scope.")

(cl-defmethod mrosetta-context-org-collection-set ((collection mrosetta-context-org-collection) item)
  "Add or reset the ITEM within the managed org COLLECTION."
  (let* ((item-id (or (mrosetta-context-org-entry-id item)
                      (mrosetta-keychain-generate-key (mrosetta-context-org-collection-keychain collection))))
         (items (setf (slot-value collection 'items) (assq-delete-all item-id (slot-value collection 'items)))))
    (mrosetta-context-org-entry-id-set item item-id)
    ;; Add the new item to collection
    (push `(,item-id . ,item) items)
    item))

(cl-defmethod mrosetta-context-org-collection-get ((collection mrosetta-context-org-collection) item-id)
  "Get an item from the COLLECTION defined by the provided ITEM-ID. Return the item or nil if none present."
  (cdr (assq item-id (mrosetta-context-org-collection-items collection))))

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
  "An org entry reference matched within the Metarosetta framework.")

(defclass mrosetta-context-org-mlexpression (mrosetta-context-org-entry)
  ((mldefinition
    :initarg :mldefinition
    :type list
    :documentation "The metalanguage definition referring to the org entry in context."
    :reader mrosetta-context-org-mlexpression-mldefinition
    :writer mrosetta-context-org-mlexpression-mldefinition-set)
   (cparameters
    :initform '()
    :type list
    :documentation "The list of connector-specific parameters in form of an alist containing parameter-value pairs."
    :reader mrosetta-context-org-mlexpression-cparameters
    :writer mrosetta-context-org-mlexpression-cparameters-set)
   (matches
    :initform (mrosetta-context-org-collection)
    :type mrosetta-context-org-collection
    :documentation "The managed collection of all current matches corresponding to the metalanguage expression in context."
    :reader mrosetta-context-org-mlexpression-matches))
  "An org entry referencing a particular metalanguage definition.")

(defclass mrosetta-context-org-match (mrosetta-context-org-entry)
  ((sync-id
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
   (mlexpressions
    :initform (mrosetta-context-org-collection)
    :type mrosetta-context-org-collection
    :documentation "A managed collection of all defined and tracked metalanguage expressions in scope of the Metarosetta package."
    :reader mrosetta-context-org-db-mlexpressions))
  "The root index object for all metalanguage definitions and matches within the org context.")

(defclass mrosetta-context-org ()
  ((index-file
    :initarg :index-file
    :type string
    :documentation "The file to which the Metarosetta context will persist all current data from the corresponding index datastore."
    :reader mrosetta-context-org-index-file)
   (index
    :type mrosetta-context-org-db
    :documentation "The active datastore of the current Metarosetta context, containing all defined metalagnuage definitions with their respectively tracked matches."
    :reader mrosetta-context-org-index)
   (mlexpressions
    :initform '()
    :type list
    :documentation "The current session's cache containing all compiled metalanguage expressions active in current context."
    :reader mrosetta-context-org-mlexpressions)
   (sync-interval
    :initform 'nil
    :type (or null number)
    :documentation "The synchronization interval for the current context. If non-nil, specifies the number of seconds between synchronization requests using the provided connector instance."
    :reader mrosetta-context-org-sync-interval)
   (connector
    :initarg :connector
    :type mrosetta-connector
    :documentation "The connector instance to use within the current context"
    :reader mrosetta-context-org-connector))
  "The Metarosetta org context object. Handles all Metarosetta-related operations within the org context.")

(cl-defmethod initialize-instance ((context mrosetta-context-org) &rest slots)
  "Initialize the CONTEXT instance by loading the index datastore from file and compiling the Metalanguage expressions, as well as register for saving the index before killing Emacs. Pass along CONTEXT and SLOTS to the default method."
  (let* ((initialized-context (apply #'cl-call-next-method context slots))
         (file (slot-value initialized-context 'index-file)))
    ;; Load the index datastore
    (setf (slot-value initialized-context 'index) (or (eieio-persistent-read file 'mrosetta-context-org-db)
                                                      (mrosetta-context-org-db :file file)))
    ;; Compile the registered Metalanguage expressions
    (setf (slot-value initialized-context 'mlexpressions)
          (mapcar (lambda (org-mlexpression-pair)
                    (let* ((org-mlexpression-id (car org-mlexpression-pair))
                           (org-mlexpression-entry (cdr org-mlexpression-pair))
                           (mlexpression (mrosetta-mlexpression :mldefinition (mrosetta-context-org-mlexpression-mldefinition org-mlexpression-entry))))
                      ;; Parse and compile the metalanguage expression
                      (mrosetta-parse mlexpression)
                      (mrosetta-compile mlexpression)
                      ;; Return the metalanguage expression pair
                      `(,org-mlexpression-id . ,mlexpression)))
                  (mrosetta-context-org-collection-items (mrosetta-context-org-db-mlexpressions (slot-value initialized-context 'index)))))
    ;; Save the index datastore before killing Emacs
    (add-hook 'kill-emacs-hook (lambda ()
                                 (eieio-persistent-save (slot-value initialized-context 'index))))))

(cl-defmethod mrosetta-context-org-process-heading ((context mrosetta-context-org))
  "Process the heading at point by the provided Metarosetta CONTEXT."
  (let ((heading-text (org-get-heading)))
    (and heading-text
         (or (and (string-match "#mrosetta[[:blank:]]+\\(.+\\)" heading-text)
                  ;; Process the Metarosetta Metalanguage expression definition
                  (let* ((input-text (match-string 1 heading-text))
                         (mldefinition (car (read-from-string (concat "(" input-text ")"))))
                         (mlexpression-id (let ((id-property (org-entry-get (point) "mrosetta-mlexpression-id")))
                                            (when (and id-property
                                                       (not (string-empty-p id-property)))
                                              (string-to-number id-property))))
                         (mlexpression-index (mrosetta-context-org-db-mlexpressions (mrosetta-context-org-index context)))
                         (mlexpression-cache (setf (slot-value context 'mlexpressions)
                                                   (assq-delete-all mlexpression-id (slot-value context 'mlexpressions))))
                         (mlexpression-index-entry (or (let ((entry (mrosetta-context-org-collection-get mlexpression-index mlexpression-id)))
                                                         (when entry
                                                           (mrosetta-context-org-mlexpression-mldefinition-set entry mldefinition)
                                                           entry))
                                                       (mrosetta-context-org-mlexpression :org-file (buffer-file-name)
                                                                                          :mldefinition mldefinition)))
                         (mlexpression (mrosetta-mlexpression :mldefinition mldefinition))
                         (connector (mrosetta-context-org-connector context))
                         (cparameters (mrosetta-connector-parameters (eieio-object-class connector))))
                    (prog1 t ;; Regardless of the processing result, the match itself is valid and should return as such
                      ;; Fetch connector-specific parameters from org-entry and update the object
                      (mrosetta-context-org-mlexpression-cparameters-set mlexpression-index-entry
                                                                         (mapcar (lambda (cparameter)
                                                                                   `(,cparameter . ,(org-entry-get (point)
                                                                                                                   (concat "mrosetta-mlexpression-connector-"
                                                                                                                           ;; Convert keyword symbol to simple string
                                                                                                                           (substring (symbol-name cparameter) 1)))))
                                                                                 cparameters))
                      ;; Index the entry
                      (setq mlexpression-index-entry (mrosetta-context-org-collection-set mlexpression-index mlexpression-index-entry))
                      (setq mlexpression-id (mrosetta-context-org-entry-id mlexpression-index-entry))
                      ;; Parse and compile the Metalanguage expression
                      (mrosetta-parse mlexpression)
                      (mrosetta-compile mlexpression)
                      ;; Cache the Metalanguage expression
                      (push `(,mlexpression-id . ,mlexpression) mlexpression-cache)
                      ;; Populate the org entry istelf with Metarosetta properties
                      ;; Expression ID
                      (org-entry-put (point) "mrosetta-mlexpression-id" (number-to-string mlexpression-id))
                      ;; Connector-specific parameters
                      (dolist (cparameter-pair (mrosetta-context-org-mlexpression-cparameters mlexpression-index-entry))
                        (let ((key (car cparameter-pair))
                              (value (cdr cparameter-pair)))
                          (org-entry-put (point)
                                         (concat "mrosetta-mlexpression-connector-"
                                                 (substring (symbol-name key) 1))
                                         (or value ""))))
                      ;; Notify the user
                      (message "Mrosetta Metalanguage expression processed successfully!"))))
             (let* ((mlexpression-cache (mrosetta-context-org-mlexpressions context))
                    (mlexpression-ids (mapcar (lambda (mlexpression-pair) (car mlexpression-pair))
                                              mlexpression-cache)))
               ;; Check if provided heading matches any of the active Metalanguage expressions
               (while (let ((mlexpression-id (pop mlexpression-ids)))
                        (and mlexpression-id
                             (let* ((mlexpression (cdr (assq mlexpression-id mlexpression-cache)))
                                    (exdata (mrosetta-process mlexpression :text heading-text)))
                               ;; If matched, sync through connector in context and stop iterating
                               ;; If not, continue to the next possible Metalanguage expression match
                               (not (and exdata
                                         (let* ((sdata (cdr exdata))
                                                (connector (mrosetta-context-org-connector context))
                                                (mlexpression-index-entry (mrosetta-context-org-collection-get (mrosetta-context-org-db-mlexpressions (mrosetta-context-org-index context))
                                                                                                               mlexpression-id))
                                                (cparameters (mapcan (lambda (cparameter-pair)
                                                                       `(,(car cparameter-pair) ,(cdr cparameter-pair)))
                                                                     (mrosetta-context-org-mlexpression-cparameters mlexpression-index-entry)))
                                                (mlexpression-matches (mrosetta-context-org-mlexpression-matches mlexpression-index-entry))
                                                (match-id (let ((id-property (org-entry-get (point) "mrosetta-match-id")))
                                                            (when (and id-property
                                                                       (not (string-empty-p id-property)))
                                                              (string-to-number id-property))))
                                                (match-index-entry (or (mrosetta-context-org-collection-get mlexpression-matches match-id)
                                                                       (mrosetta-context-org-match :org-file (buffer-file-name))))
                                                (match-sync-id (mrosetta-context-org-match-sync-update match-index-entry)))
                                           (prog1 t ;; Regardless if the sync actually succeeded, the match itself is valid and should return as such
                                             ;; Add index entry, if new
                                             (setq match-index-entry (mrosetta-context-org-collection-set mlexpression-matches match-index-entry))
                                             (setq match-id (mrosetta-context-org-entry-id match-index-entry))
                                             ;; Add sync metadata
                                             (setq sdata `((org-id . ,match-id)
                                                           (org-sync . "yes")
                                                           (org-did-sync . "yes")
                                                           (org-sync-id . ,match-sync-id)
                                                           ,@sdata))
                                             ;; Sync the processed semantic match data
                                             (mrosetta-connector-send connector
                                                                      ;; The identifier property symbol
                                                                      'org-id
                                                                      ;; The list of match instances to send
                                                                      `(,sdata)
                                                                      ;; Callback function
                                                                      (lambda (did-succeed &rest params)
                                                                        ;; Notify the user
                                                                        (if did-succeed
                                                                            (message "Mrosetta Metalanguage match synced successfully!")
                                                                          (let ((msg (plist-get params :message)))
                                                                            (message "Mrosetta send error: %s" msg))))
                                                                      cparameters)
                                             ;; Update the org entry itself
                                             (org-entry-put (point) "mrosetta-match-id" match-id)
                                             (org-entry-put (point) "mrosetta-match-sync-id" match-sync-id))))))))))))))

(cl-defmethod mrosetta-context-org-update ((context mrosetta-context-org))
  "Update all tracked org heading entries within CONTEXT."
  (let ((mlexpression-index (mrosetta-context-org-db-mlexpressions (mrosetta-context-org-index context)))
        (mlexpression-cache (mrosetta-context-org-mlexpressions context))
        (connector (mrosetta-context-org-connector context)))
    ;; Update all tracked matches accross all defined Metalanguage expressions
    (dolist (mlexpression-index-entry (mrosetta-context-org-collection-items mlexpression-index))
      (let* ((mlexpression-id (mrosetta-context-org-entry-id mlexpression-index-entry))
             (mlexpression-matches (mrosetta-context-org-mlexpression-matches mlexpression-index-entry))
             (cparameters (mapcan (lambda (cparameter-pair)
                                    `(,(car cparameter-pair) ,(cdr cparameter-pair)))
                                  (mrosetta-context-org-mlexpression-cparameters mlexpression-index-entry)))
             (mlexpression (cdr (assq mlexpression-id mlexpression-cache))))
        ;; Fetch any match instances marked as unsynced
        (mrosetta-connector-receive connector
                                    ;; The query property symbol
                                    'org-did-sync
                                    ;; The query property value to match
                                    "no"
                                    ;; Callback function
                                    (lambda (sdata &rest params)
                                      (if sdata
                                          (let ((match-sync-ids '()))
                                            (dolist (instance-data sdata)
                                              (let* ((exdata `(,(mrosetta-mlexpression-key mlexpression) . ,instance-data))
                                                     (match-id (cdr (assq 'org-id instance-data)))
                                                     (match-sync-id (cdr (assq 'org-sync-id instance-data)))
                                                     (match-index-entry (mrosetta-context-org-collection-get mlexpression-matches match-id))
                                                     (match-org-file (mrosetta-context-org-entry-file match-index-entry)))
                                                ;; Push match ID for response payload
                                                (push `(,match-id . ,match-sync-id) match-sync-ids)
                                                ;; Update the org heading
                                                (org-map-entries (lambda ()
                                                                   (let* ((heading-text (org-get-heading))
                                                                          (updated-heading-text (mrosetta-update mlexpression
                                                                                                                 :text heading-text
                                                                                                                 :sdata exdata)))
                                                                     ;; Update the heading text
                                                                     (while (or (char-equal (following-char) ?*)
                                                                                (char-equal (following-char) ?\s))
                                                                       ;; Skip the heading markup
                                                                       (forward-char))
                                                                     (delete-region (point) (line-end-position))
                                                                     (insert updated-heading-text)
                                                                     ;; Update the heading properties
                                                                     (org-entry-put (point) "mrosetta-match-sync-id" match-sync-id)
                                                                     ;; Update the match index entry
                                                                     (mrosetta-context-org-match-sync-id-set match-index-entry match-sync-id)
                                                                     ;; Notify the user
                                                                     (message "Mrosetta Metalanguage match with id %s updated in file %s!"
                                                                              (number-to-string match-id)
                                                                              (number-to-string match-org-file))))
                                                                 (concat "+mrosetta-match-id="
                                                                         "\"" match-id "\"")
                                                                 `(,match-org-file))))
                                            ;; Send response confirmation
                                            (let ((response-sdata (mapcar (lambda (match-sync-id-pair)
                                                                            `((org-id . ,(car match-sync-id-pair))
                                                                              (org-did-sync . "yes")
                                                                              (org-sync-id . ,(cdr match-sync-id-pair))))
                                                                          match-sync-ids)))
                                              (mrosetta-connector-send connector
                                                                       'org-id
                                                                       response-sdata
                                                                       (lambda (did-succeed &rest params)
                                                                         (if did-succeed
                                                                             (message "Mrosetta update for Metalanguage expression with id %s confirmed successfully!"
                                                                                      (number-to-string mlexpression-id))
                                                                           (let ((msg (plist-get params :message)))
                                                                             (message "Mrosetta send error: %s" msg))))
                                                                       cparameters)))
                                        (let ((msg (plist-get params :message)))
                                          (message "Mrosetta receive error: %s" msg))))
                                    cparameters)))))

(cl-defmethod mrosetta-context-org-sync ((context mrosetta-context-org))
  "Update all tracked org headings of defined Metalanguage expressions within provided CONTEXT. Repeat every SYNC-INTERVAL specified within the CONTEXT slot, or nil for no repitition."
  (let ((sync-interval (mrosetta-context-org-sync-interval context)))
    ;; Run update within context
    (mrosetta-context-org-update context)
    ;; Schedule next update, if sync-interval set
    (when sync-interval
      (run-at-time (format "%s min" (number-to-string sync-interval))
                   nil
                   #'mrosetta-context-org-sync
                   context))))

(cl-defmethod mrosetta-context-org-sync-start ((context mrosetta-context-org) sync-interval)
  "Commence with periodic synchronization within the specified CONTEXT. SYNC-INTERVAL specifies the synchronization interval in minutes."
  (when (setf (slot-value context 'sync-interval) sync-interval)
    (mrosetta-context-org-sync context)))

(cl-defmethod mrosetta-context-org-sync-stop ((context mrosetta-context-org))
  "Stop the periodic synchronization within the specified CONTEXT."
  (setf (slot-value context 'sync-interval) nil))

(provide 'metarosetta)

;;; metarosetta.el ends here
