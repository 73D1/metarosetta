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
(require 'request)

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
   (rsuffix
    :initform 'nil
    :type (or null string)
    :documentation "The regular expression matching a specified suffix of the encompassing expression instance. Either a regex string or nil."
    :reader mrosetta-rsuffix)
   (left-rboundary
    :initform 'nil
    :type (or null string)
    :documentation "The left regex-specific boundary defining the beginning of the match."
    :reader mrosetta-left-rboundary)
   (right-rboundary
    :initform 'nil
    :type (or null string)
    :documentation "The right regex-specific boundary defining the end of the match."
    :reader mrosetta-right-rboundary)
   (rbuffer
    :initform "[[:blank:]]*"
    :type string
    :documentation "The regular expression matching buffer characters surrounding the encompassing expression."
    :reader mrosetta-rbuffer)
   (left-rbuffer-key
    :type number
    :documentation "The regex group key for the encompassing expression's left buffer match."
    :reader mrosetta-left-rbuffer-key)
   (right-rbuffer-key
    :type number
    :documentation "The regex group key for the encompassing expression's right buffer match."
    :reader mrosetta-right-rbuffer-key)
   (key
    :initform 'nil
    :type (or null symbol)
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
   (is-contextual
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is matched elastically depending on neighboring elements. Either non-nil or nil."
    :reader mrosetta-is-contextual)
   (modifier
    :initform 'nil
    :type (or null symbol)
    :documentation "Specifies a symbol referencing a stored modifier function, if any. Either a symbol or nil."
    :reader mrosetta-modifier)
   (is-optional
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is optional to match within input text. Either non-nil or nil."
    :reader mrosetta-is-optional)
   (should-ignore
    :initform 'nil
    :documentation "Specifies whether the encompassing expression should be matched but disregarded in output. Either non-nil or nil."
    :reader mrosetta-should-ignore)
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
         (regex-key (mrosetta-generate-regex-key rkeychain))
         (rinstance)
         (rinstance-key (mrosetta-generate-regex-key rkeychain))
         (rmatch (slot-value mlexpression 'rmatch))
         (rmatch-key (mrosetta-generate-regex-key rkeychain))
         (rprefix (slot-value mlexpression 'rprefix))
         (rsuffix (slot-value mlexpression 'rsuffix))
         (left-rboundary (slot-value mlexpression 'left-rboundary))
         (right-rboundary (slot-value mlexpression 'right-rboundary))
         (rbuffer (slot-value mlexpression 'rbuffer))
         (left-rbuffer-key (mrosetta-generate-regex-key rkeychain))
         (right-rbuffer-key (mrosetta-generate-regex-key rkeychain))
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
    (or (when (and (mrosetta-is-contextual mlexpression) is-inner)
          ;; Return the full inner-text matched within the parent expression if not marked as ignorable
          (when (and (eq (mrosetta-extype mlexpression) :match)
                     (not (mrosetta-should-ignore mlexpression)))
            `(,(or (mrosetta-key mlexpression) :nokey) . ,htext)))
        (save-match-data
          (and htext
               (string-match (mrosetta-regex mlexpression) htext)
               ;; Found match for the entirety of the expression
               (let ((extext (match-string (mrosetta-regex-key mlexpression) htext))
                     (pos))
                 (save-match-data
                   ;; Iterate over all instance occurrences within the matching expression text
                   (while (string-match (mrosetta-rinstance mlexpression) extext pos)
                     (setq pos (match-end 0))
                     ;; Process the exact match as defined by the expression
                     (let ((instance-exdata))
                       ;; Cases where the expression is a :fractal
                       (when (eq (mrosetta-extype mlexpression) :fractal)
                         ;; Recursively process all fractals within
                         (let ((fractals (mrosetta-fractals mlexpression)))
                           (dolist (fractal fractals)
                             (let ((fractal-exdata (mrosetta-process fractal :inner (match-string (mrosetta-regex-key fractal) extext))))
                               (when fractal-exdata
                                 (setq instance-exdata `(,@instance-exdata ,fractal-exdata)))))))
                       ;; Cases where the expression is a :match
                       (when (and (eq (mrosetta-extype mlexpression) :match)
                                  (not (mrosetta-should-ignore mlexpression)))
                         ;; Just store the semantic end-match, modified if defined as such
                         (let ((match (match-string (mrosetta-rmatch-key mlexpression) extext))
                               (modifier (mrosetta-modifier mlexpression)))
                           (when modifier
                             (setq match (funcall modifier match)))
                           (setq instance-exdata match)))
                       (when instance-exdata
                         (setq exdata `(,@exdata ,instance-exdata)))))
                   (when exdata
                     ;; Splice instance data in case of a singular expression
                     (when (not (mrosetta-is-plural mlexpression))
                       (setq exdata (car exdata)))
                     ;; Return the structured semantic data object
                     `(,(or (mrosetta-key mlexpression) :nokey) . ,exdata)))))))))

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
                        (or (mrosetta-key mlexpression) :nokey))))
      (error "Data structure error: Key mismatch"))
    (or (when (and (mrosetta-is-contextual mlexpression) is-inner)
          ;; Return the full inner-text matched within the parent expression or the updated text passed in
          (or exdata htext))
        (save-match-data
          (and htext
               (string-match (mrosetta-regex mlexpression) htext)
               ;; Found metalanguage expression match
               (let ((extext (match-string (mrosetta-regex-key mlexpression) htext))
                     (pos))
                 (save-match-data
                   (while (string-match (mrosetta-rinstance mlexpression) extext pos)
                     (setq pos (match-end 0))
                     ;; Update each instance
                     (let ((instance-exdata (if (mrosetta-is-plural mlexpression) (pop exdata) exdata))
                           (instance-newtext))
                       (if (eq (mrosetta-extype mlexpression) :fractal)
                           ;; Recursively update all fractals within
                           (let ((fractals (mrosetta-fractals mlexpression)))
                             (dolist (fractal fractals)
                               (let* ((fractal-exdata (assq (mrosetta-key fractal) instance-exdata))
                                      (fractal-text (match-string (mrosetta-regex-key fractal) extext))
                                      (fractal-newtext (mrosetta-update fractal :inner fractal-text :sdata fractal-exdata)))
                                 (setq instance-newtext (concat instance-newtext fractal-newtext)))))
                         ;; Update end-elements
                         (let ((left-buffer (match-string (mrosetta-left-rbuffer-key mlexpression) extext))
                               (right-buffer (match-string (mrosetta-right-rbuffer-key mlexpression) extext)))
                           (when (eq (mrosetta-extype mlexpression) :match)
                             ;; Update match text, including ignorable matches
                             (let ((prefix (mrosetta-match-prefix mlexpression))
                                   (suffix (mrosetta-match-suffix mlexpression))
                                   (match (or instance-exdata
                                              (match-string (mrosetta-rmatch-key mlexpression) extext))))
                               (setq instance-newtext (concat left-buffer prefix match suffix right-buffer))))
                           (when (eq (mrosetta-extype mlexpression) :literal)
                             ;; Include the literal, with surrounding buffer
                             (let ((literal (mrosetta-match-literal mlexpression)))
                               (setq instance-newtext (concat left-buffer literal right-buffer))))))
                       (setq newtext (concat newtext instance-newtext))))
                   ;; Return the updated text
                   newtext)))))))

(cl-defgeneric mrosetta-send (connector sdata &rest cparameters)
  "Using the specified CONNECTOR, send the SDATA containing a list of processed text instances with specified connector-specific CPARAMETERS.")

(cl-defgeneric mrosetta-receive (connector &rest cparameters)
  "Using the specified CONNECTOR, receive structured data with specified connector-specific CPARAMETERS.")

(defclass mrosetta-connector-coda ()
  ((token
    :initarg :token
    :type string
    :documentation "The bearer token used to authenticate against the Coda API."
    :reader mrosetta-connector-coda-token)))

(defun mrosetta-connector-coda-get-status-code-handlers (callbackfn)
  "By using the provided CALLBACKFN, generate the standard Coda API status code handlers in form of an alist."
  `((400 . ,(lambda (&rest _) (funcall callbackfn nil :message "Parameters and/or payload invalid!")))
    (401 . ,(lambda (&rest _) (funcall callbackfn nil :message "API token is invalid or has expired!")))
    (403 . ,(lambda (&rest _) (funcall callbackfn nil :message "Beyond API token authorization scope!")))
    (404 . ,(lambda (&rest _) (funcall callbackfn nil :message "Resource could not be located!")))
    (429 . ,(lambda (&rest _) (funcall callbackfn nil :message "Sent too many requests!")))))

(cl-defmethod mrosetta-send ((connector mrosetta-connector-coda) sdata &rest cparameters)
  "Using the specified Coda CONNECTOR, send the SDATA list of processed textual instances with specified CPARAMETERS. Specify :callback function for response callback."
  (let ((token (mrosetta-connector-coda-token connector))
        (doc-id (plist-get cparameters :doc-id))
        (table-id (plist-get cparameters :table-id))
        (payload `(("rows" . [])
                   ("keyColumns" . ,(vector (symbol-name (plist-get cparameters :id-property))))))
        (callback (plist-get cparameters :callback)))
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
                     ,@(mrosetta-connector-coda-get-status-code-handlers callback)))))

(cl-defmethod mrosetta-receive ((connector mrosetta-connector-coda) &rest cparameters)
  "Using the specified Coda CONNECTOR, receive semantic data based on the query parameters of :property and :value within CPARAMETERS. Specify :callback function for response callback containing the requested data."
  (let* ((token (mrosetta-connector-coda-token connector))
         (doc-id (plist-get cparameters :doc-id))
         (table-id (plist-get cparameters :table-id))
         (property (plist-get cparameters :property))
         (pvalue (plist-get cparameters :pvalue))
         (page-token (plist-get cparameters :page-token))
         (sdata (or (plist-get cparameters :sdata) '()))
         (callback (plist-get cparameters :callback)))
    ;; Receive the data
    (request
      (concat "https://coda.io/apis/v1/docs/" doc-id "/tables/" table-id "/rows")
      :type "GET"
      :headers `(("Authorization" . ,(concat "Bearer " token)))
      :params (if page-token
                  ;; Fetch the next page of the request in current context
                  `(("pageToken" . ,page-token))
                ;; Create a new request based on parameter criteria
                `(("query" . ,(concat "\"" (symbol-name property) "\""
                                      ":"
                                      (when (stringp pvalue) "\"")
                                      pvalue
                                      (when (stringp pvalue) "\"")))
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
                                          (mrosetta-receive connector :doc-id doc-id :table-id table-id :page-token next-page-token :sdata sdata :callback callback))
                                     ;; No more pages, return the compiled data
                                     (funcall callback sdata)))))
                     ,@(mrosetta-connector-coda-get-status-code-handlers callback)))))

(provide 'metarosetta)

;;; metarosetta.el ends here
