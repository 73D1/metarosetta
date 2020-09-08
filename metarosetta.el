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
    :documentation "The last key generated and assigned to a group within the context of a single instance."
    :reader mrosetta-lastkey)
   (keys
    :initform '()
    :type list
    :documentation "A property list containing all the generated keys and corresponding references of respectively assigned objects."
    :reader mrosetta-keys))
  "A regex group key generator and manager.")

(cl-defmethod mrosetta-generate-rkey ((keychain mrosetta-keychain) mlexpression)
  "Register for a new key with a particular MLEXPRESSION instance within a provided KEYCHAIN."
  (let ((key (+ 1 (slot-value keychain 'lastkey))))
    (setf (slot-value keychain 'keys) (plist-put (slot-value keychain 'keys) key mlexpression))
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
    :documentation "A symbol specifying the type of the encompassing expression instance. Can be either a :literal, :context, :word or :fractal."
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
   (rkey
    :type number
    :documentation "The regex matching group key for the encompassing expression instance."
    :reader mrosetta-rkey)
   (regex
    :type string
    :documentation "The compiled regular expression of the expression in context."
    :reader mrosetta-regex)
   (rmatch
    :type string
    :documentation "The textual match of the encompassing expression within the currently set input."
    :reader mrosetta-rmatch)
   (key
    :type string
    :documentation "The property key to which the expression output value is assigned, if any."
    :reader mrosetta-key)
   (value
    :type (or string list)
    :documentation "The output value matching the encompassing expression instance within the currently set input."
    :reader mrosetta-value)
   (is-upparcase
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches only uppercase words. Either non-nil or nil."
    :reader mrosetta-is-uppercase)
   (is-capitalized
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches only capitalized words. Either non-nil or nil."
    :reader mrosetta-is-capitalized)
   (match-substring
    :initform 'nil
    :type (or null string)
    :documentation "Specifies a specific substring all possible expression matches should contain, if any. Either a string or nil."
    :reader mrosetta-match-substring)
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
   (match-literal
    :initform 'nil
    :type (or null string)
    :documentation "Specifies the literal string that the expression maches exclusively. Either a string or nill."
    :reader mrosetta-match-literal)
   (should-uppercase
    :initform 'nil
    :documentation "Specifies whether the original encompassing expression match should get uppercased."
    :reader mrosetta-should-uppercase)
   (should-lowercase
    :initform 'nil
    :documentation "Specifies whether the original encompassing expression match should get lowercased."
    :reader mrosetta-should-lowercase)
   (has-plural-value
    :initform 'nil
    :documentation "Specifies whether the encompassing expression matches plural values or just a single one. Either nil or non-nil."
    :reader mrosetta-has-plural-value)
   (is-optional
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is optional to match within input text. Either non-nil or nil."
    :reader mrosetta-is-optional)
   (has-key
    :initform 'nil
    :documentation "Specifies whether the encompassing expression is assigned a key for its output. Either nil or non-nil."
    :reader mrosetta-has-key)
  )
  "The Metarosetta Expression object used to define a contextual translational expression for semantic processing.")

(cl-defmethod mrosetta-parse-literal ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse the :right arg content within ARGS as a literal quote into the MLEXPRESSION instance in context."
  (let ((literal-quote (plist-get args :right)))
    (when (eq literal-quote nil)
      (error "Metalanguage syntax error: Literal expression without quoted content"))
    (setf (slot-value mlexpression 'extype) :literal)
    (setf (slot-value mlexpression 'match-literal) literal-quote))
  (plist-put args :right nil))

(cl-defmethod mrosetta-parse-word ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'extype) :word)
  args)

(cl-defmethod mrosetta-parse-word-uppercase ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse an uppercase word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-uppercase) t)
  (mrosetta-parse-word mlexpression args))

(cl-defmethod mrosetta-parse-word-capitalized ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse a capitalized word expression into the MLEXPRESSION instance in context. This expression utilizes no ARGS."
  (setf (slot-value mlexpression 'is-capitalized) t)
  (mrosetta-parse-word mlexpression args))

(cl-defmethod mrosetta-parse-word-content ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse quoted text from :right arg within ARGS as matching word content into the MLEXPRESSION instance in context."
  (let ((substring-quote (plist-get args :right)))
    (when (eq substring-quote nil)
      (error "Metalanguage syntax error: Substring match expression without quoted content"))
    (setf (slot-value mlexpression 'match-substring) substring-quote))
  (plist-put args :right nil))

(cl-defmethod mrosetta-parse-word-prefix ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse quoted text from :left arg within ARGS as matching word prefix into the MLEXPRESSION instance in context."
  (let ((prefix-quote (plist-get args :left)))
    (when (eq prefix-quote nil)
      (error "Metalanguage syntax error: Prefix match expression without quoted content"))
    (setf (slot-value mlexpression 'match-prefix) prefix-quote))
  (plist-put args :left nil))

(cl-defmethod mrosetta-parse-word-suffix ((mlexpression mrosetta-mlexpression) &rest args)
  "Parse quoted text from :left arg within ARGS as matching word suffix into the MLEXPRESSION instance in context."
  (let ((suffix-quote (plist-get args :left)))
    (when (eq suffix-quote nil)
      (error "Metalanguage syntax error: Suffix match expression without quoted content"))
    (setf (slot-value mlexpression 'match-suffix) suffix-quote))
  (plist-put args :left nil))

(provide 'metarosetta)

;;; metarosetta.el ends here
