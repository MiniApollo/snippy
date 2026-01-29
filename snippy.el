;;; snippy.el --- Vscode snippets support for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Mark Surmann
;; Keywords: snippet
;; Package-Requires: ((emacs "25.1") )
;; Version: 0.1.0

;;; Commentary:

;; Vscode snippets support for Emacs

;;; Code:

;; TODO:
;; Only set vars for this package not change it globally
;;(setq-local json-object-type 'alist) ;; To set type default is alist
;(setq-local json-array-type 'list) ;; Default is vector e.g [Hello there my name is]

(defvar snippy/package-json-location "./friendly-snippets/package.json" "Location of the package.json file")
(defvar snippy/package-json-content (json-read-file snippy/package-json-location))

;; Vscode engine check

(defun snippy/clean-version-string (version)
  "Remove common semver prefixes like ^ or ~ from VERSION string."
  (if (and (stringp version) (string-match "\\([0-9.]+\\)" version))
      (match-string 1 version)
    version))

(defun snippy/get-engine-vscode-version ()
  (snippy/clean-version-string (alist-get 'vscode (alist-get 'engines snippy/package-json-content))))

(defvar snippy/engine-vscode-version (snippy/get-engine-vscode-version)
  "Current Engine vscode version")

(defvar snippy/minimal-engine-vscode-version "1.11.0"
  "The minimum version required for this package.")

(defun snippy/check-engine-version()
  (if (version<= snippy/minimal-engine-vscode-version snippy/engine-vscode-version)
      (message "Okey %s" snippy/engine-vscode-version)
    (message "Bady %s" snippy/engine-vscode-version))
  )

;(snippy/check-engine-version)

;; Read in by language
;; Snippets
(setq-local snippy/snippets-paths (alist-get 'snippets (alist-get 'contributes snippy/package-json-content)))
;(message "%s" snippy/snippets-paths)

;; Get language paths
;; AI slop warning
;; But it works
(defun snippy/get-all-paths-by-language (my-snippet-data target-lang)
  "Return a list of all paths associated with TARGET-LANG."
  (let ((target (if (symbolp target-lang) (symbol-name target-lang) target-lang)))
    (seq-map
     (lambda (entry) (cdr (assoc 'path entry)))
     (seq-filter
      (lambda (entry)
        (let ((val (cdr (assoc 'language entry))))
          (if (vectorp val)
              ;; If vector, convert elements to strings and check
              (seq-some (lambda (x) (string-equal (format "%s" x) target)) val)
            ;; If single value, format as string and compare
            (string-equal (format "%s" val) target))))
      my-snippet-data))))

; Test writeout
;(message "Result for C: %s" (snippy/get-all-paths-by-language snippy/snippets-paths "cpp"))
;(message "Result for Markdown: %s" (snippy/get-all-paths-by-language snippy/snippets-paths "rust"))

(setq-local snippy/current-language "css")
(setq-local snippy/current-language-path (snippy/get-all-paths-by-language snippy/snippets-paths snippy/current-language))
;(message "%s" snippy/current-language-path)

;; Read in snippets
;; Assuming snippy/current-language-path is defined elsewhere as a list of strings

(defvar snippy/merged-snippets
  (let ((base-dir "./friendly-snippets/"))
    (mapcan (lambda (suffix)
              (let ((full-path (concat base-dir suffix)))
                (if (file-exists-p full-path)
                    (json-read-file full-path)
                  (ignore (message "Skipping: %s (not found)" full-path)))))
            snippy/current-language-path))
  "A merged alist of all snippets found in the paths defined by snippy/current-language-path.")

;; (message "%s" snippy/merged-snippets)

;; Search for snippet
(require 'seq)

(defun snippy/find-snippet-by-prefix (prefix snippets)
  "Return the first snippet entry where the prefix matches PREFIX."
  ;; Or seq-filter
  (seq-find (lambda (snippet)
              (let ((snippet-data (cdr snippet))) ; Get the (prefix . "...") part
                (string= (cdr (assoc 'prefix snippet-data)) prefix)))
            snippets))


;; TODO:
;; Built in var support
;; Fix variable update on default chars

;; Snippet expansion
;; More AI Slop :D
;; The Main function
(defun snippy/expand-snippet (prefix)
  "Finds a snippet by PREFIX and expands it, handling VS Code's body format."
  (interactive "sEnter snippet name: ")
  (let* ((snippet-data (snippy/find-snippet-by-prefix prefix snippy/merged-snippets))
         ;; Extract the 'body' from the alist
         (body (cdr (assoc 'body snippet-data))))
    (if body
        (let ((body-str (if (vectorp body)
                            (mapconcat #'identity body "\n")
                          body)))
          (my/expand-snippet-at-point body-str))
      (message "Snippet '%s' not found" prefix))))

(require 'yasnippet)
(require 'subr-x)

(defun snippy--get-variable-value (var-name)
  "Resolves VS Code variables to their Emacs string values."
  (let ((file-name (buffer-file-name)))
    (pcase var-name
      ("TM_SELECTED_TEXT" (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) ""))
      ("TM_CURRENT_LINE" (thing-at-point 'line t))
      ("TM_CURRENT_WORD" (or (thing-at-point 'word t) ""))
      ("TM_LINE_INDEX" (number-to-string (1- (line-number-at-pos))))
      ("TM_LINE_NUMBER" (number-to-string (line-number-at-pos)))
      ("TM_FILENAME" (if file-name (file-name-nondirectory file-name) "Untitled"))
      ("TM_FILENAME_BASE" (if file-name (file-name-base file-name) "Untitled"))
      ("TM_DIRECTORY" (if file-name (file-name-directory file-name) default-directory))
      ("TM_DIRECTORY_BASE" (file-name-nondirectory (directory-file-name (if file-name (file-name-directory file-name) default-directory))))
      ("TM_FILEPATH" (or file-name ""))
      ("RELATIVE_FILEPATH" (if file-name (file-relative-name file-name) ""))
      ("CLIPBOARD" (or (current-kill 0) ""))
      ("WORKSPACE_NAME" (if (project-current) (file-name-nondirectory (directory-file-name (project-root (project-current)))) "No Workspace"))
      ("WORKSPACE_FOLDER" (if (project-current) (project-root (project-current)) default-directory))

      ;; Dates
      ("CURRENT_YEAR" (format-time-string "%Y"))
      ("CURRENT_YEAR_SHORT" (format-time-string "%y"))
      ("CURRENT_MONTH" (format-time-string "%m"))
      ("CURRENT_MONTH_NAME" (format-time-string "%B"))
      ("CURRENT_MONTH_NAME_SHORT" (format-time-string "%b"))
      ("CURRENT_DATE" (format-time-string "%d"))
      ("CURRENT_DAY_NAME" (format-time-string "%A"))
      ("CURRENT_DAY_NAME_SHORT" (format-time-string "%a"))
      ("CURRENT_HOUR" (format-time-string "%H"))
      ("CURRENT_MINUTE" (format-time-string "%M"))
      ("CURRENT_SECOND" (format-time-string "%S"))
      ("CURRENT_SECONDS_UNIX" (format-time-string "%s"))

      ;; Randoms
      ("RANDOM" (format "%06d" (random 1000000)))
      ("RANDOM_HEX" (format "%06x" (random 16777215)))
      ("UUID" (if (fboundp 'org-id-uuid) (org-id-uuid) (format "%06x%06x" (random 16777215) (random 16777215))))

      ;; Fallback
      (_ nil))))

(defun snippy--perform-transform (value regex format options)
  "Applies a VSCode style transform to a value string."
  ;; Note: Translating JS Regex to Emacs Regex perfectly is difficult.
  ;; We assume standard regex compatibility for simple cases.
  (condition-case nil
      (let ((case-fold-search (string-match-p "i" options)))
        (replace-regexp-in-string regex format value nil nil))
    (error value)))

(defun snippy--replace-variables (snippet)
  "Resolves VS Code variables, ignoring numeric tabstops."
  (let ((result snippet)
        (case-fold-search nil))

    ;; 1. Handle Transformations: ${VAR/Regex/Format/Options}
    ;; Ensure we only match VARs (letters), not numeric indices
    (while (string-match "\\${\\([A-Z_][A-Z_0-9]*\\)/\\([^/]+\\)/\\([^/]*\\)/\\([a-z]*\\)}" result)
      (let* ((var-name (match-string 1 result))
             (regex (match-string 2 result))
             (fmt (match-string 3 result))
             (opts (match-string 4 result))
             (val (snippy--get-variable-value var-name)))
        (setq result (replace-match
                      (if val (snippy--perform-transform val regex fmt opts) "")
                      t t result))))

    ;; 2. Handle Variables with Defaults: ${VAR:default}
    ;; The [A-Z_] start ensures we don't touch ${1:default}
    (while (string-match "\\${\\([A-Z_][A-Z_0-9]*\\)\\(?::\\([^}]*\\)\\)?}" result)
      (let* ((var-name (match-string 1 result))
             (default-val (or (match-string 2 result) ""))
             (val (snippy--get-variable-value var-name)))
        (when (string-match-p "\\$" default-val)
          (setq default-val (snippy--replace-variables default-val)))
        (setq result (replace-match (or val default-val) t t result))))

    ;; 3. Handle Simple Variables: $VAR
    ;; Use \\b (word boundary) to prevent matching $1 as part of a variable
    (let ((start 0))
      (while (string-match "\\$\\([A-Z_][A-Z_0-9]*\\)\\b" result start)
        (let* ((var-name (match-string 1 result))
               (val (snippy--get-variable-value var-name)))
          (if val
              (setq result (replace-match val t t result))
            (setq start (match-end 0))))))
    result))

(defun snippy--convert-choices (snippet)
  "Convert VS Code choices ${1|one,two|} to YASnippet format."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "\\${\\([0-9]+\\)|\\([^|]*\\)|}"
     (lambda (match)
       ;; IMPORTANT: Use 'snippet' here, not 'match'
       (let* ((index (match-string 1 snippet))
              (choice-str (match-string 2 snippet))
              ;; Split the choices and clean up whitespace
              (choices (split-string choice-str "," t "[ \t\n\r]+"))
              (lisp-list (format "'(%s)"
                                 (mapconcat #'prin1-to-string choices " "))))
         (format "${%s:$$(yas-choose-value %s)}" index lisp-list)))
     snippet t t)))

;; Testing the fix:
(let ((input "align-items: ${1|flex-start,flex-end,center|};"))
  (insert (snippy--convert-choices input)))align-items: ${1|flex${i:$$(yas-choose-value '("n-items: ${1|flex-start" "fl"))}start,flex-end,center|};

(defun my/expand-snippet-at-point (snippet-content)
  "Expands VS Code format SNIPPET-CONTENT at point with interactive choices."
  (unless (featurep 'yasnippet)
    (user-error "Yasnippet is required"))

  (let ((processed snippet-content))
    ;; 1. Handle Vector body
    (when (vectorp processed)
      (setq processed (mapconcat #'identity processed "\n")))

    ;; 2. Convert Choices FIRST
    (setq processed (snippy--convert-choices processed))

    ;; 3. Resolve Variables
    (setq processed (snippy--replace-variables processed))

    ;; 4. Expand
    (yas-expand-snippet processed)))
