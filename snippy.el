;;; snippy.el --- Vscode snippets support for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Mark Surmann
;; Keywords: snippet
;; Package-Requires: ((emacs "25.1") )
;; Version: 0.1.0

;;; Commentary:

;; Vscode snippets support for Emacs

;;; Code:

;; TODO
;; Major mode loading
;; Clean up for release

;; Not very interesting
;; Variable-Transform
;; Placeholder-Transform
;; Grammar

(defvar snippy/snippet-dir "./friendly-snippets/" "Snippets directory")
(defvar snippy/package-json-content (json-read-file (format "%spackage.json" snippy/snippet-dir)))

(defvar snippy/minimal-engine-vscode-version "1.11.0"
  "The minimum version required for this package.")

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

(defun snippy/check-engine-version()
  (if (version<= snippy/minimal-engine-vscode-version snippy/engine-vscode-version)
      (message "Okey %s" snippy/engine-vscode-version)
    (message "Bady %s" snippy/engine-vscode-version)))

(snippy/check-engine-version)

;; Read in by language
;; Snippets
(defvar snippy/snippets-paths (alist-get 'snippets (alist-get 'contributes snippy/package-json-content)))
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

(defvar snippy/current-language "markdown"
  "The language currently used by snippy in the local buffer.")

(defvar snippy/current-language-path (snippy/get-all-paths-by-language snippy/snippets-paths snippy/current-language))
;(message "%s" snippy/current-language-path)

;; Read in snippets
(defvar snippy/merged-snippets
  (mapcan (lambda (suffix)
            (let ((full-path (concat snippy/snippet-dir suffix)))
              (if (file-exists-p full-path)
                  (json-read-file full-path)
                (ignore (message "Skipping: %s (not found)" full-path)))))
          snippy/current-language-path)
  "A merged alist of all snippets found in the paths defined by snippy/current-language-path.")

;; (message "%s" snippy/merged-snippets)

;; Search for snippet
(require 'seq)
(defun snippy/find-snippet-by-prefix (prefix snippets)
  "Return the first snippet entry where the prefix matches PREFIX."
  (seq-find (lambda (snippet)
              (let* ((snippet-data (cdr snippet))
                     (prefix-val (cdr (assoc 'prefix snippet-data))))
                (cond
                 ;; If it's a string, compare directly
                 ((stringp prefix-val)
                  (string= prefix-val prefix))
                 ;; If it's a vector or list, check if the prefix is inside it
                 ((sequencep prefix-val)
                  (seq-contains-p prefix-val prefix))
                 (t nil))))
            snippets))

;; Expand Snippet
(defun snippy/expand-snippet-by-prefix (prefix)
  (interactive "sEnter snippet name: ")
  ;; (unless (featurep 'yasnippet)
  ;;   (user-error "Yasnippet is required for this function"))

  (message "%s" (snippy/find-snippet-by-prefix prefix snippy/merged-snippets))
  (snippy/expand-snippet (snippy/find-snippet-by-prefix prefix snippy/merged-snippets))
  )

(require 'yasnippet)

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

(defun snippy/expand-snippet (snippet)
  "Convert LSP-style choices to YASnippet elisp and expand, handling strings or vectors."
  (let* ((body-raw (cdr (assoc 'body snippet)))
         ;; If it's a vector, join it. If it's already a string, use it.
         (body-str (cond ((vectorp body-raw) (mapconcat #'identity body-raw "\n"))
                         ((stringp body-raw) body-raw)
                         (t "")))
         ;; Choices
         ;; Convert ${1|a,b|} to ${1:$$(yas-choose-value '("a" "b"))}
         (body-choices
          (replace-regexp-in-string
           "\\${\\([0-9]+\\)|\\([^|]+\\)|}"
           (lambda (match)
             (save-match-data  ;; <--- Essential to protect the outer match
               (let* ((index (match-string 1 match))
                      (choice-str (match-string 2 match))
                      (choices (split-string choice-str ","))
                      (lisp-list (format "'(%s)"
                                         (mapconcat #'prin1-to-string choices " "))))
                 (format "${%s:$$(yas-choose-value %s)}" index lisp-list))))
           body-str t t))
         ;; Variables
         (final-body
          (replace-regexp-in-string
           "\\${\\([A-Z_]+\\)}"
           (lambda (match)
             (let ((var-name (match-string 1 match)))
               ;; Call your helper function.
               ;; We use (or ... var-name) as a fallback if the function returns nil.
               (or (snippy--get-variable-value var-name) var-name)))
           body-choices t t)))

    (yas-expand-snippet final-body)))
