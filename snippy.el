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
(setq-local json-array-type 'list) ;; Default is vector e.g [Hello there my name is]

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
(message "%s" snippy/snippets-paths)

;; (defvar my-snippet-data
;;   [((language . ["plaintext" "markdown" "tex" "html" "global" "all"]) (path . "./snippets/global.json"))
;;    ((language . ["license"]) (path . "./snippets/license.json"))
;;    ((language . ["c"]) (path . "./snippets/c/c.json"))
;;    ((language . ["cdoc"]) (path . "./snippets/c/cdoc.json"))
;;    ((language . ["cpp"]) (path . "./snippets/cpp/cpp.json"))
;;    ((language . ["c"]) (path . "./snippets/extra-c-stuff.json"))] ;; Example of a duplicate
;;   "The snippet data structure as a vector of alists.")

(defun get-all-paths-by-language (my-snippet-data target-lang)
  "Return a list of all paths associated with TARGET-LANG."
  (let* ((lang-str (if (symbolp target-lang) (symbol-name target-lang) target-lang))
         ;; 1. Filter the vector for all matching entries
         (matches (seq-filter
                   (lambda (entry)
                     (seq-contains-p (cdr (assoc 'language entry)) lang-str))
                   my-snippet-data)))
    ;; 2. Extract only the 'path' from those entries and return as a list
    (seq-map (lambda (entry) (cdr (assoc 'path entry))) matches)))

;; --- Usage ---
(message "%s" (get-all-paths-by-language snippy/snippets-paths "c"))
;; => ("./snippets/c/c.json" "./snippets/extra-c-stuff.json")

(message "%s" (get-all-paths-by-language snippy/snippets-paths "markdown"))
;; => ("./snippets/global.json")
