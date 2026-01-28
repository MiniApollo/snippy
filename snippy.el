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

;;(setq json-object-type 'alist) ;; To set type default is alist
(setq-local json-array-type 'list) ;; Default is vector e.g [Hello there my name is]

(defvar snippy/package-json-location "./friendly-snippets/package.json" "Location of the package.json file")
(defvar snippy/package-json-content (json-read-file snippy/package-json-location))

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

(snippy/check-engine-version)
