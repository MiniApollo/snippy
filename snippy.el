;;; snippy.el --- Vscode snippet support for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Mark Surmann
;; Keywords: snippet
;; Package-Requires: ((emacs "25.1") )
;; Version: 0.1.0

;;; Commentary:

;; Vscode snippet support for Emacs

;;; Code:

;; TODO:
;; Only set vars for this package not change it globally

(setq json-object-type 'alist) ;; To set type default is alist
(setq json-array-type 'list) ;; Default is vector e.g [Hello there my name is]

(defvar snippy/package-json-location "./friendly-snippets/package.json" "Location of the package.json file")
(setq snippy/package-json-content (json-read-file snippy/package-json-location))
