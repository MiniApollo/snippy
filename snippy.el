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

(setq-local snippy/current-language "c")
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
  (interactive "sEnter snippet name: ")
  (my/expand-snippet-at-point (snippy/find-snippet-by-prefix prefix snippy/merged-snippets))
  (message "%s" (snippy/find-snippet-by-prefix prefix snippy/merged-snippets))
)

;; SOMEWHAT WORKING SLOP
(defvar-local my/snippet-overlays nil)
(defvar-local my/snippet-index 0)

(defun my/sync-tabstops (ov after beg end &optional _len)
  "Synchronize all overlays sharing the same tabstop ID with OV."
  ;; Only run after the change and if not undoing
  (when (and after (not undo-in-progress))
    (let* ((id (overlay-get ov 'tabstop))
           (inhibit-modification-hooks t)
           (text (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
      (save-excursion
        (save-match-data
          (dolist (mirror my/snippet-overlays)
            (let ((m-beg (overlay-start mirror))
                  (m-end (overlay-end mirror)))
              ;; Sync if IDs match and it's not the overlay currently being edited
              (when (and (eq (overlay-get mirror 'tabstop) id)
                         (not (eq mirror ov)))
                (goto-char m-beg)
                (delete-region m-beg m-end)
                (insert text)
                ;; Update mirror bounds to match new text length
                (move-overlay mirror m-beg (point))))))))))

(defun my/expand-snippet-at-point (snippet-alist)
  "Expands snippet and links mirrors."
  (interactive)
  (let* ((body-data (cdr (assoc 'body snippet-alist)))
         (body-str (if (vectorp body-data) (mapconcat #'identity body-data "\n") body-data))
         (all-ovs nil))

    (mapc #'delete-overlay my/snippet-overlays)

    ;; 1. Handle Choices (Interactively and globally for the string)
    (while (string-match "\\${\\([0-9]+\\)|\\([^|]+\\)|}" body-str)
      (let* ((id (match-string 1 body-str))
             (options (split-string (match-string 2 body-str) ","))
             (choice (completing-read (format "Choice for $%s: " id) options)))
        ;; Update EVERY instance of $id, ${id:default}, or ${id|choices|} in the string
        (setq body-str (replace-regexp-in-string
                        (format "\\$\\(%s\\)\\|\\${\\(%s\\)[:|][^}]+}" id id)
                        choice body-str))))
    ;; 2. Insertion
    (let ((start (point))
          (all-ovs nil))
      (insert body-str)
      (save-excursion
        (goto-char start)
        ;; Limit the search to the end of the inserted string
        (let ((limit (point-max))) ; Or use (+ start (length body-str)) if text after could match
          (while (re-search-forward "\\$\\([0-9]+\\)\\|\\${\\([0-9]+\\):\\([^}]+\\)}" limit t)
            (let* ((num (string-to-number (or (match-string 1) (match-string 2))))
                   (val (or (match-string 3) ""))
                   (beg (match-beginning 0)))
              ;; 1. Replace the placeholder with the default value first
              (replace-match val t t)
              ;; 2. Create the overlay on the *new* text bounds
              (let ((ov (make-overlay beg (point))))
                (overlay-put ov 'tabstop num)
                (overlay-put ov 'modification-hooks '(my/sync-tabstops))
                (overlay-put ov 'face '(:background "#333" :underline t))
                (push ov all-ovs))))))

      ;; Update state and initialize
      (setq my/snippet-overlays (nreverse all-ovs)
            my/snippet-index 0)
      (my/snippet-jump 0))))

(defun my/snippet-jump (n)
  "Jump to the Nth tabstop and select its placeholder text."
  (interactive "p")
  (let* ((tabstops (mapcar (lambda (ov) (overlay-get ov 'tabstop)) my/snippet-overlays))
         ;; Sort: 1, 2, 3... and 0 always goes last
         (unique-sorted (sort (delete-dups tabstops)
                              (lambda (a b)
                                (if (zerop a) nil (if (zerop b) t (< a b))))))
         (target-id (nth n unique-sorted))
         (target-ov (seq-find (lambda (ov) (eq (overlay-get ov 'tabstop) target-id))
                              my/snippet-overlays)))

    (cond
     ;; Case 1: No more tabstops - Cleanup
     ((or (not target-id) (< n 0))
      (mapc #'delete-overlay my/snippet-overlays)
      (setq my/snippet-overlays nil
            my/snippet-index 0)
      (message "Snippet finished."))

     ;; Case 2: Jump and Select
     (target-ov
      (setq my/snippet-index n)
      (goto-char (overlay-start target-ov))
      (set-mark (overlay-end target-ov))
      (activate-mark)
      (message "Editing tabstop: %d" target-id)))))

(defun my/snippet-next () (interactive) (setq my/snippet-index (1+ my/snippet-index)) (my/snippet-jump my/snippet-index))
(defun my/snippet-prev () (interactive) (setq my/snippet-index (1- my/snippet-index)) (my/snippet-jump my/snippet-index))
