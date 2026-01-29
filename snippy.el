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
(require 'cl-lib)

;; 1. Variables and Faces
(defvar-local my/snippet-overlays nil)
(defvar-local my/snippet-current-index 0)

;; Face for inactive fields
(defface my/snippet-field-face
  '((t :background "#444444" :underline nil))
  "Face for snippet fields not currently being edited.")

;; Face for the currently active field
(defface my/snippet-active-face
  '((t :inherit region :underline t))
  "Face for the active snippet field.")

;; Keymap that is only active when a snippet is running
(defvar my/snippet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'my/snippet-next)
    (define-key map (kbd "<tab>") #'my/snippet-next)
    (define-key map (kbd "S-TAB") #'my/snippet-prev)
    (define-key map (kbd "<backtab>") #'my/snippet-prev)
    (define-key map (kbd "C-g") #'my/snippet-exit)
    map))

(define-minor-mode my/snippet-mode
  "Minor mode active during snippet expansion."
  :init-value nil
  :lighter " Snip"
  :keymap my/snippet-mode-map)

;; 2. Mirror Synchronization
(defun my/sync-tabstops (ov after beg end &optional _len)
  "Synchronize all overlays sharing the same tabstop ID with OV."
  (when (and after (not undo-in-progress))
    (let* ((id (overlay-get ov 'tabstop))
           (text (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
           (inhibit-modification-hooks t)) ;; Prevent infinite loops

      ;; Group changes so one 'undo' reverts mirrors and source together
      (atomic-change-group
        (save-excursion
          (dolist (mirror my/snippet-overlays)
            (when (and (eq (overlay-get mirror 'tabstop) id)
                       (not (eq mirror ov))) ;; Don't edit the source
              (let ((m-beg (overlay-start mirror))
                    (m-end (overlay-end mirror)))
                (goto-char m-beg)
                (delete-region m-beg m-end)
                (insert text)
                ;; Explicitly fix overlay bounds if insert moved them weirdly
                (move-overlay mirror m-beg (point))))))))))

;; 3. Expansion Logic
(defun my/expand-snippet-at-point (snippet-alist)
  "Expands snippet, links mirrors, and activates snippet mode."
  (interactive)
  ;; Ensure delete-selection-mode is on so typing replaces text
  (unless delete-selection-mode (delete-selection-mode 1))

  (let* ((body-data (cdr (assoc 'body snippet-alist)))
         (body-str (if (vectorp body-data) (mapconcat #'identity body-data "\n") body-data))
         (start (point))
         (all-ovs nil))

    ;; A. Handle Choices (Interactively resolve before insertion)
    (while (string-match "\\${\\([0-9]+\\)|\\([^|]+\\)|}" body-str)
      (let* ((id (match-string 1 body-str))
             (options (split-string (match-string 2 body-str) ","))
             (choice (completing-read (format "Choice for $%s: " id) options)))
        (setq body-str (replace-regexp-in-string
                        (format "\\$\\(%s\\)\\|\\${\\(%s\\)[:|][^}]+}" id id)
                        choice body-str))))

    ;; B. Insert text
    (insert body-str)

    ;; C. Parse Tabstops and Create Overlays
    (save-excursion
      (goto-char start)
      (let ((limit (point-max)))
        ;; Regex handles $1 and ${1:default}
        (while (re-search-forward "\\$\\([0-9]+\\)\\|\\${\\([0-9]+\\):\\([^}]*\\)}" limit t)
          (let* ((num (string-to-number (or (match-string 1) (match-string 2))))
                 (val (or (match-string 3) ""))
                 (match-beg (match-beginning 0))
                 (match-end (match-end 0)))

            ;; Replace placeholder with default value
            (replace-match val t t)

            ;; Create Overlay
            ;; IMPORTANT: (point) is now after the inserted 'val'.
            ;; Arg 4 (nil): Front-advance (false) - wait, actually...
            ;; Arg 5 (t): Rear-advance (false)? No.
            ;; We want (make-overlay BEG END BUFFER FRONT-ADVANCE REAR-ADVANCE)
            ;; We set FRONT-ADVANCE to t. If we insert at the end of the field, it stays inside.
            (let ((ov (make-overlay match-beg (point) nil nil t)))
              (overlay-put ov 'tabstop num)
              (overlay-put ov 'modification-hooks '(my/sync-tabstops))
              (overlay-put ov 'face 'my/snippet-field-face)
              ;; Priority ensures our face doesn't completely mask the region selection
              (overlay-put ov 'priority 0)
              (push ov all-ovs))))))

    ;; D. Initialize State
    (my/snippet-exit) ;; Clean up any previous snippets
    (setq my/snippet-overlays (nreverse all-ovs)
          my/snippet-current-index 0)
    (my/snippet-mode 1) ;; Enable the keymap
    (my/snippet-jump 0)))

;; 4. Navigation
(defun my/snippet-jump (n)
  "Jump to the Nth tabstop."
  (let* ((tabstops (mapcar (lambda (ov) (overlay-get ov 'tabstop)) my/snippet-overlays))
         ;; Sort unique IDs: 1, 2, 3... and 0 is always last
         (unique-ids (sort (delete-dups (copy-sequence tabstops))
                           (lambda (a b)
                             (if (zerop a) nil (if (zerop b) t (< a b))))))
         (target-id (nth n unique-ids)))

    (if (not target-id)
        (my/snippet-exit) ;; No more stops? Exit.

      ;; Update Index
      (setq my/snippet-current-index n)

      ;; Find the first overlay with this ID (primary field)
      (let ((target-ov (seq-find (lambda (ov) (eq (overlay-get ov 'tabstop) target-id))
                                 my/snippet-overlays)))
        (when target-ov
          ;; 1. Update faces: Make all inactive, then make target active
          (dolist (ov my/snippet-overlays)
            (overlay-put ov 'face 'my/snippet-field-face))
          (dolist (ov my/snippet-overlays)
            (when (eq (overlay-get ov 'tabstop) target-id)
              (overlay-put ov 'face 'my/snippet-active-face)))

          ;; 2. Move Point and Select
          (goto-char (overlay-start target-ov))
          (set-mark (overlay-end target-ov))
          (activate-mark)

          ;; Ensure the cursor is placed nicely if the field is empty
          (when (= (overlay-start target-ov) (overlay-end target-ov))
            (deactivate-mark)))))))

(defun my/snippet-next ()
  (interactive)
  (my/snippet-jump (1+ my/snippet-current-index)))

(defun my/snippet-prev ()
  (interactive)
  (my/snippet-jump (max 0 (1- my/snippet-current-index))))

(defun my/snippet-exit ()
  "Cleanup overlays and disable snippet mode."
  (interactive)
  (mapc #'delete-overlay my/snippet-overlays)
  (setq my/snippet-overlays nil
        my/snippet-current-index 0)
  (my/snippet-mode -1)
  (message "Snippet finished."))
