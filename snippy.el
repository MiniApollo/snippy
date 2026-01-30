;;; snippy.el --- Vscode snippets support for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Mark Surmann
;; Keywords: snippet
;; Package-Requires: ((emacs "25.1") )
;; Version: 0.1.0

;;; Commentary:

;; Vscode snippets support for Emacs

;;; Code:

;; TODO
;; Multi language support
;; Auto download of friendly-snippets or changed repo with var

;; Not very interesting
;; Variable-Transform
;; Placeholder-Transform
;; Grammar

(require 'seq)
(require 'yasnippet)

(defgroup snippy nil
  "Custom snippet management utilities."
  :group 'editing)

(defcustom snippy-snippet-dir (expand-file-name "./friendly-snippets/")
  "Directory containing the snippets and package.json."
  :type 'directory
  :group 'snippy)

(defconst snippy--min-vscode-version "1.11.0"
  "The minimum VSCode engine version required.")

(defvar snippy-package-json-content nil
  "Package json file content")

(defun snippy-get-package-data ()
  "Read and parse the package.json file."
  (let ((file (expand-file-name "package.json" snippy-snippet-dir)))
    (if (file-exists-p file)
        (setq snippy-package-json-content (json-read-file file))
      (error "Could not find package.json in %s" snippy-snippet-dir))))

;; Vscode engine check
(defun snippy--clean-version (version)
  "Extract a clean semver string from VERSION, removing ^ or ~."
  (when (stringp version)
    (if (string-match "\\([0-9.]+\\)" version)
        (match-string 1 version)
      version)))

(defun snippy-check-engine-version()
  (let* ((current-engine-version
          (snippy--clean-version (alist-get 'vscode (alist-get 'engines snippy-package-json-content)))))
    (cond
     ((null current-engine-version)
      (message "Snippy: Could not determine VSCode version from package.json."))
     ((version<= snippy--min-vscode-version current-engine-version)
      (message "Snippy: Version check passed %s" current-engine-version))
     (t
      (warn "Snippy: VSCode version %s is below requirement %s"
            current-engine-version snippy--min-vscode-version)))
    ))

;; Read in by language
;; Snippets
(defvar snippy--emacs-to-vscode-lang-alist
  '((text-mode . "plaintext")
    (markdown-mode . "markdown")
    (gfm-mode . "markdown")
    (latex-mode . "tex")
    (tex-mode . "tex")
    (html-mode . "html")
    (mhtml-mode . "html")
    (c-mode . "c")
    (c-ts-mode . "c")
    (c++-mode . "cpp")
    (cpp-mode . "cpp")
    (c++-ts-mode . "cpp")
    (csharp-mode . "csharp")
    (csharp-ts-mode . "csharp")
    (editorconfig-mode . "editorconfig")
    (git-commit-mode . "gitcommit")
    (ejs-mode . "ejs")
    (eruby-mode . "eruby")
    (erlang-mode . "erlang")
    (elixir-mode . "elixir")
    (elixir-ts-mode . "elixir")
    (f90-mode . "fortran")
    (fortran-mode . "fortran")
    (glsl-mode . "glsl")
    (nix-mode . "nix")
    (lua-mode . "lua")
    (go-mode . "go")
    (go-ts-mode . "go")
    (fennel-mode . "fennel")
    (php-mode . "php")
    (quarto-mode . "quarto")
    (rescript-mode . "rescript")
    (ruby-mode . "ruby")
    (ruby-ts-mode . "ruby")
    (rspec-mode . "rspec")
    (rust-mode . "rust")
    (rust-ts-mode . "rust")
    (haskell-mode . "haskell")
    (scala-mode . "scala")
    (solidity-mode . "solidity")
    (swift-mode . "swift")
    (sql-mode . "sql")
    (verilog-mode . "systemverilog")
    (sh-mode . "shellscript")
    (bash-ts-mode . "shellscript")
    (plantuml-mode . "plantuml")
    (java-mode . "java")
    (java-ts-mode . "java")
    (julia-mode . "julia")
    (css-mode . "css")
    (css-ts-mode . "css")
    (scss-mode . "scss")
    (js-mode . "javascript")
    (js2-mode . "javascript")
    (js-ts-mode . "javascript")
    (js-json-mode . "javascript")
    (json-ts-mode . "javascript")
    (rjsx-mode . "javascriptreact")
    (typescript-mode . "typescript")
    (typescript-ts-mode . "typescript")
    (tsx-ts-mode . "typescriptreact")
    (terraform-mode . "terraform")
    (svelte-mode . "svelte")
    (vue-mode . "vue")
    (python-mode . "python")
    (python-ts-mode . "python")
    (cobol-mode . "cobol")
    (kotlin-mode . "kotlin")
    (bibtex-mode . "bibtex")
    (web-mode . "twig")
    (r-mode . "r")
    (ess-r-mode . "r")
    (org-mode . "org")
    (gdscript-mode . "gdscript")
    (yaml-mode . "yaml")
    (makefile-mode . "make")
    (dart-mode . "dart")
    (objc-mode . "objc")
    (tcl-mode . "tcl")
    (perl-mode . "perl")
    (vhdl-mode . "vhdl")
    (dockerfile-mode . "dockerfile")
    (dockerfile-ts-mode . "dockerfile")
    (powershell-mode . "powershell")
    (reason-mode . "reason")
    (ocaml-mode . "ocaml")
    (purescript-mode . "purescript")
    (gleam-mode . "gleam")
    (asciidoc-mode . "asciidoc")
    (beancount-mode . "beancount")
    (rst-mode . "rst")
    (cmake-mode . "cmake")
    (zig-mode . "zig"))
  "Alist mapping Emacs major modes to VS Code language identifiers.")

(defvar-local snippy--buffer-language nil
  "The language currently used by snippy in the local buffer.")

(defun snippy--get-vscode-language-name (&optional mode)
  "Return the VS Code language string for MODE (defaults to current `major-mode`)."
  (let* ((target-mode (or mode major-mode))
         (match (assoc target-mode snippy--emacs-to-vscode-lang-alist)))
    (if match
        (cdr match)
      (message "No VS Code mapping found for %s" target-mode)
      nil)))

(defun snippy--update-buffer-language ()
  "Update `snippy--buffer-language` based on the current major mode."
  (setq snippy--buffer-language (snippy--get-vscode-language-name))
  (snippy-refresh-snippets))

(defun snippy--get-all-snippets-paths ()
  "Returns the snippets paths in package.json file for all languages"
  (alist-get 'snippets (alist-get 'contributes snippy-package-json-content)))

;;  (snippy-get-package-data)
;;  (snippy-check-engine-version)
;; (message "%s" (snippy--get-all-snippets-paths))

;;;###autoload
(define-minor-mode snippy-minor-mode
  "Minor mode for managing snippets via package.json."
  :group 'snippy
  (if snippy-minor-mode
      ;; Logic when the mode is TURNED ON
      (condition-case err
          (progn
            (snippy-get-package-data)
            (snippy-check-engine-version)
            (snippy--update-buffer-language)
            ;; Register the CAPF locally
            (add-hook 'completion-at-point-functions #'snippy-capf nil t)
            (message "Snippy minor mode enabled in current buffer"))
        (error
         (setq snippy-mode nil)
         (error "Failed to enable Snippy mode: %s" (error-message-string err))))
    ;; Logic when the mode is TURNED OFF
    (setq snippy-package-json-content nil
          snippy--buffer-language nil
          snippy--merged-snippets nil)
    (remove-hook 'completion-at-point-functions #'snippy-capf t)
    (message "Snippy minor mode disabled in current buffer")))

;;;###autoload
(define-globalized-minor-mode global-snippy-minor-mode
  snippy-minor-mode
  snippy--turn-on
  :group 'snippy)

(defun snippy--turn-on ()
  "Enable `snippy-minor-mode` in appropriate buffers."
  (when (and (not (minibufferp))
             ;; Ensure there is an actual file or a major mode set
             (not (derived-mode-p 'special-mode))
             (not (derived-mode-p 'tags-table-mode))
             ;; Optional: Only enable if a package.json exists in the project
             ;; (locate-dominating-file default-directory "package.json")
             )
    (snippy-minor-mode 1)))

;; Get language paths
;; AI slop warning
;; But it works
(defun snippy--get-all-paths-for-language (my-snippet-data target-lang)
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
;; (message "Result for C: %s" (snippy--get-all-paths-for-language snippy--get-all-snippets-paths "cpp"))
;; (message "Result for Markdown: %s" (snippy--get-all-paths-for-language snippy--get-all-snippets-paths "rust"))

(defun snippy--get-current-language-path ()
  "Returns the snippet language path for the current language in the buffer"
  (snippy--get-all-paths-for-language (snippy--get-all-snippets-paths) snippy--buffer-language))
;(message "%s" (snippy--get-current-language-path))

;; Read in snippets
(defvar-local snippy--merged-snippets nil
  "A merged alist of all snippets found in the paths defined by snippy--get-current-language-path.")

(defun snippy-refresh-snippets ()
  "Force an update on the snippets for the current buffer."
  (interactive)
  (setq snippy--merged-snippets
        (mapcan (lambda (suffix)
                  (let ((full-path (concat snippy-snippet-dir suffix)))
                    (if (file-exists-p full-path)
                        (json-read-file full-path)
                      (message "Skipping: %s (not found)" full-path)
                      nil))) ; Ensure we return nil for mapcan if file missing
                (snippy--get-current-language-path))))
;; (message "%s" snippy--merged-snippets)

;; Search for snippet
(defun snippy--find-snippet-by-prefix (prefix snippets)
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
(defun snippy-expand (prefix)
  "Expand snippet by prefix"
  (interactive "sEnter snippet name: ")
  (unless (featurep 'yasnippet)
    (user-error "Yasnippet is not loaded. Please install or require it first"))
  (unless (bound-and-true-p yas-minor-mode)
    (yas-minor-mode t))
  ;; (message "%s" (snippy--find-snippet-by-prefix prefix snippy--merged-snippets))
  (snippy-expand-snippet (snippy--find-snippet-by-prefix prefix snippy--merged-snippets)))

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

(defun snippy-expand-snippet (snippet)
  "Convert LSP-style choices to YASnippet elisp and expand, handling strings or vectors."
  (let* ((body-raw (cdr (assoc 'body snippet)))
         ;; If it's a vector, join it. If it's already a string, use it.
         (body-str (cond ((vectorp body-raw) (mapconcat #'identity body-raw "\n"))
                         ((stringp body-raw) body-raw)
                         (t "")))
         ;; Remove duplicate placeholders.
         ;; Yasnippet don't likes duplicates
         (body-no-dups
          (let ((seen-ids '()))
            (replace-regexp-in-string
             "\\${\\([0-9]+\\):[^}]+}"
             (lambda (match)
               (let ((id (match-string 1 match)))
                 (if (member id seen-ids)
                     (concat "$" id)     ; It's a duplicate, return just $N
                   (push id seen-ids)    ; First time seeing this ID
                   match)))              ; Keep the original ${N:default}
             body-str t t)))         ;; Choices
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
           body-no-dups t t))
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

;; CAPF
;; From yasnippet-capf
(defun snippy--doc-buffer (cand)
  "Generate a documentation buffer for snippet CAND."
  (when-let* ((snippet (get-text-property 0 'snippy-snippet cand))
              (body-raw (cdr (assoc 'body snippet)))
              (mode major-mode))
    (with-current-buffer (get-buffer-create "*snippy-doc*")
      (erase-buffer)
      (let ((body-str (cond ((vectorp body-raw) (mapconcat #'identity body-raw "\n"))
                            ((stringp body-raw) body-raw)
                            (t ""))))
        (insert "Expands to:\n\n" body-str)
        ;; Simple font-lock based on the current buffer's mode
        (delay-mode-hooks (funcall mode))
        (ignore-errors (font-lock-ensure))
        (current-buffer)))))

(defvar snippy-capf-properties
  (list :annotation-function (lambda (cand)
                               (let ((desc (get-text-property 0 'snippy-desc cand)))
                                 (if desc (format " -- %s" desc) " (Snippet)")))
        :company-kind (lambda (_) 'snippet)
        :company-doc-buffer #'snippy--doc-buffer
        :exit-function (lambda (cand _status)
                         (let ((beg (- (point) (length cand)))
                               (end (point)))
                           (delete-region beg end)
                           (snippy-expand cand)))
        :exclusive 'no)
  "Completion extra properties for Snippy.")

(defun snippy-capf-candidates (prefix)
  "Return a list of candidates from Snippy propertized with metadata."
  (let (candidates)
    (pcase-dolist (`(,_name . ,data) snippy--merged-snippets)
      (let ((p (cdr (assoc 'prefix data)))
            (desc (cdr (assoc 'description data))))
        (dolist (key (if (listp p) p (if (vectorp p) (append p nil) (list p))))
          (when (string-prefix-p prefix key t)
            (push (propertize key
                             'snippy-desc desc
                             'snippy-snippet data)
                  candidates)))))
    (delete-dups candidates)))

;;;###autoload
(defun snippy-capf (&optional interactive)
  "Complete with snippy at point.
If INTERACTIVE is non-nil, trigger completion immediately.
Works even with an empty prefix/string."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions '(snippy-capf)))
        (or (completion-at-point) (user-error "No snippy completions at point")))
    (when snippy-minor-mode
      (let* ((bnd (bounds-of-thing-at-point 'symbol))
             ;; If no symbol at point, use the current position for both start and end
             (start (or (car bnd) (point)))
             (end (or (cdr bnd) (point))))
        `(,start ,end
          ,(completion-table-with-cache
            (lambda (input) (snippy-capf-candidates input)))
          ,@snippy-capf-properties)))))
(provide 'snippy)
;;; snippy.el ends here
