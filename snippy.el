;;; snippy.el --- Vscode snippets support for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Mark Surmann

;; Author: Mark Surmann <overmilord62@gmail.com>
;; Created: 28 Jan 2026

;; Keywords: convenience, emulation
;; Package-Requires: ((emacs "30.1") (yasnippet "0.14.0"))
;; Version: 1.1.2
;; URL: https://github.com/MiniApollo/snippy

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; VSCode snippets support for Emacs with Yasnippet

;;; Code:

;; TODO
;; Add to docs how to modify this list to add custom languages
;; Fix yasnippet jumping. Use the fixer function in yasnippets snippets.
;; Do some rewrites.

;;; ============================================================================
;;; Dependencies
;;; ============================================================================

(require 'cl-lib)
(require 'seq)
(require 'yasnippet)
(require 'json)
(require 'project)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup snippy nil
  "Custom snippet management utilities."
  :group 'editing)

(defcustom snippy-install-dir (expand-file-name user-emacs-directory)
  "Directory where to install/clone snippets"
  :type 'directory
  :group 'snippy)

(defcustom snippy-source '("https://github.com/rafamadriz/friendly-snippets.git" . "friendly-snippets")
  "The source details for downloading snippets."
  :type '(cons (string :tag "Repository URL")
               (string :tag "Directory Name"))
  :group 'snippy)

(defcustom snippy-global-languages nil
  "List of languages to enable globally across all major modes."
  :type '(repeat string)
  :group 'snippy)

;;; ============================================================================
;;; Internal Variables
;;; ============================================================================

(defconst snippy--min-vscode-version "1.11.0"
  "The minimum VSCode engine version required.")

(defvar snippy-package-json-content nil
  "Package json file content parsed as an alist.")

(defvar-local snippy--buffer-language nil
  "The language currently used by snippy in the local buffer.")

(defvar-local snippy--merged-snippets nil
  "A merged alist of all snippets found in the snippets files.")

;;; ============================================================================
;;; Setup & Version Checking
;;; ============================================================================

(defun snippy--get-snippet-dir ()
  "Return snippet directory"
  (expand-file-name (cdr snippy-source) snippy-install-dir))

(defun snippy-install-or-update-snippets ()
  "Install or update snippet git repo in snippy-install-dir.
  If snippy-install-dir is nil, it defaults to `user-emacs-directory`."
  (interactive)
  (let* ((base (or snippy-install-dir user-emacs-directory))
         (dest (expand-file-name (cdr snippy-source) base)))
    (if (file-directory-p dest)
        (progn
          (message "Pulling updates in %s..." dest)
          (start-process "Snippy-git-pull" nil "git" "pull"))
      (message "Cloning %s to %s..." (car snippy-source) dest)
      (start-process "Snippy-git-clone" nil "git" "clone" (car snippy-source) dest))))

(defun snippy-get-package-data ()
  "Read and parse the package.json file.
  Used for getting the snippet paths to read and the VScode engine version."
  (let ((file (expand-file-name "package.json" (snippy--get-snippet-dir))))
    (if (file-exists-p file)
        (setq snippy-package-json-content (json-read-file file))
      (user-error "Snippy: package.json not found. Run `M-x snippy-install-or-update-snippets' first"))))

;;; ============================================================================
;;; Vscode engine check
;;; ============================================================================

(defun snippy--clean-version (version)
  "Extract a clean semver string from VERSION, removing ^ or ~."
  (when (stringp version)
    (if (string-match "\\([0-9.]+\\)" version)
        (match-string 1 version)
      version)))

(defun snippy-check-engine-version ()
  "Check if the current VSCode engine version meets the minimum requirement."
  (let ((current-engine-version
         (snippy--clean-version (alist-get 'vscode (alist-get 'engines snippy-package-json-content)))))
    (cond
     ((null current-engine-version)
      (lwarn 'snippy :warning
             "Snippy: Could not determine VSCode version from package.json."))
     ((version< current-engine-version snippy--min-vscode-version)
      (lwarn 'snippy :warning
             "VSCode version %s is below requirement %s. Some features may not work."
             current-engine-version snippy--min-vscode-version)))))

;;; ============================================================================
;;; Language Remap
;;; ============================================================================

(defvar snippy-emacs-to-vscode-lang-alist
  '((text-mode . "plaintext")
    (markdown-mode . "markdown")
    (markdown-ts-mode . "markdown")
    (gfm-mode . "markdown")
    (latex-mode . "tex")
    (tex-mode . "tex")
    (html-mode . "html")
    (html-ts-mode . "html")
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
    (lua-ts-mode . "lua")
    (go-mode . "go")
    (go-ts-mode . "go")
    (fennel-mode . "fennel")
    (php-mode . "php")
    (php-ts-mode . "php")
    (quarto-mode . "quarto")
    (rescript-mode . "rescript")
    (ruby-mode . "ruby")
    (ruby-ts-mode . "ruby")
    (rspec-mode . "rspec")
    (rust-mode . "rust")
    (rust-ts-mode . "rust")
    (haskell-mode . "haskell")
    (haskell-ts-mode . "haskell")
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
    (astro-mode . "astro")
    (astro-ts-mode . "astro")
    (clojure-mode . "clojure")
    (clojure-ts-mode . "clojure")
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
    (vue-ts-mode . "vue")
    (python-mode . "python")
    (python-ts-mode . "python")
    (cobol-mode . "cobol")
    (kotlin-mode . "kotlin")
    (kotlin-ts-mode . "kotlin")
    (bibtex-mode . "bibtex")
    (web-mode . "twig")
    (r-mode . "r")
    (ess-r-mode . "r")
    (org-mode . "org")
    (gdscript-mode . "gdscript")
    (gdscript-ts-mode . "gdscript")
    (yaml-mode . "yaml")
    (yaml-ts-mode . "yaml")
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
    (cmake-ts-mode . "cmake")
    (zig-mode . "zig")
    (zig-ts-mode . "zig"))
  "Alist mapping Emacs major modes to VS Code language identifiers.")

(defun snippy--get-vscode-language-name (&optional mode)
  "Return the VS Code language string for MODE (defaults to current `major-mode`)."
  (let* ((target-mode (or mode major-mode))
         (match (assoc target-mode snippy-emacs-to-vscode-lang-alist)))
    (if match
        (cdr match)
      nil)))

(defun snippy--update-buffer-language ()
  "Update `snippy--buffer-language` based on the current major mode."
  (setq snippy--buffer-language
        (cons (snippy--get-vscode-language-name) snippy-global-languages)))

;;; ============================================================================
;;; Snippet Reading & Parsing
;;; ============================================================================

(defun snippy--get-all-paths-for-language (my-snippet-data target-lang)
  "Return a list of all paths associated with TARGET-LANG."
  (when target-lang
    (let ((target (if (symbolp target-lang) (symbol-name target-lang) target-lang)))
      (cl-loop for entry across (or my-snippet-data [])
               for val = (cdr (assoc 'language entry))
               when (if (vectorp val)
                        (cl-some (lambda (x) (string-equal (format "%s" x) target)) val)
                      (string-equal (format "%s" val) target))
               collect (cdr (assoc 'path entry))))))

(defun snippy--get-all-snippets-paths ()
  "Returns the snippets paths in package.json file for all languages"
  (alist-get 'snippets (alist-get 'contributes snippy-package-json-content)))

(defun snippy--get-current-language-path ()
  "Returns a combined list of snippet paths for all languages."
  (cl-loop with all-dirs = (snippy--get-all-snippets-paths)
           for lang in (ensure-list snippy--buffer-language)
           append (snippy--get-all-paths-for-language all-dirs lang)))

(defun snippy-refresh-snippets ()
  "Force an update on the snippets for the current buffer."
  (interactive)
  (snippy--update-buffer-language)
  (setq snippy--merged-snippets
        (cl-loop with dir = (snippy--get-snippet-dir)
                 for suffix in (snippy--get-current-language-path)
                 for full-path = (expand-file-name suffix dir)
                 if (file-exists-p full-path)
                 append (json-read-file full-path)
                 else
                 do (message "Skipping: %s (not found)" full-path))))

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

;;; ============================================================================
;;; Snippet Expansion
;;; ============================================================================

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
  "Convert VSCode snippets to Yasnippet and expand it"
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

(defun snippy-expand (prefix)
  "Expand snippet by prefix"
  (interactive "sEnter snippet prefix: ")
  (unless (featurep 'yasnippet)
    (user-error "Yasnippet is not loaded. Please install or require it first"))
  (unless (bound-and-true-p yas-minor-mode)
    (yas-minor-mode t))
  ;; (message "%s" (snippy--find-snippet-by-prefix prefix snippy--merged-snippets))
  (snippy-expand-snippet (snippy--find-snippet-by-prefix prefix snippy--merged-snippets)))

;;; ============================================================================
;;; Completion At Point (CAPF)
;;; ============================================================================

(defun snippy--doc-buffer (cand)
  "Generate a documentation buffer for snippet CAND."
  (when-let* ((snippet (get-text-property 0 'snippy-snippet cand))
              (body-raw (cdr (assoc 'body snippet)))
              (mode major-mode))
    (with-current-buffer (get-buffer-create "*snippy-doc*")
      (let ((inhibit-read-only t)
            (desc (get-text-property 0 'snippy-desc cand)))
        (erase-buffer)
        (let ((body-str (cond ((vectorp body-raw) (mapconcat #'identity body-raw "\n"))
                              ((stringp body-raw) body-raw)
                              (t ""))))
          (insert "Expands to:\n\n" body-str)
          (when (and desc (not (string-empty-p desc)))
            (insert "\n" (make-string 20 ?-) "\n" desc))

          (delay-mode-hooks (funcall mode))
          (ignore-errors (font-lock-ensure))
          (setq-local cursor-type nil)
          (read-only-mode 1)
          (current-buffer))))))

(defvar snippy-capf-properties
  (list :annotation-function (lambda (cand)
                               (let ((name (get-text-property 0 'snippy-name cand)))
                                 (if name
                                     (format "  %s" name)
                                   "  Snippet")))
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
    (pcase-dolist (`(,name . ,data) snippy--merged-snippets)
      (let ((p (cdr (assoc 'prefix data)))
            (desc (cdr (assoc 'description data))))
        (dolist (key (if (listp p) p (if (vectorp p) (append p nil) (list p))))
          (when (string-prefix-p prefix key t)
            (push (propertize key
                              'snippy-name (format "%s" name)
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

;;; ============================================================================
;;; Minor Modes
;;; ============================================================================

;;;###autoload
(define-minor-mode snippy-minor-mode
  "Toggle snippy in the current buffer"
  :group 'snippy
  (if snippy-minor-mode
      ;; Logic when the mode is TURNED ON
      (condition-case err
          (progn
            (snippy-get-package-data)
            (snippy-refresh-snippets)
            (when (called-interactively-p 'any)
              (progn (snippy-check-engine-version)
                     (message "Snippy minor mode enabled in current buffer"))))
        (error
         (setq snippy-minor-mode nil)
         (error "Failed to enable Snippy mode: %s" (error-message-string err))))
    ;; Logic when the mode is TURNED OFF
    (setq snippy--buffer-language nil
          snippy--merged-snippets nil)
    (when (called-interactively-p 'any)
      (message "Snippy minor mode disabled in current buffer"))))


;;;###autoload
(define-globalized-minor-mode global-snippy-minor-mode
  snippy-minor-mode
  snippy--turn-on
  :group 'snippy
  (if global-snippy-minor-mode
      (progn (snippy-check-engine-version)
             (message "Global Snippy mode enabled"))
    (message "Global Snippy mode disabled")))

(defun snippy--turn-on ()
  "Enable `snippy-minor-mode` only in file-visiting programming or text buffers."
  (when (and (not (minibufferp))
             ;; Only enable in "real" content buffers (Prog or Text)
             (derived-mode-p 'prog-mode 'text-mode)
             ;; Prevent infinite loops or redundant checks
             (not snippy-minor-mode))
    (snippy-minor-mode t)))

(provide 'snippy)
;;; snippy.el ends here
