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

;;; ============================================================================
;;; Dependencies
;;; ============================================================================

(require 'cl-lib)
(require 'json)
(require 'yasnippet)
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

;; TODO:
;; Remove unreal engine snippets from default load
;; Users should add them manually
;; Rethink global and all languages position in fundemental mode
;; Add to docs how to modify this list to add custom languages

(defvar snippy-emacs-to-vscode-lang-alist
  '((text-mode . "plaintext")
    (markdown-mode . "markdown")
    (markdown-ts-mode . "markdown")
    (gfm-mode . "markdown")
    (latex-mode . "tex")
    (tex-mode . "tex")
    (plain-tex-mode . "plaintex")
    (html-mode . "html")
    (html-ts-mode . "html")
    (mhtml-mode . "html")
    (html-mode . "djangohtml")
    (html-mode . "htmldjango")
    (fundamental-mode . "all")
    (fundamental-mode . "global")
    (web-mode . "angular")
    (web-mode . "twig")
    (bibtex-mode . "bib")
    (bibtex-mode . "bibtex")
    (blade-mode . "blade")

    ;; C / C++
    (c-mode . "c")
    (c-mode . "cdoc")
    (c-ts-mode . "c")
    (c-ts-mode . "cdoc")
    (c++-mode . "cpp")
    (c++-mode . "cppdoc")
    (c++-mode . "unreal")
    (cpp-mode . "cpp")
    (c++-ts-mode . "cpp")
    (c++-ts-mode . "cppdoc")
    ;; (c++-ts-mode . "unreal")

    ;; C#
    (csharp-mode . "csharp")
    (csharp-mode . "cs")
    (csharp-mode . "csharpdoc")
    (csharp-mode . "unity")
    (csharp-ts-mode . "csharp")
    (csharp-ts-mode . "cs")
    (csharp-ts-mode . "csharpdoc")
    ;; (csharp-ts-mode . "unity")

    (editorconfig-mode . "editorconfig")
    (git-commit-mode . "gitcommit")
    (git-commit-mode . "NeogitCommitMessage")
    (ejs-mode . "ejs")
    (eruby-mode . "eruby")
    (erlang-mode . "erlang")

    ;; Elixir
    (elixir-mode . "elixir")
    (elixir-ts-mode . "elixir")
    (eelixir-mode . "eelixir")
    (heex-mode . "heex")

    (f90-mode . "fortran")
    (fortran-mode . "fortran")
    (glsl-mode . "glsl")
    (nix-mode . "nix")

    ;; Lua
    (lua-mode . "lua")
    (lua-mode . "luadoc")
    (lua-ts-mode . "lua")
    (lua-ts-mode . "luadoc")

    (go-mode . "go")
    (go-ts-mode . "go")
    (fennel-mode . "fennel")

    ;; PHP
    (php-mode . "php")
    (php-mode . "phpdoc")
    (php-ts-mode . "php")
    (php-ts-mode . "phpdoc")

    (quarto-mode . "quarto")
    (rescript-mode . "rescript")

    ;; Ruby
    (ruby-mode . "ruby")
    (ruby-mode . "rails")
    (ruby-mode . "rdoc")
    (ruby-ts-mode . "ruby")
    (ruby-ts-mode . "rails")
    (ruby-ts-mode . "rdoc")
    (rspec-mode . "rspec")

    ;; Rust
    (rust-mode . "rust")
    (rust-mode . "rustdoc")
    (rust-ts-mode . "rust")
    (rust-ts-mode . "rustdoc")

    (haskell-mode . "haskell")
    (haskell-ts-mode . "haskell")
    (scala-mode . "scala")
    (solidity-mode . "solidity")
    (swift-mode . "swift")
    (sql-mode . "sql")
    (verilog-mode . "systemverilog")
    (systemverilog-mode . "verilog")

    ;; Shell / Bash
    (sh-mode . "shellscript")
    (sh-mode . "sh")
    (sh-mode . "shell")
    (sh-mode . "shelldoc")
    (sh-mode . "zsh")
    (bash-ts-mode . "shellscript")
    (bash-ts-mode . "sh")
    (bash-ts-mode . "shell")
    (bash-ts-mode . "shelldoc")
    (bash-ts-mode . "zsh")

    (plantuml-mode . "plantuml")

    ;; Java
    (java-mode . "java")
    (java-mode . "java-testing")
    (java-mode . "javadoc")
    (java-ts-mode . "java")
    (java-ts-mode . "java-testing")
    (java-ts-mode . "javadoc")

    (julia-mode . "julia")
    (astro-mode . "astro")
    (astro-ts-mode . "astro")
    (clojure-mode . "clojure")
    (clojure-ts-mode . "clojure")

    ;; CSS / Stylesheets
    (css-mode . "css")
    (css-ts-mode . "css")
    (scss-mode . "scss")
    (sass-mode . "sass")
    (less-css-mode . "less")
    (stylus-mode . "stylus")

    ;; JavaScript / JSON
    (js-mode . "javascript")
    (js-mode . "jsdoc")
    (js2-mode . "javascript")
    (js-ts-mode . "javascript")
    (js-ts-mode . "jsdoc")
    (js-json-mode . "javascript")
    (json-ts-mode . "javascript")
    (rjsx-mode . "javascriptreact")

    ;; TypeScript
    (typescript-mode . "typescript")
    (typescript-mode . "jsdoc")
    (typescript-mode . "remix")
    (typescript-mode . "tsdoc")
    (typescript-ts-mode . "typescript")
    (typescript-ts-mode . "jsdoc")
    (typescript-ts-mode . "remix")
    (typescript-ts-mode . "tsdoc")
    (tsx-ts-mode . "typescriptreact")

    (terraform-mode . "terraform")
    (svelte-mode . "svelte")
    (vue-mode . "vue")
    (vue-ts-mode . "vue")

    ;; Python
    (python-mode . "python")
    (python-mode . "django")
    (python-mode . "django-rest")
    (python-mode . "pydoc")
    (python-ts-mode . "python")
    (python-ts-mode . "django")
    (python-ts-mode . "django-rest")
    (python-ts-mode . "pydoc")

    (cobol-mode . "cobol")

    ;; Kotlin
    (kotlin-mode . "kotlin")
    (kotlin-mode . "kdoc")
    (kotlin-ts-mode . "kotlin")
    (kotlin-ts-mode . "kdoc")

    (r-mode . "r")
    (ess-r-mode . "r")
    (rmarkdown-mode . "rmd")
    (org-mode . "org")
    (gdscript-mode . "gdscript")
    (gdscript-ts-mode . "gdscript")

    ;; YAML
    (yaml-mode . "yaml")
    (yaml-ts-mode . "yaml")

    (makefile-mode . "make")
    (dart-mode . "dart")
    (dart-mode . "flutter")
    (objc-mode . "objc")
    (tcl-mode . "tcl")
    (perl-mode . "perl")
    (vhdl-mode . "vhdl")

    ;; Dockerfile
    (dockerfile-mode . "dockerfile")
    (dockerfile-ts-mode . "dockerfile")

    (powershell-mode . "powershell")
    (powershell-mode . "ps1")
    (reason-mode . "reason")
    (ocaml-mode . "ocaml")
    (tuareg-mode . "ocaml.ocamllex")
    (tuareg-mode . "ocamlinterface")
    (purescript-mode . "purescript")
    (gleam-mode . "gleam")
    (asciidoc-mode . "asciidoc")
    (beancount-mode . "beancount")
    (rst-mode . "rst")

    ;; CMake
    (cmake-mode . "cmake")
    (cmake-ts-mode . "cmake")

    ;; Zig
    (zig-mode . "zig")
    (zig-ts-mode . "zig")

    (dune-mode . "dune")
    (dune-project-mode . "dune-project") ; Small fix for a potential typo in your source
    (edge-mode . "edge")
    (fsharp-mode . "fsh")
    (jade-mode . "jade")
    (pug-mode . "pug")
    (jekyll-mode . "jekyll")
    (kivy-mode . "kivy")
    (license-mode . "license")
    (liquid-mode . "liquid")
    (artbollocks-mode . "loremipsum")
    (mint-mode . "mint")
    (norg-mode . "norg")
    (nushell-mode . "nu"))
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
               for val = (alist-get 'language entry)
               for val-strings = (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x))
                                         (if (vectorp val) (append val nil) (list val)))
               when (member target val-strings)
               collect (alist-get 'path entry)))))

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
  (cl-find-if (lambda (snippet)
                (let ((target (alist-get 'prefix (cdr snippet))))
                  (cond
                   ((stringp target) (string= prefix target))
                   ((vectorp target) (cl-position prefix target :test #'string=))
                   ((listp target)   (member prefix target)))))
              snippets))

;;; ============================================================================
;;; Snippet Expansion
;;; ============================================================================

(defun snippy--get-variable-value (var-name)
  "Resolves VS Code variables to their Emacs string values."
  (let ((file-name (buffer-file-name)))
    (pcase var-name
      ("TM_SELECTED_TEXT" (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) ""))
      ("TM_CURRENT_LINE" (or (and (thing-at-point 'line t)
                                  (string-trim-right (thing-at-point 'line t) "\n"))
                             ""))
      ("TM_CURRENT_WORD" (or (thing-at-point 'word t) ""))
      ("TM_LINE_INDEX" (number-to-string (1- (line-number-at-pos))))
      ("TM_LINE_NUMBER" (number-to-string (line-number-at-pos)))
      ("TM_FILENAME" (if file-name (file-name-nondirectory file-name) "Untitled"))
      ("TM_FILENAME_BASE" (if file-name (file-name-base file-name) "Untitled"))
      ("TM_DIRECTORY" (if file-name (file-name-directory file-name) default-directory))
      ("TM_DIRECTORY_BASE" (file-name-nondirectory (directory-file-name (if file-name (file-name-directory file-name) default-directory))))
      ("TM_FILEPATH" (or file-name ""))
      ("RELATIVE_FILEPATH" (if file-name (file-relative-name file-name) ""))
      ("CLIPBOARD" (if kill-ring (current-kill 0) ""))
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

(defun snippy--transform-snippet-body (body-str)
  "Apply transformations to convert VSCode snippet syntax to Yasnippet."
  (let ((result body-str))
    ;; 1. Handle Variables ($VAR or ${VAR})
    (setq result (replace-regexp-in-string
                  "\\$\\([A-Z_]+\\)\\|\\${\\([A-Z_]+\\)}"
                  (lambda (match)
                    (let ((var-name (or (match-string 1 match)
                                        (match-string 2 match))))
                      (or (snippy--get-variable-value var-name) var-name)))
                  result t t))

    ;; 2. Handle Choices ${1|a,b|}
    (setq result (replace-regexp-in-string
                  "\\${\\([0-9]+\\)|\\([^|]+\\)|}"
                  (lambda (match)
                    (let* ((index (match-string 1 match))
                           (choices (split-string (match-string 2 match) ","))
                           (lisp-list (format "'(%s)" (mapconcat #'prin1-to-string choices " "))))
                      (format "${%s:$$(yas-choose-value %s)}" index lisp-list)))
                  result t t))

    ;; 3. Handle Placeholders and Deduplication
    (let ((seen-ids '()))
      (setq result (replace-regexp-in-string
                    "\\${\\([0-9]+\\):\\([^}]+\\)}"
                    (lambda (match)
                      (let* ((id (match-string 1 match))
                             (default (match-string 2 match)))
                        (if (member id seen-ids)
                            (concat "$" id)
                          (push id seen-ids)
                          (format "${%s:%s}" id default))))
                    result t t)))
    result))

(defun snippy-expand-snippet (snippet)
  "Convert VSCode snippets to Yasnippet and expand it."
  (let* ((body-raw (cdr (assoc 'body snippet)))
         (body-str (cond ((vectorp body-raw) (mapconcat #'identity body-raw "\n"))
                         ((stringp body-raw) body-raw)
                         (t ""))))
    (yas-expand-snippet (snippy--transform-snippet-body body-str))))

(defun snippy-expand (prefix)
  "Expand snippet by prefix"
  (interactive "sEnter snippet prefix: ")
  (unless (featurep 'yasnippet)
    (user-error "Yasnippet is not loaded. Please install or require it first"))
  (unless (bound-and-true-p yas-minor-mode)
    (yas-minor-mode t))
  (snippy-expand-snippet (snippy--find-snippet-by-prefix prefix snippy--merged-snippets)))

;;; ============================================================================
;;; Yasnippet Fix
;;; ============================================================================

(defun snippy--advise-yas-expand (orig-fun snippet &rest args)
  "Intercept and transform SNIPPET on-the-fly"
  (let ((transformed-body
         (cond
          ;; Case 1: Eglot/LSP sends a raw string snippet
          ((stringp snippet)
           (snippy--transform-snippet-body snippet))

          ;; Case 2: Standard yasnippet expands a template object
          ((and (vectorp snippet) (fboundp 'yas--template-content))
           (let ((content (yas--template-content snippet)))
             (if (stringp content)
                 (snippy--transform-snippet-body content)
               snippet)))

          ;; Fallback: If it's something unexpected, leave it alone
          (t snippet))))

    ;; Pass the freshly transformed string (or fallback object) to the original function
    (apply orig-fun transformed-body args)))

;; Apply the advice globally
(advice-add 'yas-expand-snippet :around #'snippy--advise-yas-expand)

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
  (let (candidates
        (case-fold-search (or completion-ignore-case case-fold-search)))
    (pcase-dolist (`(,name . ,data) snippy--merged-snippets)
      (let ((p (cdr (assoc 'prefix data)))
            (desc (cdr (assoc 'description data))))
        ;; Normalize keys safely to a list structure
        (dolist (key (cond ((listp p) p)
                           ((vectorp p) (append p nil))
                           (t (list p))))
          (when (string-prefix-p prefix key case-fold-search)
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
  "Toggle snippy in the current buffer."
  :group 'snippy
  (if snippy-minor-mode
      (condition-case err
          (progn
            (unless snippy-package-json-content
              (snippy-get-package-data))
            (snippy-refresh-snippets)
            (when (called-interactively-p 'any)
              (snippy-check-engine-version)
              (message "Snippy minor mode enabled")))
        (error
         (snippy-minor-mode -1)
         (message "Snippy: Failed to initialize: %s" (error-message-string err))))
    (setq snippy--buffer-language nil
          snippy--merged-snippets nil)
    (when (called-interactively-p 'any)
      (message "Snippy minor mode disabled"))))

(defun snippy--turn-on ()
  "Enable `snippy-minor-mode` only in file-visiting programming or text buffers."
  (when (and (not (minibufferp))
             ;; Only enable in "real" content buffers (Prog or Text)
             (derived-mode-p 'prog-mode 'text-mode)
             ;; Prevent infinite loops or redundant checks
             (not snippy-minor-mode))
    (snippy-minor-mode t)))

;;;###autoload
(define-globalized-minor-mode global-snippy-minor-mode
  snippy-minor-mode
  snippy--turn-on
  :group 'snippy)

(provide 'snippy)
;;; snippy.el ends here
