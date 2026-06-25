;;; snippy.el --- Vscode snippet support with Yasnippet  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Mark Surmann

;; Author: Mark Surmann <overmilord62@gmail.com>
;; Assisted-by: Gemini:3.5 Flash
;; Created: 28 Jan 2026

;; Keywords: convenience, emulations
;; Package-Requires: ((emacs "30.1") (yasnippet "0.14.0"))
;; Version: 2.0.0
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
;; TODO:
;; Fix special character snippets like !,@ etc.
;; Characters like !, @ do not work either with yasnippet. The default behaviour
;; is that they are not treated part of the snippet.
;; It can be fixed with the following code for yasnippet.
;; (setq yas-key-syntaxes '("w_" "w_." "^ "))
;; More experimentation needed for snippy.
;; The problem was with my config.
;; When cape-capf-super combines multiple completion functions, it uses the prefix boundaries of the first
;; Capf in the list that returns a match. So when merging backends cape be the first

;; Add these to readme:
;; Non existent modes
;; (license-mode "license")
;; (blade-mode "blade")
;; (eelixir-mode "eelixir")
;; (rmarkdown-mode "rmd")

(require 'cl-lib)
(require 'yasnippet)
(require 'project)

;;; User customizable variables

(defgroup snippy nil
  "Custom snippet management utilities."
  :group 'editing)

(defcustom snippy-install-dir (expand-file-name user-emacs-directory)
  "Directory where to install/clone snippets."
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

;;; Internal Variables

(defconst snippy--min-vscode-version "1.11.0"
  "The minimum VSCode engine version required.")

(defvar snippy--package-json-content nil
  "Package json file content parsed as an alist.")

(defvar-local snippy--buffer-language nil
  "The language currently used by snippy in the local buffer.")

(defvar-local snippy--merged-snippets nil
  "A merged alist of all snippets found in the snippets files.")

(defvar-local snippy--computed-candidates nil
  "List of completely propertized completion candidates for the current buffer.")

;;; Setup & Version Checking

(defun snippy--get-snippet-dir ()
  "Return snippet directory."
  (expand-file-name (cdr snippy-source) snippy-install-dir))

(defun snippy-install-or-update-snippets ()
  "Install or update snippet git repo in `snippy-install-dir'.
If `snippy-install-dir' is nil, it defaults to `user-emacs-directory`."
  (interactive)
  (let* ((base (or snippy-install-dir user-emacs-directory))
         (dest (expand-file-name (cdr snippy-source) base)))
    (if (file-directory-p dest)
        (let ((default-directory dest))
          (message "Pulling updates in %s..." dest)
          (start-process "Snippy-git-pull" nil "git" "pull"))
      (message "Cloning %s to %s..." (car snippy-source) dest)
      (with-temp-buffer
        (if (= 0 (call-process "git" nil t nil "clone" (car snippy-source) dest))
            (message "Finished cloning to %s" dest)
          (message "Git clone failed: %s" (string-trim (buffer-string))))))))

(defun snippy--get-package-data ()
  "Read and parse the package.json file.
Used for getting the snippet paths to read and the VScode engine version."
  (let ((file (expand-file-name "package.json" (snippy--get-snippet-dir))))
    (if (file-exists-p file)
        (setq snippy--package-json-content
              (with-temp-buffer
                (insert-file-contents file)
                (json-parse-buffer :object-type 'alist)))
      (user-error "Snippy: package.json not found.  Run `M-x snippy-install-or-update-snippets' first"))))

;;; Vscode engine check

(defun snippy--clean-version (version)
  "Extract a clean semver string from VERSION, removing ^ or ~."
  (when (stringp version)
    (if (string-match "\\([0-9.]+\\)" version)
        (match-string 1 version)
      version)))

(defun snippy--check-engine-version ()
  "Check if the current VSCode engine version meets the minimum requirement."
  (let ((current-engine-version
         (snippy--clean-version (alist-get 'vscode (alist-get 'engines snippy--package-json-content)))))
    (cond
     ((null current-engine-version)
      (lwarn 'snippy :warning
             "Snippy: Could not determine VSCode version from package.json."))
     ((version< current-engine-version snippy--min-vscode-version)
      (lwarn 'snippy :warning
             "VSCode version %s is below requirement %s. Some features may not work."
             current-engine-version snippy--min-vscode-version)))))

;;; Language Remap

(defvar snippy-emacs-to-vscode-lang-alist
  '((text-mode "plaintext")
    (markdown-mode "markdown")
    (markdown-ts-mode "markdown")
    (gfm-mode "markdown")
    (latex-mode "tex")
    (tex-mode "tex")
    (plain-tex-mode "plaintex")
    (html-mode "html")
    (html-ts-mode "html")
    (mhtml-mode "html")
    (web-mode "html")
    (bibtex-mode "bib" "bibtex")

    ;; C / C++
    (c-mode "c" "cdoc")
    (c-ts-mode "c" "cdoc")
    (c++-mode "cpp" "cppdoc")
    (cpp-mode "cpp")
    (c++-ts-mode "cpp" "cppdoc")

    ;; C#
    (csharp-mode "csharp" "cs" "csharpdoc")
    (csharp-ts-mode "csharp" "cs" "csharpdoc")

    (editorconfig-mode "editorconfig")
    (git-commit-mode "gitcommit" "NeogitCommitMessage")
    (ejs-mode "ejs")
    (eruby-mode "eruby")
    (erlang-mode "erlang")

    ;; Elixir
    (elixir-mode "elixir")
    (elixir-ts-mode "elixir")
    (heex-ts-mode "heex")

    (f90-mode "fortran")
    (fortran-mode "fortran")
    (glsl-mode "glsl")
    (nix-mode "nix")

    ;; Lua
    (lua-mode "lua" "luadoc")
    (lua-ts-mode "lua" "luadoc")

    (go-mode "go")
    (go-ts-mode "go")
    (fennel-mode "fennel")

    ;; PHP
    (php-mode "php" "phpdoc")
    (php-ts-mode "php" "phpdoc")

    (quarto-mode "quarto")
    (rescript-mode "rescript")

    ;; Ruby
    (ruby-mode "ruby" "rails" "rdoc")
    (ruby-ts-mode "ruby" "rails" "rdoc")
    (rspec-mode "rspec")

    ;; Rust
    (rust-mode "rust" "rustdoc")
    (rust-ts-mode "rust" "rustdoc")

    (haskell-mode "haskell")
    (haskell-ts-mode "haskell")
    (scala-mode "scala")
    (solidity-mode "solidity")
    (swift-mode "swift")
    (sql-mode "sql")
    (verilog-mode "systemverilog" "verilog")

    ;; Shell / Bash
    (sh-mode "shellscript" "sh" "shell" "shelldoc" "zsh")
    (bash-ts-mode "shellscript" "sh" "shell" "shelldoc" "zsh")

    (plantuml-mode "plantuml")

    ;; Java
    (java-mode "java" "java-testing" "javadoc")
    (java-ts-mode "java" "java-testing" "javadoc")

    (julia-mode "julia")
    (astro-mode "astro")
    (astro-ts-mode "astro")
    (clojure-mode "clojure")
    (clojure-ts-mode "clojure")

    ;; CSS / Stylesheets
    (css-mode "css")
    (css-ts-mode "css")
    (scss-mode "scss")
    (sass-mode "sass")
    (less-css-mode "less")
    (stylus-mode "stylus")

    ;; JavaScript / JSON
    (js-mode "javascript" "jsdoc")
    (js2-mode "javascript")
    (js-ts-mode "javascript" "jsdoc")
    (js-json-mode "javascript")
    (json-ts-mode "javascript")
    (rjsx-mode "javascriptreact")

    ;; TypeScript
    (typescript-mode "typescript" "jsdoc" "remix" "tsdoc")
    (typescript-ts-mode "typescript" "jsdoc" "remix" "tsdoc")
    (tsx-ts-mode "typescriptreact")

    (terraform-mode "terraform")
    (svelte-mode "svelte")
    (vue-mode "vue")
    (vue-ts-mode "vue")

    ;; Python
    (python-mode "python" "pydoc")
    (python-ts-mode "python" "pydoc")

    (cobol-mode "cobol")

    ;; Kotlin
    (kotlin-mode "kotlin" "kdoc")
    (kotlin-ts-mode "kotlin" "kdoc")

    (r-mode "r")
    (ess-r-mode "r")
    (org-mode "org")
    (gdscript-mode "gdscript")
    (gdscript-ts-mode "gdscript")

    ;; YAML
    (yaml-mode "yaml")
    (yaml-ts-mode "yaml")

    (makefile-mode "make")
    (dart-mode "dart" "flutter")
    (objc-mode "objc")
    (tcl-mode "tcl")
    (perl-mode "perl")
    (vhdl-mode "vhdl")

    ;; Dockerfile
    (dockerfile-mode "dockerfile")
    (dockerfile-ts-mode "dockerfile")

    (powershell-mode "powershell" "ps1")
    (reason-mode "reason")
    (ocaml-mode "ocaml")
    (tuareg-mode "ocaml.ocamllex" "ocamlinterface")
    (purescript-mode "purescript")
    (gleam-mode "gleam")
    (asciidoc-mode "asciidoc")
    (beancount-mode "beancount")
    (rst-mode "rst")

    ;; CMake
    (cmake-mode "cmake")
    (cmake-ts-mode "cmake")

    ;; Zig
    (zig-mode "zig")
    (zig-ts-mode "zig")

    (dune-mode "dune")
    (dune-project-mode "dune-project")
    (edge-mode "edge")
    (fsharp-mode "fsh")
    (jade-mode "jade")
    (pug-mode "pug")
    (jekyll-mode "jekyll")
    (kivy-mode "kivy")
    (liquid-mode "liquid")
    (artbollocks-mode "loremipsum")
    (mint-mode "mint")
    (norg-mode "norg")
    (nushell-mode "nu"))
  "Alist mapping Emacs major modes to a list of VS Code language identifiers.")

(defun snippy--get-vscode-language-name ()
  "Return a list of all VSCode language strings for the current major mode."
  (alist-get major-mode snippy-emacs-to-vscode-lang-alist))

(defun snippy--update-buffer-language ()
  "Update `snippy--buffer-language` based on the current major mode."
  (setq snippy--buffer-language
        (append (snippy--get-vscode-language-name) snippy-global-languages)))

;;; Snippet Reading & Parsing

(defun snippy--get-all-paths-for-language (snippet-entries target-lang)
  "Return a list of all paths in SNIPPET-ENTRIES associated with TARGET-LANG."
  (when target-lang
    (let ((target (if (symbolp target-lang) (symbol-name target-lang) target-lang)))
      (cl-loop for entry across (or snippet-entries [])
               for val = (alist-get 'language entry)
               for val-strings = (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x))
                                         (if (vectorp val) (append val nil) (list val)))
               when (member target val-strings)
               collect (alist-get 'path entry)))))

(defun snippy--get-all-snippets-paths ()
  "Return the snippets paths in package.json file for all languages."
  (alist-get 'snippets (alist-get 'contributes snippy--package-json-content)))

(defun snippy--get-current-language-path ()
  "Return a combined list of snippet paths for all languages."
  (cl-loop with all-dirs = (snippy--get-all-snippets-paths)
           for lang in (ensure-list snippy--buffer-language)
           append (snippy--get-all-paths-for-language all-dirs lang)))

(defun snippy--compute-candidates ()
  "Pre-compute and propertize all capf candidates for speed."
  (let (raw-candidates)
    (pcase-dolist (`(,name . ,data) snippy--merged-snippets)
      (let ((pref (alist-get 'prefix data))
            (desc (alist-get 'description data)))
        (dolist (key (if (stringp pref) (list pref) (append pref nil)))
          (push (propertize key
                            'snippy-name (format "%s" name)
                            'snippy-desc desc
                            'snippy-snippet data)
                raw-candidates))))
    (setq snippy--computed-candidates (delete-dups raw-candidates))))

(defun snippy-refresh-snippets ()
  "Force an update on the snippets for the current buffer."
  (interactive)
  (snippy--update-buffer-language)
  (setq snippy--merged-snippets
        (cl-loop with dir = (snippy--get-snippet-dir)
                 for suffix in (snippy--get-current-language-path)
                 for full-path = (expand-file-name suffix dir)
                 if (file-exists-p full-path)
                 append (with-temp-buffer
                          (insert-file-contents full-path)
                          (json-parse-buffer :object-type 'alist))
                 else
                 do (message "Skipping: %s (not found)" full-path)))
  (snippy--compute-candidates))

(defun snippy--find-snippet-by-prefix (prefix)
  "Return the first snippet entry where the prefix matches PREFIX."
  (cl-find-if (lambda (snippet)
                (let ((target (alist-get 'prefix (cdr snippet))))
                  (cond
                   ((stringp target) (string= prefix target))
                   ((vectorp target) (cl-position prefix target :test #'string=))
                   ((listp target)   (member prefix target)))))
              snippy--merged-snippets))

;;; Snippet Expansion

(defun snippy--get-variable-value (var-name)
  "Resolves VSCode variables to their Emacs string values based on VAR-NAME."
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
  "Transform BODY-STR from VSCode snippet syntax to Yasnippet."
  (let ((result body-str))
    ;; 1. Handle Variables ($VAR, ${VAR}, or ${VAR:default})
    (setq result (replace-regexp-in-string
                  "\\$\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\|\\${\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(:.*?\\)?}"
                  (lambda (match)
                    (save-match-data
                      (let* ((var-name (or (match-string-no-properties 1 match)
                                           (match-string-no-properties 2 match)))
                             (val (snippy--get-variable-value var-name)))
                        (if val val match))))
                  result t t))

    ;; 2. Handle Choices ${1|a,b|}
    (setq result (replace-regexp-in-string
                  "\\${\\([0-9]+\\)|\\(.*?\\)|}"
                  (lambda (match)
                    (save-match-data
                      (let* ((index (match-string-no-properties 1 match))
                             (choices (split-string (match-string-no-properties 2 match) "," t "[ \t\n\r]+"))
                             (lisp-list (format "'(%s)" (mapconcat #'prin1-to-string choices " "))))
                        (format "${%s:$$(yas-choose-value %s)}" index lisp-list))))
                  result t t))

    ;; 3. Handle Placeholders and Deduplication
    (let ((seen-ids '()))
      (setq result (replace-regexp-in-string
                    "\\${\\([0-9]+\\):\\(\\(?:[^}]\\|\\\\}\\)*\\)}"
                    (lambda (match)
                      (save-match-data
                        (let* ((id (match-string-no-properties 1 match))
                               (default (match-string-no-properties 2 match)))
                          (if (member id seen-ids)
                              (concat "$" id)
                            (push id seen-ids)
                            (format "${%s:%s}" id default)))))
                    result t t)))
    result))

(defun snippy-expand-snippet (snippet)
  "Convert VSCode SNIPPET to Yasnippet and expand it."
  (let* ((body-raw (alist-get 'body snippet))
         (body-str (cond ((vectorp body-raw) (mapconcat #'identity body-raw "\n"))
                         ((stringp body-raw) body-raw)
                         (t ""))))
    (yas-expand-snippet (snippy--transform-snippet-body body-str))))

(defun snippy-expand (prefix)
  "Expand snippet by PREFIX."
  (interactive "sEnter snippet prefix: ")
  (unless (featurep 'yasnippet)
    (user-error "Yasnippet is not loaded.  Please install or require it first"))
  (unless (bound-and-true-p yas-minor-mode)
    (yas-minor-mode t))
  (snippy-expand-snippet (snippy--find-snippet-by-prefix prefix)))

;;; Yasnippet Eglot Fix

(defun snippy--fix-lsp-yasnippet (orig-fun snippet &rest args)
  "Intercept and transform SNIPPET from VSCode style to Yasnippet.
ORIG-FUN is the original function being advised.
ARGS are the remaining arguments passed to ORIG-FUN."
  (let ((transformed-body (if (stringp snippet)
                              (snippy--transform-snippet-body snippet)
                            snippet)))
    (apply orig-fun transformed-body args)))

;;;###autoload
(define-minor-mode snippy-fix-lsp-snippet-mode
  "Toggle VSCode to Yasnippet transformation for LSP server snippets."
  :global t
  :group 'snippy
  (if snippy-fix-lsp-snippet-mode
      (advice-add 'yas-expand-snippet :around #'snippy--fix-lsp-yasnippet)
    (advice-remove 'yas-expand-snippet #'snippy--fix-lsp-yasnippet)))

;;; Completion At Point (CAPF)

(defun snippy--doc-buffer (candidate)
  "Generate a documentation buffer for the snippet from CANDIDATE."
  (when-let* ((snippet  (get-text-property 0 'snippy-snippet candidate))
              (body-raw (alist-get 'body snippet))
              (desc     (get-text-property 0 'snippy-desc candidate))
              (mode     major-mode))
    (with-current-buffer (get-buffer-create "*snippy-doc*")
      (let ((inhibit-read-only t)
            (body-str (cond ((vectorp body-raw) (mapconcat #'identity body-raw "\n"))
                            ((stringp body-raw) body-raw)
                            (t ""))))
        (erase-buffer)
        (insert "Expands to:\n\n" body-str)

        (when (and desc (not (string-empty-p desc)))
          (insert "\n" (make-string 20 ?-) "\n" desc))

        (delay-mode-hooks (funcall mode))
        (ignore-errors (font-lock-ensure))
        (setq-local cursor-type nil)
        (read-only-mode 1)
        (current-buffer)))))

(defvar snippy--capf-properties
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

;;;###autoload
(defun snippy-capf (&optional interactive)
  "Complete with snippy at point.
If called interactively restrict completion to `snippy-capf'.
Optional argument INTERACTIVE specifies whether the call is interactive."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'snippy-capf)))
        (or (completion-at-point)
            (user-error "No snippy completions at point")))
    (when (and snippy-minor-mode snippy--computed-candidates)
      (let* ((end (point))
             (start (save-excursion
                      (skip-chars-backward "[:alnum:]!@#$%_&*\\^\\-")
                      (point))))
        `(,start ,end
                 ,snippy--computed-candidates
                 ,@snippy--capf-properties)))))

;;; Minor Modes

;;;###autoload
(define-minor-mode snippy-minor-mode
  "Toggle snippy in the current buffer."
  :group 'snippy
  (if snippy-minor-mode
      (condition-case err
          (progn
            (unless snippy--package-json-content
              (snippy--get-package-data))
            (snippy-refresh-snippets)
            (when (called-interactively-p 'any)
              (snippy--check-engine-version)
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
