;;; snippy-tests.el --- Tests for snippy.el  -*- lexical-binding:t -*-

;; To run:
;; emacs -batch -f package-initialize -l snippy.el -l snippy-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'snippy)

;;; ============================================================================
;;; Mock Data & Fixtures
;;; ============================================================================

(defvar snippy-test--mock-package-json
  '((engines . ((vscode . "^1.20.0")))
    (contributes . ((snippets . [((language . "javascript") (path . "./snippets/javascript.json"))
                                 ((language . ["typescript" "tsx"]) (path . "./snippets/typescript.json"))]))))
  "Mock representation of parsed package.json.")

(defvar snippy-test--mock-js-snippets
  '(( "Console log" . ((prefix . "clg")
                       (body . ["console.log($1);"])
                       (description . "Log to console")))
    ( "Arrow Function" . ((prefix . ["afn" "arrow"] )
                          (body . "const $1 = () => { $2 }"))))
  "Mock parsed snippets file data.")

;;; ============================================================================
;;; Test Cases
;;; ============================================================================

;; 1. Setup & Directory Methods
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-get-snippet-dir ()
  "Ensure `snippy--get-snippet-dir' builds the path correctly."
  (let ((snippy-install-dir "/tmp/emacs-snippets")
        (snippy-source '("http://github.com" . "my-snippets")))
    (should (string= (snippy--get-snippet-dir) "/tmp/emacs-snippets/my-snippets"))))


;; 2. Version and Engine Checking
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-clean-version ()
  "Verify `snippy--clean-version' extracts correct semantic version strings."
  (should (string= (snippy--clean-version "^1.53.0") "1.53.0"))
  (should (string= (snippy--clean-version "~1.2.0") "1.2.0"))
  (should (string= (snippy--clean-version "1.11.0") "1.11.0"))
  (should-not (snippy--clean-version nil)))

(ert-deftest snippy-test-check-engine-version-warning ()
  "Test `snippy-check-engine-version' alerts when version is below requirement."
  (let ((snippy-package-json-content '((engines . ((vscode . "1.5.0")))))
        (warning-triggered nil))
    ;; Intercept lwarn signals
    (cl-letf (((symbol-function 'lwarn)
               (lambda (type level message &rest args)
                 (when (eq type 'snippy) (setq warning-triggered t)))))
      (snippy-check-engine-version)
      (should warning-triggered))))


;; 3. Language Mapping Checks
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-get-vscode-language-name ()
  "Verify correct resolution of Emacs modes to VSCode languages."
  (with-temp-buffer
    (js-mode)
    (should (equal (snippy--get-vscode-language-name) '("javascript" "jsdoc"))))
  (with-temp-buffer
    (python-ts-mode)
    (should (equal (snippy--get-vscode-language-name) '("python" "pydoc"))))
  (with-temp-buffer
    (delay-mode-hooks (special-mode))
    (should-not (snippy--get-vscode-language-name))))

(ert-deftest snippy-test-update-buffer-language ()
  "Ensure buffer language updates to target plus globals."
  (let ((snippy-global-languages '("global1" "global2")))
    (with-temp-buffer
      (js-mode)
      (snippy--update-buffer-language)
      (should (equal snippy--buffer-language '("javascript" "jsdoc" "global1" "global2"))))))


;; 4. Paths & Path Selection Logic
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-get-all-paths-for-language ()
  "Test nested structures for arrays vs single strings are resolved accurately."
  (let ((data (alist-get 'snippets (alist-get 'contributes snippy-test--mock-package-json))))
    ;; Array check
    (should (equal (snippy--get-all-paths-for-language data "tsx") '("./snippets/typescript.json")))
    ;; Standard single string check
    (should (equal (snippy--get-all-paths-for-language data "javascript") '("./snippets/javascript.json")))
    ;; Symbol fallback tolerance
    (should (equal (snippy--get-all-paths-for-language data 'javascript) '("./snippets/javascript.json")))))


;; 5. Parsing & Finding Snippets
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-find-snippet-by-prefix ()
  "Verify prefixes can match single strings, arrays, or lists inside snippet indexes."
  (should (snippy--find-snippet-by-prefix "clg" snippy-test--mock-js-snippets))
  ;; Array element prefix match
  (should (snippy--find-snippet-by-prefix "arrow" snippy-test--mock-js-snippets))
  ;; Non-existent prefix fallback
  (should-not (snippy--find-snippet-by-prefix "invalid" snippy-test--mock-js-snippets)))


;; 6. Syntactic Transformation (VSCode Syntax -> Yasnippet)
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-transform-snippet-variables ()
  "Test variable extraction and standard date expansions work seamlessly."
  ;; File Base expansion
  (let ((buffer-file-name "/home/user/project/src/main.js"))
    (should (string= (snippy--transform-snippet-body "$TM_FILENAME_BASE") "main")))
  ;; Simple year test
  (should (string-match-p "^[0-9]\\{4\\}$" (snippy--transform-snippet-body "$CURRENT_YEAR"))))

(ert-deftest snippy-test-transform-snippet-choices ()
  "Ensure VSCode style multi-choice syntax gets converted into yas-choose-value logic."
  (let ((input "${1|one,two,three|}")
        (expected "${1:$$(yas-choose-value '(\"one\" \"two\" \"three\"))}"))
    (should (string= (snippy--transform-snippet-body input) expected))))

(ert-deftest snippy-test-transform-snippet-placeholders ()
  "Check that nested and mirrored identifiers optimize/deduplicate cleanly."
  (let ((input "${1:foo} bar $1")
        (expected "${1:foo} bar $1"))
    (should (string= (snippy--transform-snippet-body input) expected))))


;; 7. Completion At Point (CAPF) Validation
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-capf-candidates ()
  "Verify candidate engine extracts prefixes, annotations, and injects properties."
  (let ((snippy--merged-snippets snippy-test--mock-js-snippets)
        (snippy--computed-candidates nil))
    (snippy--compute-candidates)
    (should (= (length snippy--computed-candidates) 3)) ; "clg", "afn", "arrow"
    (let ((clg-cand (cl-find "clg" snippy--computed-candidates :test #'string=)))
      (should clg-cand)
      (should (string= (get-text-property 0 'snippy-name clg-cand) "Console log")))))


;; 8. Minor Mode Lifecycle
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-minor-mode-toggle ()
  "Assert activation triggers updates and teardown clears cached buffer context."
  (with-temp-buffer
    (js-mode)
    ;; Mock initialization configurations to prevent disk reads
    (setq snippy-package-json-content snippy-test--mock-package-json)
    (cl-letf (((symbol-function 'snippy-refresh-snippets)
               (lambda ()
                 (snippy--update-buffer-language)
                 (setq snippy--merged-snippets snippy-test--mock-js-snippets))))

      ;; Turn Mode On
      (snippy-minor-mode 1)
      (should snippy-minor-mode)
      (should (equal snippy--buffer-language '("javascript" "jsdoc")))
      (should snippy--merged-snippets)

      ;; Turn Mode Off
      (snippy-minor-mode -1)
      (should-not snippy-minor-mode)
      (should-not snippy--buffer-language)
      (should-not snippy--merged-snippets))))

;; 9. Deep Variable Resolution (snippy--get-variable-value)
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-get-variable-value-fallback-and-bounds ()
  "Verify variable resolution when no file is visiting, and check index positions."
  (with-temp-buffer
    ;; Explicitly ensure buffer-file-name is nil
    (setq buffer-file-name nil)
    (should (string= (snippy--get-variable-value "TM_FILENAME") "Untitled"))
    (should (string= (snippy--get-variable-value "TM_FILENAME_BASE") "Untitled"))
    (should (string= (snippy--get-variable-value "TM_FILEPATH") ""))

    ;; Test text alignment metrics
    (insert "Line 1\nLine 2\nLine 3")
    (goto-char (point-min))
    (forward-line 1) ; Move to Line 2
    (should (string= (snippy--get-variable-value "TM_LINE_INDEX") "1"))
    (should (string= (snippy--get-variable-value "TM_LINE_NUMBER") "2"))
    (should (string= (snippy--get-variable-value "TM_CURRENT_LINE") "Line 2"))))

(ert-deftest snippy-test-get-variable-value-dates-and-randoms ()
  "Ensure date, Unix timestamps, and randomized generation meet spec formats."
  (should (string-match-p "^[0-9]\\{2\\}$" (snippy--get-variable-value "CURRENT_DATE")))
  (should (string-match-p "^[0-9]\\{2\\}$" (snippy--get-variable-value "CURRENT_HOUR")))
  (should (string-match-p "^[0-9]+$" (snippy--get-variable-value "CURRENT_SECONDS_UNIX")))
  ;; Hex validation
  (should (string-match-p "^[0-9a-f]\\{6\\}$" (snippy--get-variable-value "RANDOM_HEX"))))


;; 10. Advanced Syntactic Transformations (Edge Cases)
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-transform-mixed-syntax ()
  "Verify transformations on bodies containing mixed variables, placeholders, and choices."
  (let ((input "fn ${1:name}(${2|a,b|}) {\n\t$TM_FILENAME\n\treturn $1;\n}")
        ;; We simulate without a file back-ended buffer, so TM_FILENAME maps to "Untitled"
        (expected "fn ${1:name}(${2:$$(yas-choose-value '(\"a\" \"b\"))}) {\n\tUntitled\n\treturn $1;\n}"))
    (with-temp-buffer
      (setq buffer-file-name nil)
      (should (string= (snippy--transform-snippet-body input) expected)))))

(ert-deftest snippy-test-transform-placeholder-deduplication-complex ()
  "Ensure multiple instances of identical placeholder IDs convert subsequent hits to mirrors."
  (let ((input "${1:foo} -> ${1:bar} -> $1")
        (expected "${1:foo} -> $1 -> $1"))
    (should (string= (snippy--transform-snippet-body input) expected))))


;; 11. CAPF Metadata Extraction & Annotations
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-capf-properties-and-annotations ()
  "Test annotation functions and structural properties assigned to candidates."
  (let* ((cand "clg")
         (propertized-cand (propertize cand
                                       'snippy-name "Console Log"
                                       'snippy-desc "Prints to stdout")))
    ;; Test annotation property block logic extraction
    (let ((annot-fun (plist-get snippy-capf-properties :annotation-function)))
      (should (string= (funcall annot-fun propertized-cand) "  Console Log")))

    ;; Test visual classification kind metadata
    (let ((kind-fun (plist-get snippy-capf-properties :company-kind)))
      (should (eq (funcall kind-fun propertized-cand) 'snippet)))))


;; 12. Robustness Against Malformed/Empty Config structures
;; ----------------------------------------------------------------------------
(ert-deftest snippy-test-empty-language-path-tolerance ()
  "Verify path discovery does not crash when package data configurations are empty."
  (let ((empty-data []))
    (should-not (snippy--get-all-paths-for-language empty-data "javascript"))
    (should-not (snippy--get-all-paths-for-language nil "python"))))

(ert-deftest snippy-test-unmapped-mode-fallback ()
  "Ensure unmapped major modes fall back gracefully without breaking buffer states."
  (with-temp-buffer
    ;; Use an arbitrary mode that is truly missing from the mapping alist
    (delay-mode-hooks (special-mode))
    (snippy--update-buffer-language)
    ;; It should fall back to nil (plus any global languages if defined)
    (if snippy-global-languages
        (should (equal snippy--buffer-language snippy-global-languages))
      (should-not snippy--buffer-language))))

(provide 'snippy-tests)
;;; snippy-tests.el ends here
