;;; snippy-tests.el --- Tests for snippy.el  -*- lexical-binding:t -*-

;; To run:
;; emacs -batch -f package-initialize -l snippy.el -l snippy-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'json)
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
  (should (string= (snippy--get-vscode-language-name 'js-mode) "javascript"))
  (should (string= (snippy--get-vscode-language-name 'python-ts-mode) "python"))
  (should-not (snippy--get-vscode-language-name 'non-existent-major-mode)))

(ert-deftest snippy-test-update-buffer-language ()
  "Ensure buffer language updates to target plus globals."
  (let ((snippy-global-languages '("global1" "global2")))
    (with-temp-buffer
      (js-mode)
      (snippy--update-buffer-language)
      (should (equal snippy--buffer-language '("javascript" "global1" "global2"))))))


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
  (let ((snippy--merged-snippets snippy-test--mock-js-snippets))
    (let ((res (snippy-capf-candidates "cl")))
      (should (= (length res) 1))
      (should (string= (car res) "clg"))
      (should (string= (get-text-property 0 'snippy-name (car res)) "Console log")))))


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
      (should (equal snippy--buffer-language '("javascript")))
      (should snippy--merged-snippets)

      ;; Turn Mode Off
      (snippy-minor-mode -1)
      (should-not snippy-minor-mode)
      (should-not snippy--buffer-language)
      (should-not snippy--merged-snippets))))

(provide 'snippy-tests)
;;; snippy-tests.el ends here
