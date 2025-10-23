

;;; Beolvasni a current mode snippetjeit, mikor belépünk

;; 1. Get current mode
(message "%s" major-mode)

;; 2. Read JSON from this locaiton into var
;; Read file
(message "%s" (json-read-file "~/Projects/emacsPackages/snipEngine/test.json"))

(message "%s" (json-read-file "~/Projects/emacsPackages/snipEngine/test.json"))

;; Parse json
;; (json-parse-string hello)

;; Function amikor szeretnénk hogy expand legyen

;; 3. Lecserélni a szót a snippetre
