

;;; Beolvasni a current mode snippetjeit, mikor belépünk

;; 1. Get current mode
;; (message "%s" major-mode)

;; 2. Read JSON from this locaiton into var

(setq json-object-type 'alist) ;; To set type default is alist

;; (message "%s" (json-read-file "~/Projects/emacsPackages/snipEngine/test.json"))
(setq test-snippets (json-read-file "~/Projects/emacsPackages/snipEngine/test.json"))

;; (message "%s" test-snippets)

(setq inputString "st")

;; Loop over list
(let (result)
  (dolist (current-alist test-snippets result)
	 (setq result (string-equal inputString (cdr (assoc 'prefix current-alist))) ;; assoc kell alist-get el nem megy
			)
	;; (message "Alist: %s" (string-equal inputString (cdr (assoc 'prefix current-alist))) ;; assoc kell alist-get el nem megy
			 ;; )
	)
  )
(message "%s" result)

;; (setq c-snippets (json-read-file "~/Projects/emacsPackages/snipEngine/friendly-snippets/snippets/c/c.json"))

;; Function amikor szeretnénk hogy expand legyen

;; 3. Lecserélni a szót a snippetre
;; Search by prefix value
;; Get the body from the object
;; Expand snippet

;; Gagyi megoldás:
;; Get prefix name from completion.
;; Linear search, get object names and check for a prefix match
;; Get the then get the body and expand
