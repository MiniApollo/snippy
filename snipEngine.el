
;; TODOS
;; - Complete at cursor
;; - Per major mode
;; - cursor jumps


;;; Beolvasni a current mode snippetjeit, mikor belépünk
;; 1. Get current mode
;; (message "%s" major-mode)

;; 2. Read JSON from this locaiton into var
(setq json-object-type 'alist) ;; To set type default is alist
(setq json-array-type 'list) ;; Default is vector e.g [Hello there my name is]

(setq test-snippets (json-read-file "~/Projects/emacsPackages/snipEngine/test.json"))
;; (message "%s" test-snippets)

(setq inputString "fora")
(setq c-snippets (json-read-file "~/Projects/emacsPackages/snipEngine/friendly-snippets/snippets/c/c.json"))

;; 3. Lecserélni a szót a snippetre
;; Search by prefix value
;; Get the body from the object
;; Expand snippet

;; Gagyi megoldás:
;; Get prefix name from completion.
;; Linear search, get object names and check for a prefix match
;; Get the body and expand

;; Loop over list
(setq result
	  (cl-dolist (current-alist c-snippets)

		(when (string-equal inputString (cdr (assoc 'prefix current-alist))) ;; assoc kell alist-get el nem megy
		  (cl-return current-alist))
		)
	  )

;; (message "%s" result)
;; (message "%s" (assoc 'body result))

(setq snippet (cdr (assoc 'body result))) ;; Levágja az elejét és csak az értéket veszi (key . value) -> value

;; (dolist (current-line snippet)
;;   ;; (message "%s" current-line)
;;   (insert (format "%s\n" current-line))
;; )

(defun insertSnippet (snippet)
  "Insert snippet at cursor point"
  (interactive)
  (dolist (current-line snippet)
	;; (message "%s" current-line)
	(insert (format "%s\n" current-line))
	)
)
