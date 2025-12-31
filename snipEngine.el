
(setq json-object-type 'alist) ;; To set type default is alist
(setq json-array-type 'list) ;; Default is vector e.g [Hello there my name is]
(defvar package-json-location "./friendly-snippets/package.json" "Location of the package.json file")

;; TODO remap major modes to type
;; To read in snippets the package.json can be used. It has all the remaps

;; Megkapni a mode nevet amivel lehet keresni
;; Azt behelyetesíteni

					  ;; (format "~/Projects/emacsPackages/snipEngine/friendly-snippets/snippets/%1$s/%1$s.json" ;; Nem jó mert nem mindig van mappa
					  ;; 		  (car (split-string (downcase mode-name) "/")))

(setq packageJson (json-read-file package-json-location))
(setq snipPackage (assoc 'snippets (assoc 'contributes packageJson)))

(dolist (current-alist snipPackage)
  (when (string-equal "rust" (car (assoc 'language current-alist)))
		(setq snippetPaths (append snippetPaths current-alist)) ;; Nem jó megoldás
		)
)
(message snippetPaths)


(defun elsnip/read-snippet (jsonPath)
  "Read in JSON file by filetype"
  (setq snippetsFile (json-read-file jsonPath))
  )

;; Loop over list
(defun elsnip/search-for-snippet (inputString snippetsFile)
  "Search for input string"
  (setq result
		(cl-dolist (current-alist snippetsFile)
		  (when (string-equal inputString (cdr (assoc 'prefix current-alist))) ;; assoc kell alist-get el nem megy
			(cl-return current-alist))
		  )
		)
  )

;; TODO: Cursor jump
(defun elsnip/insert-snippet (snippet)
  "Insert snippet at cursor point"
  (dolist (current-line snippet)
	;; (message "%s" current-line)
	(insert (format "%s\n" current-line))
	)
  )

;; Main entry
;; Read in specified snippets file
;; Search for snippet
;; InsertSnippet

(defun elsnip/snip-expand (inputString)
  "Main entry point"
  (interactive "sString: ")
  (progn
	;; Read snippets
	(readSnippet)
	;; (message "%s" snippetsFile))

	;; Search for snippet
	(searchForSnippet inputString snippetsFile)

	;; Insert Snippet
	(setq snippet (cdr (assoc 'body result))) ;; Levágja az elejét és csak az értéket veszi (key . value) -> value
	(insertSnippet snippet)
	)
  )
