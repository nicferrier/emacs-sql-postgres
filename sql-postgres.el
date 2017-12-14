;;; derived mode for indent
(require 'cl)

;; These values are all set by their equivalent function
;;  - we use dynbind to ensure this all works
(defvar sql-postgres-last-func-end-point nil)
(defvar sql-postgres-next-func-end-point nil)
(defvar sql-postgres-last-func-decl-point nil)

(defun sql-postgres-last-func-end ()
  (setq sql-postgres-last-func-end-point
	(save-excursion
	  (re-search-backward
	   "^\\$\\$[ \r\n\t]+language[ \t\r\n]+plpgsql;"
	   nil
	   t))))

(defun sql-postgres-next-func-end ()
  (setq sql-postgres-next-func-end-point
	(save-excursion
	  (re-search-forward
	   "^\\$\\$[ \r\n\t]+language[ \t\r\n]+plpgsql;"
	   nil
	   t)
	  (match-beginning 0))))

(defun sql-postgres-last-func-decl ()
  (setq
   sql-postgres-last-func-decl-point
   (save-excursion
     (re-search-backward
      (concat
       ;;      or replace function
       "CREATE[ \t\r\n][A-Za-z \t\r\n]+ "
       ;; the function name
       "[A-Za-z_][A-Za-z_0-9]"
       ;; the arguments - this is no args
       "+[ \t\r\n]*([^)]*)"
       ;; options return type
       "\\([ \t\n\r]+RETURNS[ \t\r\n]+[A-Za-z_-]+\\)*"
       ;; AS block quote - the block quote could actually be anything between two $
       "[ \r\n\t]+AS \\$\\$")
      nil
      t)
     (match-end 0))))

(defun sql-postgres-in-pl-p ()
  "Is POINT inside a PlPgSQL function?"
  (let* ((last-start (sql-postgres-last-func-decl))
	 (last-end (sql-postgres-last-func-end))
	 (next-end (sql-postgres-next-func-end))
	 (in-func (or
		   (and (not last-end)
			(< last-start (point))
			(<= (point) next-end))
		   (and last-end
			(< last-end last-start (point))
			(<= (point) next-end)))))
    in-func))

(defun sql-postgres-in-line-comment-p ()
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at "^[ \t]*--")))

(defun sql-postgres-indent-to (level)
  (let* ((indent-str (make-string level ? )))
    (save-excursion
      (goto-char (line-beginning-position))
      (save-excursion
	(re-search-forward "^\\([\t ]+\\)*[^ \t]" nil (line-end-position)))
      (if (not (match-string 1))
	  (insert indent-str)
	;; else
	(replace-match indent-str nil nil nil 1)))))

(defun sql-postgres-list-begins ()
  "Find the BEGINs in the current function.

Relies on dynamic binding of SQL-POSTGRES-LAST-FUNC-DECL-POINT."
  (save-excursion
    (let (results
	  (begin-pt
	   (re-search-backward "^[ \t]*begin$"
			       sql-postgres-last-func-decl-point t)))
      (while begin-pt
	(setq results (append results (list begin-pt)))
	(setq begin-pt
	      (re-search-backward "^[ \t]*begin$"
				  sql-postgres-last-func-decl-point t)))
      (mapcar (lambda (r) (cons :begin r)) results))))

(defun sql-postgres-list-ends ()
  "Find the ENDs in the current function.

Relies on dynamic binding of SQL-POSTGRES-LAST-FUNC-DECL-POINT."
  (save-excursion
    (let (results
	  (end-pt
	   (re-search-backward "^[ \t]*end;$"
			       sql-postgres-last-func-decl-point t)))
      (while end-pt
	(setq results (append results (list end-pt)))
	(setq end-pt
	      (re-search-backward "^[ \t]*end;$"
				  sql-postgres-last-func-decl-point t)))
      (mapcar (lambda (r) (cons :end r)) results))))

(defun sql-postgres-list-structure ()
  "List the structure (BEGINs and ENDs) of plpgsql."
  (sort
   (append (sql-postgres-list-begins)
	   (sql-postgres-list-ends))
   (lambda (a b)
     (< (cdr a) (cdr b)))))

(defun sql-postgres-this-function-structure ()
  "List the current function's structure.

Relies on `sql-postgres-in-pl-p` having been called to set the
current values for the dynamic pointers around the function."
  (save-excursion
    (goto-char sql-postgres-next-func-end-point)
    (sql-postgres-list-structure)))

(defun sql-postgres-structure-position (structure)
  "What position in the function STRUCTURE is POINT?"
  (cl-position
   (point)
   structure
   :test (lambda (a b)
	   (let ((B (cdr b)))
	     (< a
		(save-excursion ; not only before B but before the extent of B
		  (goto-char B)
		  (goto-char
		   (cl-case (car b)
		     (:end (line-beginning-position))
		     (:begin (+ 1 (line-end-position)))))))))))

(defun sql-postgres-in-function-structure-level (struct)
  "How many levels of indent does POINT need in STRUCT?

Given POINTs position in STRUCT, how many levels of indent deep
is it?"
  (let* ((point-pos (sql-postgres-structure-position struct))
	 (preceeding (cl-subseq struct 0 point-pos))
	 (accumulator 0))
    (mapc (lambda (e)
	    (cl-case (car e)
	      (:begin (setq accumulator (+ accumulator 1)))
	      (:end (setq accumulator (- accumulator 1)))))
	  preceeding)
    accumulator))

(defun sql-postgres-indent ()
  "Indent postgresql including plpgsql."
  (let* (sql-postgres-last-func-end-point
	 sql-postgres-next-func-end-point
	 sql-postgres-last-func-decl-point
	 ;; and now set all these values locally
	 (in-func? (sql-postgres-in-pl-p)))
    (if in-func?
	(let ((struct (sql-postgres-this-function-structure)))
	  (sql-postgres-indent-to
	   (* 4 (sql-postgres-in-function-structure-level struct)))))))

;;; sql-postgres.el ends here
