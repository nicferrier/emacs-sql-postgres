(require 'sql-postgres)

(defun sql-postgres-test-func-buffer ()
  (with-current-buffer (get-buffer-create "dummy.sql")
    (erase-buffer)
    (insert "CREATE OR REPLACE FUNCTION schema_init ()
AS $$
BEGIN
   	CREATE SEQUENCE IF NOT EXISTS route_ids;
	DROP TABLE IF EXISTS route;
	CREATE TABLE IF NOT EXISTS route (\"id\" INTEGER,
					  \"path\" TEXT,
					  \"port\" INTEGER);
   END;
$$ LANGUAGE plpgsql;
")
    (current-buffer)))

(defun sql-postgres-test-in-pl-p ()
  (unless 
      (with-current-buffer (sql-postgres-test-func-buffer)
	(goto-char (point-min))
	(re-search-forward "DROP TABLE" nil t)
	(sql-postgres-in-pl-p))
    (error "sql-postgres-test-in-pl-p #1 failed"))
  (when
      (with-current-buffer (sql-postgres-test-func-buffer)
	(goto-char (point-max))
	(sql-postgres-in-pl-p))
    (error "sql-postgres-test-in-pl-p #2 failed")))

(defun sql-postgres-test-func-structure-buffer ()
  (with-current-buffer (get-buffer-create "dummy.sql")
    (erase-buffer)
    (insert "CREATE OR REPLACE FUNCTION schema_init ()
AS $$
BEGIN
   	CREATE SEQUENCE IF NOT EXISTS route_ids;
	DROP TABLE IF EXISTS route;
   begin
	CREATE TABLE IF NOT EXISTS route (\"id\" INTEGER,
					  \"path\" TEXT,
					  \"port\" INTEGER);
      end;
   END;
$$ LANGUAGE plpgsql;
")
    (current-buffer)))

(defun sql-postgres-test-structure ()
  (unless (equal
	   '((:begin . 49) (:begin . 129) (:end . 231) (:end . 242))
	   (with-current-buffer (sql-postgres-test-func-structure-buffer)
	     (re-search-forward "^\\$\\$" nil t)
	     (sql-postgres-list-structure)))
    (error "sql-postgres-test-stucture not parsing structure properly")))

;;; tests.el ends here
