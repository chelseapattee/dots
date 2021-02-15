;;
;; To enable, put this in your .emacs:
;;
;;   (require 'mural)
;;   (mural-add-tagfile "~/www/TAGS")
;;   (mural-add-tagfile "~/local/hphp/src/TAGS")
;;   (global-set-key (kbd "C-o") 'mural-open-dwim)
;;
;; To debug launch:
;; (call-process "mural_server" nil t nil (expand-file-name "~/www/TAGS"))

(eval-when-compile
  (when (fboundp #'byte-compile-disable-warning)
    (byte-compile-disable-warning 'cl-functions)))

(require 'ido)
(require 'cl)

(defvar mural-server-path "mural_server"
  "Path to the mural_server executable")

(defvar mural-server-echo nil
  "echo server responses to *muralserver* buffer")

(defvar mural-tagfile-to-process '()
  "alist from tagfile name to a mural_server process that handles this tagfile")

(defvar mural-current-tagfile nil)
(defvar mural-server-output nil)
(defvar mural-last-query-result nil)

(defun mural-add-tagfile (tagfile)
  "Register tagfile and make it available for queries"
  (mural-get-server (expand-file-name tagfile)))

(defun mural-add-tag-process-entry (tagfile process)
  "Add or replace the process associated with tagfile"
  (let ((entry (assoc tagfile mural-tagfile-to-process)))
    (if entry
        (setcdr entry process)
      (setq mural-tagfile-to-process
            (append
             mural-tagfile-to-process
             (list (cons tagfile process)))))))

(defun mural-get-server (tagfile)
  "Return the process for the given tagfile, if necessary creating/restarting it"
  (let ((proc (cdr (assoc tagfile mural-tagfile-to-process))))
    (if (or (not proc)
            (not (eq (process-status proc) 'run)))
        (let ((newproc (mural-start-server tagfile)))
          (mural-add-tag-process-entry tagfile newproc)
          newproc)
      proc)))

(defun mural-start-server (tagfile)
  "fork a new mural_server process with this tagfile, returning the process"
  (if (not tagfile)
      (error "tagfile not provided"))
  (message "Starting mural server")
  (let ((proc (start-process
               "mural" "*muralserver*"
               mural-server-path tagfile)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter
     proc
     (lambda (process output)
       (with-current-buffer (process-buffer process)
         (save-excursion
           (if mural-server-echo
               (progn
                 (goto-char (process-mark process))
                 (insert output)
                 (set-marker (process-mark process) (point))))
           (setq mural-server-output (concat mural-server-output output))))))
     proc))

(defun mural-tagfile-for-filename (filename)
  "Given a source file, return the name of the associated
tagfile.  This matches based on sharing the same parent
directory, so /home/bob/project/foo.c will match the tag file in
/home/bobo/project/TAGS.  If no match is found, return nil"
  (let ((resolved-fname (file-truename filename)))
    (car (delete-if-not
          (lambda (tagfile)
            ;; file-truename resolves symlinks so we make sure to
            ;; compare the same thing
            (eq 0 (string-match
                   (file-truename (file-name-directory tagfile))
                   resolved-fname)))
          (mapcar 'car mural-tagfile-to-process)))))

(defun mural-query (query tagfile)
  "Run a fuzzy QUERY against TAGFILE and return the results as
specified in mural-parse-response"
  (if (equal "" query)
      '()
    (let ((proc (mural-get-server tagfile))
          (mural-server-output ""))
      (process-send-string (process-name proc)
                           (concat query "\n"))

      ;; Wait up to 1/2 second
      (loop for i from 1 to 10
            until (mural-response-complete mural-server-output)
            do (accept-process-output proc .05))
      (if (mural-response-complete mural-server-output)
          (mural-parse-response tagfile mural-server-output)
        (error "mural-server timeout"))
      )))

(defun hash-incr (key table)
  "Increment `key` in hashtable `table`"
  (puthash key (+ 1 (gethash key table 0)) table))

(defun mural-first-single (names count-table)
  "Return the first item in the list `names` that has a value of 1 in count-table"
  (let ((name (car names)))
    (if (or (eq (cdr names) nil)
            (equal 1 (gethash name count-table)))
        name
      (mural-first-single (cdr names) count-table))))

(defun mural-parse-response (tagfile results)
  "RESULTS is a tab-delimited string of the format
    MATCH tag file row
    ...
    DONE ...

mural-parse-response parses this into '((tag file-path row tagfile-path) ...).
Filenames are resolved relative to TAGFILE"
  (delq nil
        (mapcar
         (lambda (x)
           (let ((row (split-string x "\t")))
             (if (equal (elt row 0) "MATCH")
                 ;; MATCH tag filename row
                 (list (elt row 1)
                       (elt row 2)
                       (string-to-number (elt row 3))
                       tagfile)
               ;; hopefully DONE
               nil)))
         (split-string results "\n"))))

(defun mural-response-complete (result)
  "Returns t if RESULT is a complete response from the server"
  (integerp (string-match "^DONE" result)))

(defun mural-infer-tagfile ()
  (let* ((file-base (buffer-file-name))
         (tagfile-for-buffer (and file-base
                                  (mural-tagfile-for-filename file-base))))
    ;; try 1. current repo, 2. last repo used, 3. whatever is configured
    (let ((tagfile (or
                    tagfile-for-buffer
                    mural-current-tagfile
                    (car (car mural-tagfile-to-process)))))
      (if (not tagfile)
          (error "unable to infer tagfile")
        (setq mural-current-tagfile tagfile)))))

;; Tags are lists like '(tag filename row tagfile)
(defun mural-tag-tagname (tag)
  (elt tag 0))
(defun mural-tag-filename (tag)
  (elt tag 1))
(defun mural-tag-absfilename (tag)
  (file-truename
   (concat
    (file-name-directory (elt tag 3))
    (mural-tag-filename tag))))

(defun mural-tag-row (tag)
  (elt tag 2))

(defvar goto-hist '())

(defun mural-goto-first ()
  (interactive)
  (let* ((default (format "%s" (thing-at-point 'symbol)))
         (prompt (format "Find symbol (default %s): " default))
         (query (read-from-minibuffer prompt nil nil t 'goto-hist default))
         (query-string (format "%s" query))
         (results (mural-query query-string (mural-infer-tagfile))))

    (if (null results)
        (message "No results found for: %s" query-string)
      (let ((taginfo (car results)))
        (find-file (mural-tag-absfilename taginfo))
        (goto-char (point-min))
        (forward-line (1- (mural-tag-row taginfo)))))))

(defun format-taginfo (tag)
  (format "%s [%s:%d]"
          (mural-tag-tagname tag)
          (file-name-nondirectory (mural-tag-filename tag))
          (mural-tag-row tag)))

(defun make-table (tags)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (tag tags)
      (puthash (format-taginfo tag) tag table))
    table))

(defun mural-goto-ido ()
  (interactive)
  (let* ((default (format "%s" (thing-at-point 'symbol)))
         (prompt (format "Find symbol (default %s): " default))
         (query (read-from-minibuffer prompt nil nil t 'goto-hist default))
         (query-string (format "%s" query))
         (results (mural-query query-string (mural-infer-tagfile))))

    (if (null results)
        (message "No results found for: %s" query-string)
      (let* ((table (make-table results))
             (selection (ido-completing-read "" (mapcar 'format-taginfo results)))
             (taginfo (gethash selection table)))
        (find-file (mural-tag-absfilename taginfo))
        (goto-char (point-min))
        (forward-line (1- (mural-tag-row taginfo)))))))

(provide 'mural)
