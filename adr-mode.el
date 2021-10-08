(require 'cl-lib)
(require 's)
(require 'pp)

(define-derived-mode adr-list-mode tabulated-list-mode "adr-list-mode"
  "Major mode to list Architecture Decision Records."
  (setq tabulated-list-format [("#" 4 t)
                               ("Date" 12 t)
                               ("Status"  10 t)
                               ("Title" 0 t)])
  (setq tabulated-list-padding 4)
  (setq tabulated-list-sort-key '("Status" . nil))
  (tabulated-list-init-header)
  (define-key adr-list-mode-map "n" 'adr-new-record)
  )

(cl-defstruct adr
  "Record saving the main information of an adr file."
  number ;; as string
  (date "")
  (status "")
  (title "")
  filename)

(defun adr-show-records
    ()
  (interactive)
  (pop-to-buffer "*adr records*" nil)
  (adr-list-mode)
  (let* ((adr-files (adr-list-files t))
         (records (remq nil (mapcar
                             (lambda (file) (adr-parse-file file))
                             adr-files))))
    (setq tabulated-list-entries
          (mapcar
           (lambda (r)
             (list (adr-number r) (vector (adr-number r) (adr-date r) (adr-status r) (adr-title r))))
           records))
    (tabulated-list-print t)))

(defun adr-get-current-id
    ()
  (tabulated-list-get-id))

(defun adr-id-to-filename
    (id)
  (let* ((padded-number (s-pad-left 4 "0" id))
         (filename (car (directory-files "adr" t (concat "^" padded-number "-.*\.md$")))))
    filename))

(defun adr-read-records-dir (adr-config-file)
  (with-temp-buffer
    (insert-file-contents adr-config-file)
    (s-trim (buffer-string))))

(defun adr-get-records-dir
    ()
  (if-let ((adr-config-dir (locate-dominating-file default-directory ".adr-dir"))
           (adr-file (concat adr-config-dir "/.adr-dir"))
           (adr-dir (adr-read-records-dir adr-file)))
      (concat adr-config-dir adr-dir)))

(defun adr-list-files
    (full)
  (if-let ((adr-dir (adr-get-records-dir)))
      (directory-files adr-dir full "\.md$")
    (progn
      (message "Cannot find .adr-dir file ")
      nil)))

(defun adr-parse-file (filename)
  "Parse an adr file.

Return a lisp adr record representing the useful content of filename."
  (condition-case err
      (let ((record (make-adr :filename filename)))
        (with-current-buffer (find-file-noselect filename)
          (save-excursion
            (goto-char 0)
            (when (re-search-forward "^# +\\([0-9]+\\)\.? +\\(.+\\)$")
              (setf (adr-number record) (match-string-no-properties 1))
              (setf (adr-title record) (match-string-no-properties 2)))
            (when (re-search-forward "^Date: \\(.+\\)$")
              (setf (adr-date record) (match-string-no-properties 1))
              )
            (when (re-search-forward "^## Status$")
              (when (re-search-forward "\\([^ #\n]+\\)")
                (setf (adr-status record) (match-string-no-properties 1))
                ))

            record
            )))
    ((debug error)
     (message (format "Cannot parse %s. Reason: %s" filename (prin1-to-string err)))
     nil)))

(defun adr-new-record (title)
    ""
    (interactive "sNew ADR title: ")
    (let* ((adr-count (length (adr-list-files nil)))
           (new-adr-id (number-to-string (1+ adr-count)))
           (filename (adr-create-filename new-adr-id title))
           ;; TODO get path
           )
      (message filename)))

(defun adr-create-filename
    (id title)
  (let* ((title (downcase title))
         (title (s-replace " " "-" title))
         (records-dir (adr-get-records-dir)))
    (concat records-dir "/" (s-pad-left 4 "0" id) "-" title ".md")))
