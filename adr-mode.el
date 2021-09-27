(require 'cl-lib)

(cl-defstruct adr
  "Record saving the main information of an adr file."
  number
  (title "")
  (date "")
  filename)

(defun adr-list-files
    (full)
  (directory-files "adr" full "\.md$"))

(defun adr-parse-file (filename)
  "Parse an adr file.

Return (number description title) or nil."
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
;;       (list number title date)
       record
       ))))
