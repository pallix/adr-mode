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
  (tabulated-list-init-header))

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
  (let ((adr-files (adr-list-files t)))
    (setq tabulated-list-entries
          (mapcar
           (lambda (file)
             (let ((r (adr-parse-file file)))
               (list (adr-number r) (vector (adr-number r) (adr-date r) (adr-status r) (adr-title r)))
               )
              )
           adr-files
           ))
    (tabulated-list-print t)
    )
  )

(defun adr-show-current-id
    ()
  (tabulated-list-get-id))

(defun adr-idstr-to-filename
    (id)
  (let* ((padded-number (s-pad-left 4 "0" id))
         (filename (car (directory-files "adr" t (concat "^" padded-number "-.*\.md$")))))
    filename))

(defun adr-list-files
    (full)
  (directory-files "adr" full "\.md$"))

(defun adr-parse-file (filename)
  "Parse an adr file.

Return a lisp adr record representing the useful content of filename."
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
       ))))

;; https://stackoverflow.com/questions/11272632/how-to-create-a-column-view-in-emacs-lisp/11529749

