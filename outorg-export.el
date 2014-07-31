;; * Code

;; Ensure Outorg is loaded.
(require 'outorg)

(defgroup outorg-export nil
  "Customization options for automatic export of portions of a file
through the use of outorg."
  :group 'outorg)

(defcustom outorg-export-export-commands 'nil
  "Variable controlling what will be exported after saving.

If NIL, nothing will be exported.  Otherwise it must be a list of
entries for sections to be exported and exporter to use.  Each entry
is a list like this:

    (headline &optional exporter file level)

headline     Outshine commented headline to export from.  If `t',
             defaults to entire file.
exporter     Org exporter format to use when exporting.  If not
             specified (blank, `t' or `nil'), defaults to an Org
             file.  This is a symbol for the exporter to use.  If
             exporting to PDF, specify `PDF' rather than `LATEX'
             otherwise it will be exported as a `.tex' file.
file         If set, specifies the export file name.  If `T', defaults
             to the name of the exported buffer.  If `NIL', defaults
             to the name of the headline being exported if specified,
             or buffer file name if entire buffer is exported.
level        By default `SECTION' will search for top level headlines
             `* '.  Level allows for exporting specific sublevels
             instead.  This integer indicates how many stars to match.

Setting `outorg-export-export-commands' to `t' will be treated as equal
to `(('t 'org))', exporting the entire file as an org document."
  :group 'outorg-export
  :safe #'listp
  :type '(choice (repeat (list
                          (choice (string :tag "Headline")
                                  (const :tag "Entire file" t))
                          (symbol :value org :tag "Exporter")
                          (choice (const :tag "Buffer file name" t)
                                  (const :tag "Headline name" nil)
                                  (string :tag "Export file name"))
                          (integer :value 1 :tag "Level")))
                 (const :tag "Entire File as Org" t)
                 (const :tag "Nothing" nil)))

(defun outorg-export-export-insinuate (&optional disable)
  "Adds `after-save-hook' to ensure `outorg-export-export' is run.

If `DISABLE' is `t', remove the hook."
  (interactive)
  (if disable
      (remove-hook 'after-save-hook 'outorg-export-export)
    (add-hook 'after-save-hook 'outorg-export-export)))

(defun outorg-export-export (&optional arg)
  "Exports a portion of the current buffer after converting it using
`outorg'.

Export is controlled by `org-export-export-commands' either set
globally or using buffer local variables (recommended).  Setting
globally may cause unintended exporting in files that do not need
exporting.

For details on `org-export-export-commands', see the variable's
docstring.

If called interactively and `org-export-export-commands' is
`nil', it will warn that there is no specified export."
  (interactive "p")
  (let ((export-format outorg-export-export-commands))
    ;; Commented messages in cond statement are for debugging, to
    ;; verify which switch is being used.
    (cond
     ;; Case 1 :: interactive and no export-format set
     ((and arg
           (not export-format))
      (lwarn "outorg-export" :warning "Org-export-export-commands is not set."))
     ;; Case 2 :: Non-nested list with specific headline
     ((stringp (car export-format))
      ;; (message "2")
      (outorg-export--export export-format arg))
     ;; Case 3 :: Export-format is simply `t' or `(quote t)
     ((eq 't export-format)
      ;; (message "3")
      (outorg-export--export (list export-format) arg))
     ;; Case 4 :: Export-format is non-nested and exporting whole
     ;; buffer
     ((equal 't (car export-format))
      ;; (message "4")
      (outorg-export--export export-format arg))
     ;; Case 5 :: Export-format is nested lists
     (t
      ;; (message "5")
      (loop for entry in export-format do
            (outorg-export--export entry arg))))))

(defun outorg-export--export (export arg)
  "Creates a temporary buffer from the file and isolates the content
to be exported."
  (let* (;; Settings from parent file to ensure outorg can process
         ;; correctly.
         (origin        (buffer-file-name))
         (mode          major-mode)
         ;; If section is a string, use it, otherwise `t' for entire
         ;; buffer.
         (section       (if (stringp (nth 0 export))
                            (nth 0 export)
                          t))
         ;; If not specified, default to org
         (exporter      (or
                         (nth 1 export)
                         'org))
         (file          (cond
                         ;; `file' is specified, use it
                         ((stringp (nth 2 export))
                          (nth 2 export))
                         ;; File is `nil' and `section' is set, use
                         ;; `section'
                         ((and (stringp section)
                               (eq (nth 2 export) 'nil))
                          section)
                         ;; Otherwise (or `file' is `nil'), use
                         ;; filename.
                         ((file-name-sans-extension
                           (file-name-nondirectory
                            buffer-file-name)))))
         ;; Default to level 1 if not specified
         (level         (or
                         (nth 3 export)
                         1))
         ;; Create search regexp (multiple comment indicators allowed
         ;; at start of string), followed by appropriate number of
         ;; stars and `section'.
         (section-match (format
                         "^%s+ \\*\\{%s\\} %s"
                         comment-start
                         level
                         section))
         ;; Same regexp as above, but without `section'.
         (section-end   (format
                         "^%s+ \\*\\{,%s\\}"
                         comment-start
                         level)))
    (with-temp-buffer
      (insert-file-contents origin)
      (goto-char (point-min))
      (funcall mode)
      (condition-case nil
          (progn
            ;; Only search if section is a string
            (when (stringp section)
              ;; If section is not found, signal error so that nothing
              ;; is exported
              (re-search-forward section-match (point-max) 'nil)
              ;; Delete everything before matching section
              (forward-line 0)
              (delete-region (point-min) (point))
              (forward-line 1)
              ;; Section can go to end of buffer, no error needed
              (re-search-forward section-end (point-max) 'noerror)
              ;; Delete everything after the end of the section
              (forward-line 0)
              (delete-region (point) (point-max)))
            ;; Required to ensure export succeeds (or
            ;; outorg-code-buffer-point-marker will signal an error)
            (setq outorg-code-buffer-point-marker (point-marker))
            ;; Convert in place into `org', then send to export
            (outorg-convert-to-org)
            (outorg-export--export-to exporter file))
        ;; Only indicates missing headline, all other cases should be
        ;; caught by cond above.
        (error
         ;; Passed interactive argument from initial call.  Only warn
         ;; if interactive.
         (when arg
           (lwarn "outorg export" :warning
                  "Headline \"%s\" not found.  Export failed."
                    section)))))))

(defun outorg-export--export-to (exporter file)
  "Performs the actual export process to `FILE'.

`EXPORTER' is used to determine which export function to use by
matching it against the functions in the `OBARRAY'.  Special
cases where the functions are not clear such as PDF and Latex are
caught before searching the obarray."
  (let ((buffer-file-name file)
        (search-format    (format "org-%s-export-to" exporter))
        export-function)
    (cond
     ;; PDF uses latex so must test for PDF or Latex specifically
     ((equal 'pdf exporter)
      (setq export-function 'org-latex-export-to-pdf))
     ((equal 'latex exporter)
      (setq export-function 'org-latex-export-to-latex))
     ;; Find matching export function, special cases of
     ;; org-*-export-to-% are caught above (latex vs pdf)
     ((setq export-function (nth 0
                                 (all-completions
                                  search-format
                                  obarray 'fboundp)))))
    ;; All-completions returns string, ensure symbol for funcall
    ;; purposes
    (funcall (intern export-function))))

(provide 'outorg-export)

;; Local Variables:
;; outorg-export-export-commands: (("Introduction" md "README"))
;; End:
