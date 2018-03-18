(require 'ess-site)
(require 'ivy)
(require 'popwin)

;; TODO:
;; add optional arguments
(defvar yxl-ess-useful-funcs
  '(("sessionInfo() -- info on loaded pacakges" . "sessionInfo()")
    ("yxlRutils::qq() - quick quit" . "yxlRutils::qq()")
    ("yxlRutils::lsdf() -- list current dataframes" . "yxlRutils::lsdf()")
    ("yxlRutils::lsos() -- list object by size" . "yxlRutils::lsos()"))
  "list of useful R functions to execute")

(defvar yxl-ess-useful-atpoint-funcs
  '(("print()" . "print")
    ("dplyr::glimpse()" . "dplyr::glimpse")
    ("str()" . "str")
    ("summary()" . "summary")
    ("dim()" . "dim")
    ("dimnames()" . "dimnames")
    ("head()" . "head")
    ("tail()" . "tail")
    ("Hmisc::describe()" . "Hmisc::describe")
    ("DT::datatable() -- view dataframe in browser" . "DT::datatable")
    ("listviewer::jsonedit() -- view list in browser" . "listviewer::jsonedit"))
  "list of useful R functions to execute to the current object atpoint.")

(defun yxl-ess-call-atpoint-func (r-func)
  (let ((objname (current-word)))
    (if objname
        (progn
          (ess-execute (concat r-func "(" objname ")"))))))

(defun yxl-ess-call-atpoint-str ()
  (interactive)
  (yxl-ess-call-atpoint-func "str"))

(defun yxl-ess-call-atpoint-generic (r-func)
  (interactive "sR function to execute: ")
  (yxl-ess-call-atpoint-func r-func))

(defun yxl-ess-call-useful-funcs ()
  (interactive)
  (ivy-read "Call useful funcs:"
            yxl-ess-useful-funcs
            :action (lambda (x) (ess-execute (cdr x)))
            :caller 'yxl-ess-call-useful-funcs))

(defun yxl-ess-atpoint ()
  (interactive)
  (ivy-read "Call useful funcs:"
            yxl-ess-useful-atpoint-funcs
            :action (lambda (x) (yxl-ess-call-atpoint-func (cdr x)))
            :caller 'yxl-ess-atpoint))

(defun yxl-ess-atpoint-pop ()
  (interactive)
  (ivy-read "Call useful funcs:"
            yxl-ess-useful-atpoint-funcs
            :action (lambda (x)
                      (let ((ess-execute-in-process-buffer nil))
                        (yxl-ess-call-atpoint-func (cdr x))))
            :caller 'yxl-ess-atpoint-pop))

(defun yxl-ess-rdired-str ()
  (interactive)
  (let ((objname (ess-rdired-object)))
    (ess-execute (concat "str(" objname ")\n"))))

(defun yxl-ess-rdired-atpoint ()
  (interactive)
  (ivy-read "Call useful funcs:"
            yxl-ess-useful-atpoint-funcs
            :action (lambda (x)
                      (let ((objname (ess-rdired-object)))
                        (ess-execute (concat (cdr x) "(" objname ")\n"))))
            :caller 'yxl-ess-rdired-atpoint))

(defun yxl-ess-rdired-atpoint-pop ()
  (interactive)
  (ivy-read "Call useful funcs:"
            yxl-ess-useful-atpoint-funcs
            :action (lambda (x)
                      (let ((objname (ess-rdired-object))
                            (ess-execute-in-process-buffer nil))
                        (ess-execute (concat (cdr x) "(" objname ")\n"))))
            :caller 'yxl-ess-rdired-atpoint-pop))

(defun yxl-ess-open-rstudio ()
  (interactive)
  ;; https://support.rstudio.com/hc/en-us/articles/200711843-Working-Directories-and-Workspaces
  (let ((path (if (and (eq major-mode 'ess-mode)
                       buffer-file-name)
                  buffer-file-name
                default-directory)))
    (cond
     ((string-equal system-type "windows-nt")
      (message "not implemented"))
     ((string-equal system-type "darwin")
      (shell-command (format "open -a Rstudio %s & disown" path)))
     ((string-equal system-type "gnu/linux")
      (call-process-shell-command
       "rstudio" nil 0 nil (format "%s" path))))))

(defun yxl-ess-rdired-popup ()
  "Run dired-like mode on R objects.
This is the main function.  See documentation for `ess-rdired-mode' though
for more information!"
  (interactive)
  (let  ((proc ess-local-process-name)
         (buff (get-buffer-create ess-rdired-buffer)))

    (ess-command ess-rdired-objects buff)
    (ess-setq-vars-local (symbol-value ess-local-customize-alist) buff)

    (with-current-buffer buff
      (setq ess-local-process-name proc)
      (ess-rdired-mode)

      ;; When definiting the function .rdired.objects(), a "+ " is printed
      ;; for every line of the function definition; these are deleted
      ;; here.
      (goto-char (point-min))
      (delete-region (point-min) (1+ (point-at-eol)))

      ;; todo: not sure how to make ess-rdired-sort-num buffer local?
      ;;(set (make-local-variable 'ess-rdired-sort-num) 2)
      ;;(make-variable-buffer-local 'ess-rdired-sort-num)
      (setq ess-rdired-sort-num 1)
      (ess-rdired-insert-set-properties (save-excursion
                                          (goto-char (point-min))
                                          (forward-line 1)
                                          (point))
                                        (point-max))
      (setq buffer-read-only t))

    (popwin:popup-buffer buff
                         :width 0.4 :height 0.4
                         :position 'bottom
                         :stick t :noselect nil :dedicated t)))

(advice-add 'ess-rdired :override #'yxl-ess-rdired-popup)

(defun yxl-ess-repl-popup ()
  "Pop up the ess REPL associated with the current buffer."
  (interactive)
  (ess-force-buffer-current)
  (let ((buff (process-buffer (get-process ess-current-process-name))))
    (popwin:popup-buffer buff
                         :width 0.4 :height 0.4
                         :position 'bottom
                         :stick t :noselect nil :dedicated t)))

(defun yxl-ess-render ()
  "Render current file using rmarkdown::render.
Dependency: rmarkdown (obviously). By default export the output to a
\"output\" directory."
  ;; TODO: Add an option (or a prefix-arg) that renders the output to the
  ;;       root directory.
  ;; TODO: Allow argument specification (output format, ...) etc.
  (interactive)
  (let* ((file (file-name-nondirectory buffer-file-name))
         (main-cmd "Rscript -e 'rmarkdown::render(\"%s\", %s)'")
         (arg (concat "output_format = \"rmarkdown::html_document\","
                      "output_dir = \"output\""))
         (cmd (format main-cmd file arg)))
    (message cmd)
    (shell-command cmd)))

(provide 'yxl-ess)
