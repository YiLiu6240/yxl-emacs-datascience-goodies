(require 'counsel-dash)

;; A rewrite of the stock spacemacs layer to fit my needs

(defvar yxl-dash-docset-path "~/.docsets"
  "Path containing dash docsets.")

(defvar yxl-dash-browser-func 'browse-url
  "Func to browse dash docsets.")

(defvar yxl-dash-search-history nil
  "Search history for `yxl-dash-search-docset'")

(defun yxl-dash-activate-package-docsets (path)
  "Add dash docsets from specified PATH."
  (setq helm-dash-docsets-path (expand-file-name path))
  (setq helm-dash-common-docsets (helm-dash-installed-docsets))
  (message (format "activated %d docsets from: %s"
                   (length helm-dash-common-docsets) path)))

(defun yxl-dash-search-docset ()
  (interactive)
  (let* ((counsel-dash-common-docsets
          (list (ivy-read "which docset to use: "
                          (helm-dash-installed-docsets)
                          :history 'yxl-dash-search-history
                          :preselect (if (listp yxl-dash-search-history)
                                         (car yxl-dash-search-history)
                                       yxl-dash-search-history)))))
    (counsel-dash)))

(defun yxl-dash-search-docset-external-browser ()
  (interactive)
  (let ((counsel-dash-browser-func 'browse-url-generic))
    (yxl-dash-search-docset)))

(provide 'yxl-dash)
