(require 'ivy)

;; A simple pacakge to provide url shortcuts to various online documentations.

(defvar yxl-dp-docs
  '(("R - standard lib" . "https://stat.ethz.ch/R-manual/R-devel/doc/html/packages.html")
    ("R - tidyverse" . "http://tidyverse.org/")
    ("R - r4ds" . "http://r4ds.had.co.nz/")
    ("R - ggplot2" . "http://ggplot2.tidyverse.org/reference/")
    ("R - data.table" . "https://cran.r-project.org/web/packages/data.table/data.table.pdf")
    ("Python - standard lib" . "https://docs.python.org/3.6/library/index.html")
    ("Python - numpy" . "https://docs.scipy.org/doc/numpy/genindex.html")
    ("Python - pandas" . "http://pandas.pydata.org/pandas-docs/stable/")
    ("Python - sklearn" . "http://scikit-learn.org/stable/user_guide.html")
    ("Python - matplotlib" . "https://matplotlib.org/contents.html")
    ("Julia - standard lib" . "https://docs.julialang.org/en/stable/")
    ("Julia - DataFrames" . "https://dataframesjl.readthedocs.io/en/latest/")))

(defun yxl-doc-portal ()
  "Access to online documentations."
  (interactive)
  (ivy-read "Goto documentation:"
            yxl-dp-docs
            :action (lambda (x) (browse-url (cdr x)))
            :caller 'yxl-doc-portal))

(provide 'yxl-doc-portal)
