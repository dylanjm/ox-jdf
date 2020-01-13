;;; ox-jdf-report --- Org-Template for writing homework in KBAI -*- lexical-binding: t -*-
;;;
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:
(require 'ox)

(add-to-list 'org-latex-classes
             '("jdf"
               "\\documentclass[letterpaper]{jdf}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(org-export-define-derived-backend 'jdf 'latex
  :options-alist
  '((:email "EMAIL" nil "john.doe@gatech.edu")
    (:bibfile "BIBFILE" nil nil))
  :translate-alist '((template . jdf-template))
  :menu-entry
  '(?J "Export with JDF"
       ((?L "As LaTeX buffer" jdf-export-as-latex)
        (?l "As LaTeX file" jdf-export-to-latex)
        (?p "As PDF file" jdf-export-to-pdf)
        (?o "As PDF file and open"
            (lambda (a s v b)
              (if a (jdf-export-to-pdf t s v b)
                (org-open-file (jdf-export-to-pdf nil s v b))))))))


(defun jdf-template (contents info)
  "return complete document string for this export"
  (concat
   ;; Time-stamp.
   (and (plist-get info :time-stamp-file)
        (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
   ;; Document class and packages.
   (let* ((class (plist-get info :latex-class))
          (class-options (plist-get info :latex-class-options))
          (header (nth 1 (assoc class org-latex-classes)))
          (document-class-string
           (and (stringp header)
                (if (not class-options) header
                  (replace-regexp-in-string
                   "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
                   class-options header t nil 1)))))
     (if (not document-class-string)
         (user-error "Unknown LaTeX class `%s'" class)
       (org-latex-guess-babel-language
        (org-latex-guess-inputenc
         (org-element-normalize-string
          (org-splice-latex-header
           document-class-string
           org-latex-default-packages-alist ; Defined in org.el.
           org-latex-packages-alist nil     ; Defined in org.el.
           (concat (org-element-normalize-string (plist-get info :latex-header))
                   (plist-get info :latex-header-extra)))))
        info)))

   ;; Now the core content
   (let ((email (plist-get info :email))
         (author (plist-get info :author))
         (title (plist-get info :title))
         (bib (plist-get info :bibfile)))
     (concat "
\\addbibresource{" (concat (org-export-data bib info) ".bib") "}
\\author{" (org-export-data author info) "}
\\email{" (org-export-data email info) "}
\\title{" (org-export-data title info) "}
\\begin{document}
\\maketitle
"
contents
"
\\end{document}
"))))

;;;###autoload
(defun jdf-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a JDF report letter.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write content.
EXT-PLIST, when provided, is a proeprty list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Export is done in a buffer named \"*Org JDF Report Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let (jdf-special-contents)
    (org-export-to-buffer 'jdf "*Org JDF Report Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (LaTeX-mode)))))

;;;###autoload
(defun jdf-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a JDF report (tex).
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write contents.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
When optional argument PUB-DIR is set, use it as the publishing
directory.
Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep))
        (jdf-special-contents))
    (org-export-to-file 'jdf outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun jdf-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a JDF report (pdf).
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Return PDF file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep))
        (jdf-special-contents))
    (org-export-to-file 'jdf file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun jdf-export-to-pdf-and-open
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((*TEXINPUTS* (format "TEXINPUTS=%s%s/tex/latex/jdf:"
                              (or (getenv "TEXINPUTS") "")
                              (file-name-directory (locate-library "ox-jdf-report"))))
         (process-environment (cons *TEXINPUTS* process-environment)))
    (org-open-file (jdf-export-to-pdf async subtreep visible-only body-only ext-plist))))



(provide 'ox-jdf-report)
;;; ox-jdf-report.el ends here
