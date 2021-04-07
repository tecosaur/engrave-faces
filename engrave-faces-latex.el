;;; engrave-faces-latex.el --- Support for engraving buffers to LaTeX -*- lexical-binding: t; -*-

;; This file is part of engrave-faces.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Support for engraving buffers to LaTeX.

;;; Code:

(require 'engrave-faces)

(defvar engrave-faces-latex-output-style 'preset
  "TODO")

(defun engrave-faces-latex-gen-preamble ()
  "TODO"
  (mapconcat
   (lambda (face-style)
     (engrave-faces-latex-gen-preamble-line (car face-style) (cdr face-style)))
   engrave-faces-preset-styles
   "\n"))

(defun engrave-faces-latex-gen-preamble-line (face style)
  (let ((short (plist-get style         :slug))
        (fg    (plist-get style         :foreground))
        (bg    (plist-get style         :background))
        (st    (plist-get style         :strike-through))
        (it    (eql (plist-get style    :slant) 'italic))
        (bl    (member (plist-get style :weight) '(bold extra-bold))))
    (concat (when fg (format "\\definecolor{EF%s}{HTML}{%s}\n" short (substring fg 1)))
            (when bg (format "\\definecolor{Ef%s}{HTML}{%s}\n" short (substring bg 1)))
            "\\newcommand{\\EF" short "}[1]{"
            (when bg (concat "\\colorbox{Ef" short "}{"))
            (when fg (concat "\\textcolor{EF" short "}{"))
            (when st "\\sout{") (when bl "\\textbf{") (when it "\\textit{")
            "#1}"
            (when bg "}") (when fg "}") (when st "}") (when bl "}") (when it "}")
            " % " (symbol-name face))))

(defun engrave-faces-latex-face-apply (faces content)
  "TODO"
  (let ((attrs (engrave-faces-merge-attributes faces)))
    (let ((bg (plist-get attrs         :background))
          (fg (plist-get attrs         :foreground))
          (it (eql (plist-get attrs    :slant) 'italic))
          (bl (member (plist-get attrs :weight) '(bold extra-bold)))
          (st (plist-get attrs         :strike-through)))
      (concat
       (when bg (concat "\\colorbox[HTML]{" (substring bg 1) "}{"))
       (when fg (concat "\\textcolor[HTML]{" (substring fg 1) "}{"))
       (when st "\\sout{") (when bl "\\textbf{") (when it "\\textit{")
       content
       (when bg "}") (when fg "}") (when st "}") (when bl "}") (when it "}")))))

(defun engrave-faces-latex-face-mapper (faces content)
  "TODO"
  (let ((protected-content (replace-regexp-in-string "[\\{}$%&_#]" "\\\\\\&" content))
        (style (unless (eq faces 'default) (assoc faces engrave-faces-preset-styles))))
    (if (string-match-p "\\`[\n[:space:]]+\\'" content)
        protected-content
      (if (and style (eq engrave-faces-latex-output-style 'preset))
          (concat "\\EF" (plist-get (cdr style) :slug) "{" protected-content "}")
        (engrave-faces-latex-face-apply faces protected-content)))))

(defvar engrave-faces-latex-char-replacements
  '(("\\\\" . "\\\\char92{}")
    ("^" . "\\\\char94{}")
    ("~" . "\\\\char126{}")))

(defun engrave-faces-latex-post-processing ()
  (goto-char (point-min))
  (insert (if (eq engrave-faces-latex-output-style 'preset)
              "\\color{EFD}"
            (concat "\\color[HTML]{"
                    (substring (plist-get (cdr (assoc 'default engrave-faces-preset-styles))
                                          :foreground) 1)
                    "}")))
  (dolist (find-sub engrave-faces-latex-char-replacements)
    (goto-char (point-min))
    (while (search-forward (car find-sub) nil t)
      (replace-match (cdr find-sub))))
  (goto-char (point-min))
  (while (search-forward "\n}" nil t)
    (replace-match "}\n")))

;;;###autoload
(engrave-faces-define-backend "latex" ".tex" #'engrave-faces-latex-face-mapper)
(add-hook 'engrave-faces-latex-after-hook #'engrave-faces-latex-post-processing)

;;;###autoload
(defun engrave-faces-latex-buffer-standalone ()
  "Export current buffer to a standalone LaTeX buffer."
  (interactive)
  (switch-to-buffer (engrave-faces-latex-buffer))
  (goto-char (point-min))
  (insert "\\documentclass{article}

\\usepackage{xcolor}
\\usepackage{fvextra}
\\usepackage[margin=1.5cm]{geometry}
\\usepackage{sourcecodepro}
\\pagestype{empty}\n\n"
          (engrave-faces-latex-gen-preamble)
          "
\\begin{document}

\\begin{Verbatim}[breaklines=true, commandchars=\\\\\\{\\}]\n")
  (goto-char (point-max))
  (insert "\\end{Verbatim}
\\end{document}"))

(provide 'engrave-faces-latex)
;;; engrave-faces-latex.el ends here
