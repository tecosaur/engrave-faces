;;; engrave-faces-latex.el --- Support for engraving buffers to LaTeX -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is part of engrave-faces.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Support for engraving buffers to LaTeX.

;;; Code:

(require 'engrave-faces)

(defcustom engrave-faces-latex-output-style 'preset
  "How to encode LaTeX style information.
When nil, all face properties are applied via \\colorbox, \\textcolor,
\\textbf, etc. each time.
When preset, short commands are generated for `engrave-faces-preset-styles'."
  :type '(choice nil preset)
  :group 'engrave-faces)

(defcustom engrave-faces-latex-colorbox-strut
  "\\vrule height 2.1ex depth 0.8ex width 0pt"
  "LaTeX code which sets the height and depth for any colorboxes."
  :type 'string
  :group 'engrave-faces)

(defun engrave-faces-latex-gen-preamble (&optional theme type)
  "Generate a preamble which provides short commands for the preset styles.
See `engrave-faces-preset-styles' and `engrave-faces-latex-output-style'.
TYPE is passed to `engrave-faces-latex-gen-preamble-line', and when TYPE
is set to colors, the strut definition is also ommited."
  (let ((engrave-faces-current-preset-style
         (if theme
             (engrave-faces-get-theme theme)
           engrave-faces-current-preset-style)))
    (concat
     (unless (eq type 'colors)
       (format "\\newcommand\\efstrut{%s}\n" engrave-faces-latex-colorbox-strut))
     (mapconcat
      (lambda (face-style)
        (engrave-faces-latex-gen-preamble-line
         (car face-style) (cdr face-style) type))
      engrave-faces-current-preset-style
      "\n"))))

(defun engrave-faces-latex-gen-preamble-line (face style &optional type)
  "Generate a LaTeX preamble line for STYLE representing FACE.
type can be set to either the symbol colors or definitions, in which case
it limits the generate lines to only colors or definitions."
  (let ((slug (plist-get style         :slug))
        (fg   (plist-get style         :foreground))
        (bg   (plist-get style         :background))
        (st   (plist-get style         :strike-through))
        (it   (eql (plist-get style    :slant) 'italic))
        (bl   (member (plist-get style :weight) '(bold extra-bold)))
        (defaultslug
          (plist-get (alist-get 'default
                                engrave-faces-current-preset-style)
                     :slug)))
    (let ((colors
           (concat
            (if fg
                (format "\\definecolor{EF%s}{HTML}{%s}" slug (substring fg 1))
              (when (eq type 'colors)
                (format "\\colorlet{EF%s}{EF%s}" slug defaultslug)))
            (when (or (and fg bg) (eq type 'colors))
              "\n")
            (if bg
                (format "\\definecolor{Ef%s}{HTML}{%s}" slug (substring bg 1))
              (when (eq type 'colors)
                (format "\\colorlet{Ef%s}{Ef%s}" slug defaultslug)))))
          (definitions
            (concat
             "\\newcommand{\\EF" slug "}[1]{"
             (when (and bg (not (eq face 'default)))
               (concat "\\colorbox{Ef" slug "}{\\efstrut{}"))
             (when fg (concat "\\textcolor{EF" slug "}{"))
             (when st "\\sout{") (when bl "\\textbf{") (when it "\\textit{")
             "#1}"
             (make-string
              (cl-count-if #'identity
                           (list (and bg (not (eq face 'default))) fg st bl it))
              ?})
             " % " (symbol-name face))))
      (pcase type
        ('colors colors)
        ('definitions definitions)
        ('nil (concat colors "\n" definitions))
        (invalid (user-error "Invalid preamble line type `%s'." invalid))))))

(defun engrave-faces-latex-face-apply (faces content)
  "Convert each (compatable) parameter of FACES to a LaTeX command apllied to CONTENT."
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
  "Create a LaTeX representation of CONTENT With FACES applied."
  (let ((protected-content (replace-regexp-in-string "[\\{}$%&_#]" "\\\\\\&" content))
        (style (engrave-faces-preset-style faces)))
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
  (insert
   (let ((style (cdr (assoc 'default engrave-faces-preset-styles))))
     (if (eq engrave-faces-latex-output-style 'preset)
         (format "\\color{EF%s}" (plist-get style :slug))
       (concat "\\color[HTML]{" (substring (plist-get style :foreground) 1) "}"))))
  (dolist (find-sub engrave-faces-latex-char-replacements)
    (goto-char (point-min))
    (while (search-forward (car find-sub) nil t)
      (replace-match (cdr find-sub))))
  (goto-char (point-min))
  (while (re-search-forward "\n\\([[:space:]]*\\)\\(}+\\)" nil t)
    (replace-match "\\2\n\\1")))

(defun engrave-faces-latex-make-standalone ()
  "Export current buffer to a standalone LaTeX buffer."
  (goto-char (point-min))
  (insert "\\documentclass{article}


\\usepackage[margin=1.5cm]{geometry}
\\usepackage{xcolor}
\\usepackage{fvextra}
\\usepackage{sourcecodepro}
\\pagestyle{empty}\n\n"
          (engrave-faces-latex-gen-preamble)
          "
\\begin{document}
\\setlength{\\fboxsep}{0pt}
\\begin{Verbatim}[breaklines=true, commandchars=\\\\\\{\\}]\n")
  (goto-char (point-max))
  (insert "\\end{Verbatim}
\\end{document}"))

;;;###autoload (autoload #'engrave-faces-latex-buffer "engrave-faces-latex" nil t)
;;;###autoload (autoload #'engrave-faces-latex-buffer-standalone "engrave-faces-latex" nil t)
;;;###autoload (autoload #'engrave-faces-latex-file "engrave-faces-latex" nil t)
(engrave-faces-define-backend "latex" ".tex" #'engrave-faces-latex-face-mapper #'engrave-faces-latex-make-standalone #'latex-mode)
(add-hook 'engrave-faces-latex-after-hook #'engrave-faces-latex-post-processing)

(provide 'engrave-faces-latex)
;;; engrave-faces-latex.el ends here
