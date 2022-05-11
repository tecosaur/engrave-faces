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

(defcustom engrave-faces-latex-mathescape nil
  "Whether maths characters in comments should be allowed.

When nil, all potential maths (both \"$tex$\" and
\"\\(latex\\)\") is protected by
`engrave-faces-latex--protect-content'. Three non-nil symbols are
supported:
- latex, in which case the content of LaTeX maths is left unprotected
- tex, in which case the content of TeX dollar-delimited maths is left
  unprotected
- t, in which case LaTeX and TeX maths are supported

This only affects text set with `font-lock-comment-face'.

For TeX maths to be supported, fvextra's mathescape option must
also be applied. This is done automatically when generating a
standalone document."
  :type 'boolean
  :group 'engrave-faces)

(defcustom engrave-faces-latex-colorbox-strut
  "\\vrule height 2.1ex depth 0.8ex width 0pt"
  "LaTeX code which sets the height and depth for any colorboxes."
  :type 'string
  :group 'engrave-faces)

(defun engrave-faces-latex-gen-preamble (&optional theme)
  "Generate a preamble which provides short commands for the preset styles.
See `engrave-faces-preset-styles' and `engrave-faces-latex-output-style'."
  (let ((preset-style
         (if theme
             (engrave-faces-get-theme theme)
           engrave-faces-current-preset-style)))
    (concat
     (unless (cl-notany (lambda (s) (plist-get (cdr s) :background))
                        preset-style)
       (format "\\newcommand\\efstrut{%s}\n" engrave-faces-latex-colorbox-strut))
     (mapconcat
      (lambda (face-style)
        (engrave-faces-latex-gen-preamble-line (car face-style) (cdr face-style)))
      preset-style
      "\n"))))

(defun engrave-faces-latex-gen-preamble-line (face style)
  "Generate a LaTeX preamble line for STYLE representing FACE."
  (let ((short (plist-get style         :slug))
        (fg    (plist-get style         :foreground))
        (bg    (plist-get style         :background))
        (st    (plist-get style         :strike-through))
        (it    (eql (plist-get style    :slant) 'italic))
        (bl    (member (plist-get style :weight) '(bold extra-bold))))
    (concat (when fg (format "\\definecolor{EF%s}{HTML}{%s}\n" short (substring fg 1)))
            (when bg (format "\\definecolor{Ef%s}{HTML}{%s}\n" short (substring bg 1)))
            "\\newcommand{\\EF" short "}[1]{"
            (when (and bg (not (eq face 'default)))
              (concat "\\colorbox{Ef" short "}{\\efstrut{}"))
            (when fg (concat "\\textcolor{EF" short "}{"))
            (when st "\\sout{") (when bl "\\textbf{") (when it "\\textit{")
            "#1}"
            (make-string
             (cl-count-if #'identity
                          (list (and bg (not (eq face 'default))) fg st bl it))
             ?})
            " % " (symbol-name face))))

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

(defconst engrave-faces-latex--char-replacements
  '(("\\\\" . "\\char92{}")
    ("^" . "\\char94{}")
    ("~" . "\\char126{}")))

(defun engrave-faces-latex--protect-content (content)
  (replace-regexp-in-string
   (regexp-opt (mapcar #'car engrave-faces-latex--char-replacements))
   (lambda (char)
     (cdr (assoc char engrave-faces-latex--char-replacements)))
   (replace-regexp-in-string
    "[\\{}$%&_#]" "\\\\\\&"
    content)
   nil t))

(defun engrave-faces-latex--protect-content-mathescape (content)
  (let ((dollar-maths
         (and (memq engrave-faces-latex-mathescape '(t tex TeX))
              (string-match-p "\\$.+\\$" content)))
        (paren-maths
         (and (memq engrave-faces-latex-mathescape '(t latex LaTeX))
              (string-match-p "\\\\(.+\\\\)" content))))
    (replace-regexp-in-string
     (cond
      (dollar-maths "^\\([^$]*\\)\\(\\$.+\\$\\)\\([^$]*\\)$")
      (paren-maths "^\\(.*?\\)\\(\\\\(.+\\\\)\\)\\(.*?\\)$")
      (t "^\\(.*\\)\\(\\)\\(\\)$"))
     (lambda (full-match)
       (concat (engrave-faces-latex--protect-content (match-string 1 full-match))
               (match-string 2 full-match)
               (engrave-faces-latex--protect-content (match-string 3 full-match))))
     content
     nil t)))

(defun engrave-faces-latex-face-mapper (faces content)
  "Create a LaTeX representation of CONTENT With FACES applied."
  (let* ((style (engrave-faces-preset-style faces))
         (protected-content
          (funcall
           (if (and engrave-faces-latex-mathescape
                    (eq 'font-lock-comment-face (car style)))
               #'engrave-faces-latex--protect-content-mathescape
             #'engrave-faces-latex--protect-content)
           content)))
    (if (string-match-p "\\`[\n[:space:]]+\\'" content)
        protected-content
      (if (and style (eq engrave-faces-latex-output-style 'preset))
          (concat "\\EF" (plist-get (cdr style) :slug) "{" protected-content "}")
        (engrave-faces-latex-face-apply faces protected-content)))))

(defun engrave-faces-latex--post-processing ()
  " Set the initial text color and curly paren positioning.
Trailing curly parens are sometimes put on the next line, and need to be moved back."
  (goto-char (point-min))
  (insert
   (let ((style (cdr (assoc 'default engrave-faces-preset-styles))))
     (if (eq engrave-faces-latex-output-style 'preset)
         (format "\\color{EF%s}" (plist-get style :slug))
       (concat "\\color[HTML]{" (substring (plist-get style :foreground) 1) "}"))))
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
\\begin{document}\n"
          (let ((default-face
                  (alist-get 'default engrave-faces-current-preset-style)))
            (concat
             (when (plist-get default-face :background)
               (format "\\pagecolor{Ef%s}\n" (plist-get default-face :slug)))
             (when (plist-get default-face :foreground)
               (format "\\color{EF%s}\n" (plist-get default-face :slug)))))
          "\\setlength{\\fboxsep}{0pt}
\\begin{Verbatim}[breaklines=true, commandchars=\\\\\\{\\}"
          (if engrave-faces-latex-mathescape
              ", mathescape" "")
          "]\n")
  (goto-char (point-max))
  (insert "\\end{Verbatim}
\\end{document}"))

;;;###autoload (autoload #'engrave-faces-latex-buffer "engrave-faces-latex" nil t)
;;;###autoload (autoload #'engrave-faces-latex-buffer-standalone "engrave-faces-latex" nil t)
;;;###autoload (autoload #'engrave-faces-latex-file "engrave-faces-latex" nil t)
(engrave-faces-define-backend "latex" ".tex" #'engrave-faces-latex-face-mapper #'engrave-faces-latex-make-standalone #'latex-mode)
(add-hook 'engrave-faces-latex-after-hook #'engrave-faces-latex--post-processing)

(provide 'engrave-faces-latex)
;;; engrave-faces-latex.el ends here
