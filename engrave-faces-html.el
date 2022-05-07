;;; engrave-faces-html.el --- Support for engraving buffers to HTML -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is part of engrave-faces.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Support for engraving buffers to HTML.

;;; Code:

(require 'engrave-faces)

(defcustom engrave-faces-html-output-style 'preset
  "How to encode HTML style information.
When nil, all face properties are applied via inline styles.
When preset, CSS classes are generated for `engrave-faces-preset-styles'."
  :type '(choice nil preset)
  :group 'engrave-faces)

(defcustom engrave-faces-html-class-prefix "ef-"
  "Prefix to use when generating CSS class names."
  :type 'string
  :group 'engrave-faces)

(defun engrave-faces-html-gen-stylesheet (&optional theme indent)
  "Generate a preamble which provides short commands for the preset styles.
See `engrave-faces-preset-styles' and `engrave-faces-html-output-style'."
  (let ((stylesheet
         (mapconcat
          (lambda (face-style)
            (engrave-faces-html--gen-stylesheet-entry (car face-style) (cdr face-style)))
          (if theme
              (engrave-faces-get-theme theme)
            engrave-faces-current-preset-style)
          "\n")))
    (if indent
        (mapconcat (lambda (line)
                     (concat indent line))
                   (split-string stylesheet "\n")
                   "\n")
      stylesheet)))

(defun engrave-faces-html--gen-stylesheet-entry (face style)
  "Generate a HTML preamble line for STYLE representing FACE."
  (concat "." engrave-faces-html-class-prefix (or (plist-get style :slug)
                                                  (symbol-name face))
          " {\n  "
          (engrave-faces-html--gen-style-css style "\n  ")
          " }"))

(defun engrave-faces-html--gen-style-css (attrs seperator)
  "Compose the relevant CSS styles to apply compatible ATTRS, seperated by SEPERATOR."
  (let ((fg    (plist-get attrs      :foreground))
        (bg    (plist-get attrs      :background))
        (st    (plist-get attrs      :strike-through))
        (ul    (plist-get attrs      :underline))
        (it    (eql (plist-get attrs :slant) 'italic))
        (wt    (plist-get attrs      :weight))
        (ht    (plist-get attrs      :height)))
    (mapconcat
     #'identity
     (delq nil
           (list
            (when fg (format "color: %s;" fg))
            (when bg (format "background-color: %s;" bg))
            (when st "text-decoration: line-through;")
            (when ul "text-decoration: underline;")
            (when it "text-decoration: italic;")
            (when wt (format "font-weight: %s;" (engrave-faces-html--css-weight wt)))
            (when (and ht (floatp ht)) (format "font-size: %sem" ht))))
     seperator)))

(defun engrave-faces-html--css-weight (weight)
  (pcase weight
    ('ultra-light 100) ('extra-light 100)
    ('light 200) ('thin 200)
    ('semi-light 300)
    ('book 400) ('normal 400) ('regular 400)
    ('medium 500)
    ('semi-bold 600) ('demi-bold 600)
    ('bold 700)
    ('extra-bold 800)
    ('heavy 900) ('ultra-bold 900)
    ('black 950)))

(defun engrave-faces-html--face-apply (faces content)
  (let* ((attrs (engrave-faces-merge-attributes faces))
         (style (engrave-faces-html--gen-style-css attrs " ")))
    (if (string= style "")
        content
      (concat "<span style=\"" style "\">" content "</span>"))))

(defun engrave-faces-html--protect-string (str)
  (replace-regexp-in-string
   "<" "&lt;"
   (replace-regexp-in-string
    ">" "&gt;"
    (replace-regexp-in-string
     "&" "&amp;"
     str))))

(defun engrave-faces-html--face-mapper (faces content)
  "Create a HTML representation of CONTENT With FACES applied."
  (let ((protected-content (engrave-faces-html--protect-string content))
        (style (engrave-faces-preset-style faces)))
    (if (string-match-p "\\`[\n[:space:]]+\\'" content)
        protected-content
      (if (and style (eq engrave-faces-html-output-style 'preset))
          (concat "<span class=\"" engrave-faces-html-class-prefix
                  (plist-get (cdr style) :slug) "\">"
                  protected-content "</span>")
        (engrave-faces-html--face-apply faces protected-content)))))

(defun engrave-faces-html--make-standalone ()
  "Export current buffer to a standalone HTML buffer."
  (goto-char (point-min))
  (insert "<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"utf-8\">
    <title>"
          (engrave-faces-html--protect-string (if (buffer-file-name)
                                                 (file-name-nondirectory (buffer-file-name))
                                               (buffer-name)))
          "</title>
    <style>"
          (let* ((default-sty (cdr (assoc 'default engrave-faces-current-preset-style)))
                 (default-bg (plist-get default-sty :background))
                 (default-fg (plist-get default-sty :foreground)))
            (if (or default-bg default-fg)
                (concat "\n      body {"
                        (when default-bg (format " background: %s;" default-bg))
                        (when default-fg (format " color: %s;" default-fg))
                        " }")
              ""))
          "
      pre {
        font-size: 1rem;
        max-width: min(100rem, 100%);
        width: max-content;
        white-space: pre-wrap;
        margin: auto; }\n"
          (engrave-faces-html-gen-stylesheet "      ")
          "
    </style>
  </head>
  <body>
<pre>\n")
  (goto-char (point-max))
  (insert "
</pre>
  <body>
</html>"))

;;;###autoload (autoload #'engrave-faces-html-buffer "engrave-faces-html" nil t)
;;;###autoload (autoload #'engrave-faces-html-buffer-standalone "engrave-faces-html" nil t)
;;;###autoload (autoload #'engrave-faces-html-file "engrave-faces-html" nil t)
(engrave-faces-define-backend "html" ".html" #'engrave-faces-html--face-mapper #'engrave-faces-html--make-standalone #'html-mode)

(provide 'engrave-faces-html)
;;; engrave-faces-html.el ends here
