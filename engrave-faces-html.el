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

(defun engrave-faces-html-gen-stylesheet (&optional indent)
  "Generate a preamble which provides short commands for the preset styles.
See `engrave-faces-preset-styles' and `engrave-faces-html-output-style'."
  (let ((stylesheet
         (mapconcat
          (lambda (face-style)
            (engrave-faces-html-gen-stylesheet-entry (car face-style) (cdr face-style)))
          engrave-faces-preset-styles
          "\n")))
    (if indent
        (mapconcat (lambda (line)
                     (concat indent line))
                   (split-string stylesheet "\n")
                   "\n")
      stylesheet)))

(defun engrave-faces-html-gen-stylesheet-entry (face style)
  "Generate a HTML preamble line for STYLE representing FACE."
  (concat "." engrave-faces-html-class-prefix (plist-get style :slug)
          " {\n  "
          (engrave-faces-html-gen-style-css style "\n  ")
          " }"))

(defun engrave-faces-html-gen-style-css (attrs seperator)
  "Compose the relevant CSS styles to apply compatible ATTRS, seperated by SEPERATOR."
  (let ((fg    (plist-get attrs      :foreground))
        (bg    (plist-get attrs      :background))
        (st    (plist-get attrs      :strike-through))
        (ul    (plist-get attrs      :underline))
        (it    (eql (plist-get attrs :slant) 'italic))
        (wt    (plist-get attrs      :weight)))
    (mapconcat
     #'identity
     (delq nil
           (list
            (when fg (format "color: %s;" fg))
            (when bg (format "background-color: %s;" bg))
            (when st "text-decoration: line-through;")
            (when ul "text-decoration: underline;")
            (when it "text-decoration: italic;")
            (when wt (format "font-weight: %s;" wt))))
     seperator)))

(defun engrave-faces-html-face-apply (faces content)
  (let ((attrs (engrave-faces-merge-attributes faces)))
    (concat "<span style=\"" (engrave-faces-html-gen-style-css attrs " ") "\">"
            content "</span>")))

(defun engrave-faces-html-protect-string (str)
  (replace-regexp-in-string
   "<" "&lt;"
   (replace-regexp-in-string
    ">" "&gt;"
    (replace-regexp-in-string
     "&" "&amp;"
     str))))

(defun engrave-faces-html-face-mapper (faces content)
  "Create a HTML representation of CONTENT With FACES applied."
  (let ((protected-content (engrave-faces-html-protect-string content))
        (style (unless (eq faces 'default) (assoc faces engrave-faces-preset-styles))))
    (if (string-match-p "\\`[\n[:space:]]+\\'" content)
        protected-content
      (if (and style (eq engrave-faces-html-output-style 'preset))
          (concat "<span class=\"" engrave-faces-html-class-prefix
                  (plist-get (cdr style) :slug) "\">"
                  protected-content "</span>")
        (engrave-faces-html-face-apply faces protected-content)))))

(defun engrave-faces-html-make-standalone ()
  "Export current buffer to a standalone HTML buffer."
  (goto-char (point-min))
  (insert "<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"utf-8\">
    <title>"
          (engrave-faces-html-protect-string (if (buffer-file-name)
                                                 (file-name-nondirectory (buffer-file-name))
                                               (buffer-name)))
          "</title>
    <style>"
          (if-let ((default-bg (plist-get (cdr (assoc 'default engrave-faces-preset-styles)) :background)))
              (format "\n      body { background: %s }" default-bg)
            "")
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

;;;###autoload
(engrave-faces-define-backend "html" ".html" #'engrave-faces-html-face-mapper #'engrave-faces-html-make-standalone #'html-mode)

(provide 'engrave-faces-html)
;;; engrave-faces-html.el ends here
