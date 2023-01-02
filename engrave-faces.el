;;; engrave-faces.el --- Convert font-lock faces to other formats -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: TEC <https://git.tecosaur.net/>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: January 18, 2021
;; Modified: July 10, 2021
;; Version: 0.3.1
;; Keywords: faces
;; Homepage: https://github.com/tecosaur/engrave-faces
;; Package-Requires: ((emacs "27.1"))

;;; License:

;; This file is part of engrave-faces, which is not part of GNU Emacs.
;;
;; engrave-faces is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; engrave-faces is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with engrave-faces.  If not, see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;  Convert font-lock faces to other formats.

;;; Code:

(require 'map)
(eval-when-compile
  (require 'subr-x))

(defgroup engrave-faces nil
  "Export buffers with font-lock information to other formats."
  :group 'hypermedia
  :prefix "engrave-faces-")

(defcustom engrave-faces-attributes-of-interest
  '(:family :foreground :background :slant :weight :height :strike-through)
  "Attributes which sould be paid attention to."
  :type '(repeat symbol))

(defcustom engrave-faces-before-hook nil
  "Hook run before engraving a buffer.
The hook functions are run in the source buffer (not the resulting buffer)."
  :type 'hook)

(defcustom engrave-faces-after-hook nil
  "Hook run after engraving a buffer.
Unlike `engrave-faces-before-hook', these functions are run in the generated
buffer.  You may use them to modify the outlook of the final output."
  :type 'hook)

(defcustom engrave-faces-log-preset-missed-faces nil
  "Whether to log faces not found in `engrave-faces-current-preset-style'."
  :type 'boolean)

(define-obsolete-variable-alias 'engrave-faces-preset-styles 'engrave-faces-current-preset-style "0.3")

(defcustom engrave-faces-themes
  '((default .
      (;; faces.el --- excluding: bold, italic, bold-italic, underline, and some others
       (default                             :short "default"             :slug "D"   :foreground "#000000" :background "#ffffff" :family "Monospace")
       (variable-pitch                      :short "var-pitch"           :slug "vp"  :foreground "#000000"                       :family "Sans Serif")
       (shadow                              :short "shadow"              :slug "h"   :foreground "#7f7f7f")
       (success                             :short "success"             :slug "sc"  :foreground "#228b22" :weight bold)
       (warning                             :short "warning"             :slug "w"   :foreground "#ff8e00" :weight bold)
       (error                               :short "error"               :slug "e"   :foreground "#ff0000" :weight bold)
       (link                                :short "link"                :slug "l"   :foreground "#ff0000")
       (link-visited                        :short "link"                :slug "lv"  :foreground "#ff0000")
       (highlight                           :short "link"                :slug "hi"  :foreground "#ff0000")
       ;; font-lock.el
       (font-lock-comment-face              :short "fl-comment"          :slug "c"   :foreground "#b22222")
       (font-lock-comment-delimiter-face    :short "fl-comment-delim"    :slug "cd"  :foreground "#b22222")
       (font-lock-string-face               :short "fl-string"           :slug "s"   :foreground "#8b2252")
       (font-lock-doc-face                  :short "fl-doc"              :slug "d"   :foreground "#8b2252")
       (font-lock-doc-markup-face           :short "fl-doc-markup"       :slug "m"   :foreground "#008b8b")
       (font-lock-keyword-face              :short "fl-keyword"          :slug "k"   :foreground "#9370db")
       (font-lock-builtin-face              :short "fl-builtin"          :slug "b"   :foreground "#483d8b")
       (font-lock-function-name-face        :short "fl-function"         :slug "f"   :foreground "#0000ff")
       (font-lock-variable-name-face        :short "fl-variable"         :slug "v"   :foreground "#a0522d")
       (font-lock-type-face                 :short "fl-type"             :slug "t"   :foreground "#228b22")
       (font-lock-constant-face             :short "fl-constant"         :slug "o"   :foreground "#008b8b")
       (font-lock-warning-face              :short "fl-warning"          :slug "wr"  :foreground "#ff0000" :weight bold)
       (font-lock-negation-char-face        :short "fl-neg-char"         :slug "nc")
       (font-lock-preprocessor-face         :short "fl-preprocessor"     :slug "pp"  :foreground "#483d8b")
       (font-lock-regexp-grouping-construct :short "fl-regexp"           :slug "rc"                        :weight bold)
       (font-lock-regexp-grouping-backslash :short "fl-regexp-backslash" :slug "rb"                        :weight bold)
       ;; org-faces.el
       (org-block                           :short "org-block"           :slug "ob") ; forcing no background is preferable
       (org-block-begin-line                :short "org-block-begin"     :slug "obb") ; forcing no background is preferable
       (org-block-end-line                  :short "org-block-end"       :slug "obe") ; forcing no background is preferable
       ;; outlines
       (outline-1                           :short "outline-1"           :slug "Oa"  :foreground "#0000ff")
       (outline-2                           :short "outline-2"           :slug "Ob"  :foreground "#a0522d")
       (outline-3                           :short "outline-3"           :slug "Oc"  :foreground "#a020f0")
       (outline-4                           :short "outline-4"           :slug "Od"  :foreground "#b22222")
       (outline-5                           :short "outline-5"           :slug "Oe"  :foreground "#228b22")
       (outline-6                           :short "outline-6"           :slug "Of"  :foreground "#008b8b")
       (outline-7                           :short "outline-7"           :slug "Og"  :foreground "#483d8b")
       (outline-8                           :short "outline-8"           :slug "Oh"  :foreground "#8b2252")
       ;; highlight-numbers.el
       (highlight-numbers-number            :short "hl-number"           :slug "hn"  :foreground "#008b8b")
       ;; highlight-quoted.el
       (highlight-quoted-quote              :short "hl-qquote"           :slug "hq"  :foreground "#9370db")
       (highlight-quoted-symbol             :short "hl-qsymbol"          :slug "hs"  :foreground "#008b8b")
       ;; rainbow-delimiters.el
       (rainbow-delimiters-depth-1-face     :short "rd-1"                :slug "rda" :foreground "#707183")
       (rainbow-delimiters-depth-2-face     :short "rd-2"                :slug "rdb" :foreground "#7388d6")
       (rainbow-delimiters-depth-3-face     :short "rd-3"                :slug "rdc" :foreground "#909183")
       (rainbow-delimiters-depth-4-face     :short "rd-4"                :slug "rdd" :foreground "#709870")
       (rainbow-delimiters-depth-5-face     :short "rd-5"                :slug "rde" :foreground "#907373")
       (rainbow-delimiters-depth-6-face     :short "rd-6"                :slug "rdf" :foreground "#6276ba")
       (rainbow-delimiters-depth-7-face     :short "rd-7"                :slug "rdg" :foreground "#858580")
       (rainbow-delimiters-depth-8-face     :short "rd-8"                :slug "rdh" :foreground "#80a880")
       (rainbow-delimiters-depth-9-face     :short "rd-9"                :slug "rdi" :foreground "#887070"))))
  "A collection of named style presets.

This takes the form of an alist with theme names as the cars, with
cdrs in the form of `engrave-faces-current-preset-style'."
  :type '(alist
          :key-type (symbol :tag "Theme name")
          :value-type
          (repeat
           (cons (symbol :tag "Face")
                 (plist :key-type (choice
                                   (const :tag "Short identifier" :short)
                                   (const :tag "Very short identifier" :slug)
                                   (symbol :tag "Face attribute")
                                   :tag "Property")
                        :value-type (choice :tag "Value" string symbol)
                        :tag "Face specification")))))

(defcustom engrave-faces-current-preset-style
  (alist-get 'default engrave-faces-themes)
  "Overriding face values.

This is constructed as an alist of faces, and their face attributes as a plist.
For example, the \"default\" face could be specified by:

  (default :foreground \"#000000\" :background \"#FFFFFF\")

By setting :foreground, :background, etc. a certain theme can be
set for the faces. The face attributes here will also be used
when calculating inherited styles. Note that colours must be
given in hexadecimal form.

Faces here will represented more compactly when possible, by using the
:short or :slug parameter to produce a named version styles,
- :short should be a descriptive string comprised of the character class
  [A-Za-z0-9-_]
- :slug should be a compact string (i.e. as short as possible), comprised of the
  character class [A-Za-Z]

For example, for the \"default\" face,

  (default :short \"def\" :slug \"D\"
           :foreground \"#000000\" :background \"#FFFFFF\")

Other faces will need to be styled explicitly each time they are used."
  :type '(repeat
          (cons (symbol :tag "Face")
                (plist :key-type (choice
                                  (const :tag "Short identifier" :short)
                                  (const :tag "Very short identifier" :slug)
                                  (symbol :tag "Face attribute")
                                  :tag "Property")
                       :value-type (choice :tag "Value" string symbol)
                       :tag "Face specification"))))

(defvar engrave-faces-preset-missed-faces nil
  "Faces not found in `engrave-faces-current-preset-style'.")

(defvar engrave-faces--backends nil)

;;;###autoload
(defmacro engrave-faces-define-backend (backend extension face-transformer &optional standalone-transformer view-setup)
  "Create a new engraving backend BACKEND.
EXTENSION is the extension which will be used when writing
engraved files. FACE-TRANSFORMER is the all important function
which can be called with a list of faces and some content to
apply those faces to and generate an output string accordingly.

Should a pre/postable make sense for complete files using
BACKEND, a STANDALONE-TRANSFORMER may be defined which operates
on a buffer which has been generated by `engrave-faces-buffer'
and is called after hooks.

If STANDALONE-TRANSFORMER is given it will be used when directly
creating a file, and cause a -standalone version of the buffer
transforming function to be created."
  `(progn (add-to-list 'engrave-faces--backends
                       (list ,backend :face-transformer ,face-transformer :extension ,extension))
          (defun ,(intern (concat "engrave-faces-" backend "-buffer")) (&optional theme switch-to-result)
            ,(concat "Convert buffer to " backend " formatting.")
            (interactive '(nil t))
            (let ((buf (engrave-faces-buffer ,backend theme)))
              (when switch-to-result
                (switch-to-buffer buf)
                ,(when view-setup `(funcall ,view-setup)))
              buf))
          ,(when standalone-transformer
             `(defun ,(intern (concat "engrave-faces-" backend "-buffer-standalone")) (&optional theme switch-to-result)
                ,(concat "Export the current buffer to a standalone " backend " buffer.")
                (interactive '(nil t))
                (let ((buf (engrave-faces-buffer ,backend theme)))
                  (with-current-buffer buf
                    (funcall ,standalone-transformer))
                  (when switch-to-result
                    (switch-to-buffer buf)
                    ,(when view-setup `(funcall ,view-setup)))
                  buf)))
          (defun ,(intern (concat "engrave-faces-" backend "-file")) (file &optional out-file theme open-result)
            ,(concat "Convert file to " backend " formatting.")
            (interactive (list buffer-file-name nil nil t))
            (unless out-file
              (setq out-file (concat file ,extension)))
            (engrave-faces-file file out-file ,backend theme ,standalone-transformer)
            (when open-result (find-file out-file))
            out-file)
          (defvar ,(intern (concat "engrave-faces-" backend "-before-hook")) nil)
          (defvar ,(intern (concat "engrave-faces-" backend "-after-hook")) nil)))

(defun engrave-faces-file (in-file out-file backend &optional theme postprocessor)
  "Using BACKEND, engrave IN-FILE and save it as FILE.EXTENSION.
If a POSTPROCESSOR function is provided, it is called before saving."
  (with-temp-buffer
    (insert-file-contents in-file)
    (let ((buffer-file-name in-file))
      (normal-mode)
      (with-current-buffer (engrave-faces-buffer backend theme)
        (when postprocessor (funcall postprocessor))
        (write-region (point-min) (point-max) out-file)
        (kill-buffer)))))

(defun engrave-faces-buffer (backend &optional theme)
  "Export the current buffer with BACKEND and return the created buffer."
  (let ((engrave-faces-current-preset-style
         (if theme
             (engrave-faces-get-theme theme)
           engrave-faces-current-preset-style)))
    (save-excursion
      ;; Protect against the hook changing the current buffer.
      (save-excursion
        (run-hooks 'engrave-faces-before-hook)
        (run-hooks (intern (concat "engrave-faces-" backend "-before-hook"))))
      ;; Convince font-lock support modes to fontify the entire buffer
      ;; in advance.
      (when (and (boundp 'jit-lock-mode)
                 (symbol-value 'jit-lock-mode))
        (jit-lock-fontify-now (point-min) (point-max)))
      (font-lock-ensure)
      ;; It's important that the new buffer inherits default-directory
      ;; from the current buffer.
      (let ((engraved-buf
             (generate-new-buffer
              (if (buffer-file-name)
                  (concat (file-name-nondirectory (buffer-file-name))
                          (plist-get (cdr (assoc backend engrave-faces--backends)) :extension))
                (concat "*" backend "*"))))
            (face-transformer (plist-get (cdr (assoc backend engrave-faces--backends)) :face-transformer))

            (completed nil))
        (unwind-protect
            (let (next-change text)
              ;; This loop traverses and reads the source buffer, appending the
              ;; resulting text to the export buffer. This method is fast because:
              ;; 1) it doesn't require examining the text properties char by char
              ;; (engrave-faces--next-face-change is used to move between runs with
              ;; the same face), and 2) it doesn't require frequent buffer
              ;; switches, which are slow because they rebind all buffer-local
              ;; vars.
              (goto-char (point-min))
              (while (not (eobp))
                (setq next-change (engrave-faces--next-face-change (point)))
                (setq text (buffer-substring-no-properties (point) next-change))
                ;; Don't bother writing anything if there's no text (this
                ;; happens in invisible regions).
                (when (> (length text) 0)
                  (princ (funcall face-transformer
                                  (let ((prop (get-text-property (point) 'face)))
                                    (cond
                                     ((null prop) 'default)
                                     ;; FIXME: Why/where/when does the `face'
                                     ;; property take a value (quote X)?
                                     ((and (listp prop) (eq (car prop) 'quote))
                                      (eval prop t))
                                     ((and (consp prop) (keywordp (car prop)))
                                      (list prop))
                                     (t prop)))
                                  text)
                         engraved-buf))
                (goto-char next-change)))
          (setq completed t))
        (if (not completed)
            (kill-buffer engraved-buf)
          (with-current-buffer engraved-buf
            (run-hooks 'engrave-faces-after-hook)
            (run-hooks (intern (concat "engrave-faces-" backend "-after-hook"))))
          engraved-buf)))))

(defun engrave-faces-merge-attributes (faces &optional attributes)
  "Find the final ATTRIBUTES for text with FACES."
  (setq faces (engrave-faces-explicit-inheritance (if (listp faces) faces (list faces))))
  (mapcan (lambda (attr)
            (list attr (car (engrave-faces-attribute-values faces attr))))
          (or attributes engrave-faces-attributes-of-interest)))

(defun engrave-faces-explicit-inheritance (faces)
  "Expand :inherit for each face in FACES.
I.e. ([facea :inherit faceb] facec) results in (facea faceb facec)"
  (delq nil
        (mapcan
         (lambda (face)
           (if (listp face)
               (let ((inherit (plist-get face :inherit)))
                 (cons (map-delete face :inherit)
                       (engrave-faces-explicit-inheritance inherit)))
             (cons face
                   (let ((inherit (face-attribute face :inherit nil nil)))
                     (when (and inherit (not (eq inherit 'unspecified)))
                       (engrave-faces-explicit-inheritance inherit))))))
         (if (listp faces) faces (list faces)))))

(defun engrave-faces-attribute-values (faces attribute)
  "Fetch all specified instances of ATTRIBUTE for FACES, ignoring inheritence.
To consider inheritence, use `engrave-faces-explicit-inheritance' first."
  (delq nil (delq 'unspecified
                  (mapcar
                   (lambda (face)
                     (if-let ((style (cdr (assoc face engrave-faces-current-preset-style))))
                         (plist-get style attribute)
                       (cond
                        ((symbolp face)
                         (when engrave-faces-log-preset-missed-faces
                           (push face engrave-faces-preset-missed-faces))
                         (face-attribute face attribute nil nil))
                        ((listp face) (plist-get face attribute)))))
                   (delq 'default (if (listp faces) faces (list faces)))))))

(defun engrave-faces--next-face-change (pos &optional limit)
  "Find the next face change from POS up to LIMIT.

This function is lifted from htmlize."
  ;; (engrave-faces-next-change pos 'face limit) would skip over entire
  ;; overlays that specify the `face' property, even when they
  ;; contain smaller text properties that also specify `face'.
  ;; Emacs display engine merges those faces, and so must we.
  (unless limit
    (setq limit (point-max)))
  (let ((next-prop (next-single-property-change pos 'face nil limit))
        (overlay-faces (engrave-faces--overlay-faces-at pos)))
    (while (progn
             (setq pos (next-overlay-change pos))
             (and (< pos next-prop)
                  (equal overlay-faces (engrave-faces--overlay-faces-at pos)))))
    (setq pos (min pos next-prop))
    ;; Additionally, we include the entire region that specifies the
    ;; `display' property.
    (when (get-char-property pos 'display)
      (setq pos (next-single-char-property-change pos 'display nil limit)))
    pos))

(defun engrave-faces--overlay-faces-at (pos)
  (delq nil (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at pos))))

;;; Style helpers

(defun engrave-faces--check-nondefault (attr value)
  "Return VALUE as long as it is specified, and not the default for ATTR."
  (unless (or (eq value (face-attribute 'default attr nil t))
              (eq value 'unspecified))
    value))

(defun engrave-faces-preset-style (faces)
  "Return the preset style for FACES, should it exist.
Unconditionally returns nil when FACES is default."
  (pcase faces
    ('default nil)
    ((pred symbolp) (assoc faces engrave-faces-preset-styles))
    ((and (pred listp) (app length 1)) (assoc (car faces) engrave-faces-preset-styles))))

(defun engrave-faces-generate-preset ()
  "Generate a preset style based on the current Emacs theme."
  (mapcar
   (lambda (face-style)
     (apply #'append
            (list (car face-style)
                  :short (plist-get (cdr face-style) :short)
                  :slug (plist-get (cdr face-style) :slug))
            (delq nil
                  (mapcar
                   (lambda (attr)
                     (when-let ((attr-val (when (facep (car face-style))
                                            (face-attribute (car face-style) attr nil t))))
                       (when (or (engrave-faces--check-nondefault attr attr-val)
                                 (and (eq (car face-style) 'default)
                                      (not (memq attr '(:height :strike-through)))))
                         (list attr
                               (if (and (memq attr '(:foreground :background))
                                        (not (string-prefix-p "#" attr-val)))
                                   (apply 'format "#%02x%02x%02x"
                                          (mapcar (lambda (c) (ash c -8))
                                                  (color-values attr-val)))
                                 attr-val)))))
                   engrave-faces-attributes-of-interest))))
   engrave-faces-preset-styles))

(defun engrave-faces-get-theme (theme &optional noput)
  "Obtain the preset style for THEME.
Unless NOPUT is non-nil, the preset will be added to `engrave-faces-themes'.
The theme t is treated as shorthand for the current theme."
  (when (eq theme t)
    (setq theme (car custom-enabled-themes)))
  (if-let ((theme-preset (alist-get theme engrave-faces-themes)))
      theme-preset
    (if (or (eq theme (car custom-enabled-themes))
            (memq theme (custom-available-themes)))
        (let ((spec
               (if (eq theme (car custom-enabled-themes))
                   (engrave-faces-generate-preset)
                 (let ((old-theme (car custom-enabled-themes))
                       spec)
                   (load-theme theme t)
                   (setq spec (engrave-faces-generate-preset))
                   (load-theme old-theme t)
                   (redraw-display)
                   spec))))
          (unless noput
            (push (cons theme spec) engrave-faces-themes))
          spec)
      (user-error "Theme `%s' is not found in `engrave-faces-current-preset-style' or availible Emacs themes." theme))))

(defun engrave-faces-use-theme (&optional theme insert-def)
  "Select a THEME an apply it as the current engraved preset style.
When INSERT-DEF is non-nil, or the universal argument has been provided, an
expression adding THEME to `engrave-faces-themes' shall be inserted into the
current buffer at point."
  (interactive (list (intern
                      (completing-read
                       "Theme: "
                       (cl-remove-duplicates
                        (append
                         (mapcar
                          (lambda (theme)
                            (propertize (symbol-name theme) 'face '(italic font-lock-doc-face)))
                          (custom-available-themes))
                         (list (propertize (symbol-name (car custom-enabled-themes))
                                           'face '(bold font-lock-comment-face)))
                         (mapcar #'car engrave-faces-themes)))))
                     (when current-prefix-arg t)))
  (unless theme
    (setq theme (car custom-enabled-themes)))
  (let ((spec (engrave-faces-get-theme theme)))
    (if insert-def
        (engrave-faces--insert-theme-def theme spec)
      (setq engrave-faces-current-preset-style spec))))

(defun engrave-faces--insert-theme-def (name &optional spec)
  "Insert a definition for the theme NAME with a certain SPEC into the buffer."
  (insert (pp
           `(add-to-list
             'engrave-faces-themes
             ',(cons name (or spec
                              (engrave-faces-get-theme name)))))))

(provide 'engrave-faces)
;;; engrave-faces.el ends here
