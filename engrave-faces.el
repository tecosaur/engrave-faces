;;; engrave-faces.el --- Convert font-lock faces to other formats -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TEC

;; Author: TEC <https://github/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: January 18, 2021
;; Modified: January 18, 2021
;; Version: 0.0.1
;; Keywords: faces
;; Homepage: https://github.com/tec/engrave-faces
;; Package-Requires: ((emacs "27.1"))

;;; License:

;; This file is part of org-pandoc-import, which is not part of GNU Emacs.
;;
;; org-pandoc-import is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; org-pandoc-import is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with org-pandoc-import.  If not, see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;  Convert font-lock faces to other formats.

;;; Code:

;;;###autoload
(defvar engrave-faces--backends nil)
;;;###autoload
(defmacro engrave-faces-define-backend (name extension face-transformer)
  `(progn (add-to-list 'engrave-faces--backends
                       (list ,name :face-transformer ,face-transformer :extension ,extension))
          (defun ,(intern (concat "engrave-faces-" name "-buffer")) ()
            (concat "Convert buffer to " ,name " formatting")
            (engrave-faces-buffer-1 ,name))
          (defvar ,(intern (concat "engrave-faces-" name "-before-hook")) nil)
          (defvar ,(intern (concat "engrave-faces-" name "-after-hook")) nil)))

(defun engrave-faces-region-for-paste (beg end)
  "Convert the region between BEG and END to ANSI."
  (let ((engraved-buf (save-restriction
                        (narrow-to-region beg end)
                        (engrave-faces-buffer-1))))
    (unwind-protect
        (with-current-buffer engraved-buf
          (buffer-string))
      (kill-buffer engraved-buf))))

(defvar engrave-faces-attributes-of-interest
  '(:foreground :background :slant :weight :height)
  "Attributes which sould be paid attention to.")

(defvar engrave-faces-before-hook nil
  "Hook run before htmlizing a buffer.
The hook functions are run in the source buffer (not the resulting HTML
buffer).")

(defvar engrave-faces-after-hook nil
  "Hook run after htmlizing a buffer.
Unlike `engrave-faces-before-hook', these functions are run in the generated
HTML buffer.  You may use them to modify the outlook of the final HTML
output.")

(defun engrave-faces-buffer-1 (backend)
  ;; Internal function; don't call it from outside this file.  Ansify
  ;; current buffer, writing the resulting ANSI to a new buffer, and
  ;; return it.
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
    (let ((engraved-buf (generate-new-buffer (if (buffer-file-name)
                                                 (concat (file-name-nondirectory (buffer-file-name))
                                                         (plist-get (cdr (assoc backend engrave-faces--backends)) :extension))
                                               (concat "*" backend "*"))))
          (face-transformer (plist-get (cdr (assoc backend engrave-faces--backends)) :face-transformer))
          (completed nil))
      (unwind-protect
          (let (next-change text)
            ;; This loop traverses and reads the source buffer, appending
            ;; the resulting ANSI to ANSIBUF.  This method is fast
            ;; because: 1) it doesn't require examining the text
            ;; properties char by char (engrave-faces-next-face-change is used
            ;; to move between runs with the same face), and 2) it doesn't
            ;; require frequent buffer switches, which are slow because
            ;; they rebind all buffer-local vars.
            (goto-char (point-min))
            (while (not (eobp))
              (setq next-change (engrave-faces-next-face-change (point)))
              (setq text (buffer-substring-no-properties (point) next-change))
              ;; Don't bother writing anything if there's no text (this
              ;; happens in invisible regions).
              (when (> (length text) 0)
                (princ (funcall face-transformer
                                (or (get-text-property (point) 'face)
                                    'default)
                                text)
                       engraved-buf))
              (goto-char next-change)))
        (setq completed t))
      (if (not completed)
          (kill-buffer engraved-buf)
        (with-current-buffer engraved-buf
          (run-hooks 'engrave-faces-after-hook)
          (run-hooks (intern (concat "engrave-faces-" backend "-after-hook"))))
        engraved-buf))))

(defun engrave-faces-merge-attributes (faces)
  (apply #'append
         (mapcar (lambda (attr)
                   (list attr
                         (car
                          (delq nil
                                (delq 'unspecified
                                      (mapcar (lambda (face)
                                                (face-attribute face attr nil t))
                                              (delq 'default (if (listp faces) faces (list faces)))))))))
                 engrave-faces-attributes-of-interest)))

(defun engrave-faces-next-face-change (pos &optional limit)
  ;; (engrave-faces-next-change pos 'face limit) would skip over entire
  ;; overlays that specify the `face' property, even when they
  ;; contain smaller text properties that also specify `face'.
  ;; Emacs display engine merges those faces, and so must we.
  (or limit
      (setq limit (point-max)))
  (let ((next-prop (next-single-property-change pos 'face nil limit))
        (overlay-faces (engrave-faces-overlay-faces-at pos)))
    (while (progn
             (setq pos (next-overlay-change pos))
             (and (< pos next-prop)
                  (equal overlay-faces (engrave-faces-overlay-faces-at pos)))))
    (setq pos (min pos next-prop))
    ;; Additionally, we include the entire region that specifies the
    ;; `display' property.
    (when (get-char-property pos 'display)
      (setq pos (next-single-char-property-change pos 'display nil limit)))
    pos))

(defun engrave-faces-overlay-faces-at (pos)
  (delq nil (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at pos))))

;;; Style helpers

(defvar engrave-faces-preset-styles ; doom-one-light
  '((font-lock-keyword-face              :short "keyword"          :slug "k"     :foreground "#e45649")
    (font-lock-doc-face                  :short "doc"              :slug "d"     :foreground "#84888b" :slant italic)
    (font-lock-type-face                 :short "type"             :slug "t"     :foreground "#986801")
    (font-lock-string-face               :short "string"           :slug "s"     :foreground "#50a14f")
    (font-lock-warning-face              :short "warning"          :slug "w"     :foreground "#986801")
    (font-lock-builtin-face              :short "builtin"          :slug "b"     :foreground "#a626a4")
    (font-lock-comment-face              :short "comment"          :slug "ct"    :foreground "#9ca0a4")
    (font-lock-constant-face             :short "constant"         :slug "c"     :foreground "#b751b6")
    (font-lock-preprocessor-face         :short "preprocessor"     :slug "pp"    :foreground "#4078f2" :weight bold)
    (font-lock-negation-char-face        :short "neg-char"         :slug "nc"    :foreground "#4078f2" :weight bold)
    (font-lock-variable-name-face        :short "variable"         :slug "v"     :foreground "#6a1868")
    (font-lock-function-name-face        :short "function"         :slug "f"     :foreground "#a626a4")
    (font-lock-comment-delimiter-face    :short "comment-delim"    :slug "cd"    :foreground "#9ca0a4")
    (font-lock-regexp-grouping-construct :short "regexp"           :slug "rc"    :foreground "#4078f2" :weight bold)
    (font-lock-regexp-grouping-backslash :short "regexp-backslash" :slug "rb"    :foreground "#4078f2" :weight bold)
    (highlight-numbers-number            :short "number"           :slug "hn"    :foreground "#da8548" :weight bold)
    (highlight-quoted-quote              :short "qquote"           :slug "hq"    :foreground "#4078f2")
    (highlight-quoted-symbol             :short "qsymbol"          :slug "hs"    :foreground "#986801")
    (rainbow-delimiters-depth-1-face     :short "rd1"              :slug "rdi"   :foreground "#4078f2")
    (rainbow-delimiters-depth-2-face     :short "rd2"              :slug "rdii"  :foreground "#a626a4")
    (rainbow-delimiters-depth-3-face     :short "rd3"              :slug "rdiii" :foreground "#50a14f")
    (rainbow-delimiters-depth-4-face     :short "rd4"              :slug "rdiv"  :foreground "#da8548")
    (rainbow-delimiters-depth-5-face     :short "rd5"              :slug "rdv"   :foreground "#b751b6")
    (rainbow-delimiters-depth-6-face     :short "rd6"              :slug "rdvi"  :foreground "#986801")
    (rainbow-delimiters-depth-7-face     :short "rd7"              :slug "rdvii" :foreground "#4db5bd")
    (rainbow-delimiters-depth-8-face     :short "rd8"              :slug "rdiix" :foreground "#80a880")
    (rainbow-delimiters-depth-9-face     :short "rd9"              :slug "rdix"  :foreground "#887070"))
  "TODO")

(defvar engrave-faces-preset-default '(:foreground "#383a42")
  "TODO")

(defun engrave-faces-check-nondefault (attr value)
  (unless (or (eq value (face-attribute 'default attr nil t))
              (eq value 'unspecified))
    value))

(defun engrave-faces-generate-preset ()
  "Generate `engrave-faces-preset-styles' based on the current theme."
  (mapcar
   (lambda (face-style)
     (apply #'append
            (list (car face-style)
                  :short (plist-get (cdr face-style) :short)
                  :slug (plist-get (cdr face-style) :slug))
            (delq nil
                  (mapcar
                   (lambda (attr)
                     (let ((attr-val (face-attribute (car face-style) attr nil t)))
                       (when (engrave-faces-check-nondefault attr attr-val)
                         (list attr attr-val))))
                   engrave-faces-attributes-of-interest))))
   engrave-faces-preset-styles))

(provide 'engrave-faces)
;;; engrave-faces.el ends here
