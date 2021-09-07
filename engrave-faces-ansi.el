;;; engrave-faces-ansi.el --- Support for engraving buffers to LaTeX -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is part of engrave-faces.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Support for engraving buffers to LaTeX.

;;; Code:

(require 'engrave-faces)

(defcustom engrave-faces-ansi-color-mode '8-bit
  "The ansi escape mode set to use.
This accepts both n-bit and m-color forms.
Possible values are:
- `3-bit'  (`8-color')
- `4-bit'  (`16-color')
- `8-bit'  (`256-color')
- `24-bit' (`16m-color')"
  :type '(choice
          (const 3-bit)
          (const 4-bit)
          (const 8-bit)
          (const 24-bit))
  :group 'engrave-faces)

(defcustom engrave-faces-ansi-use-face-colours t
  "Whether to apply face colours"
  :group 'engrave-faces)

(defvar engrave-faces-ansi-face-nesting nil)

(defun engrave-faces-ansi-code (attrs)
  "Genrerate ANSI commands which apply ATTRS to the succeeding text."
  (concat
   (when (member (plist-get attrs :weight) '(bold extra-bold)) "\uE000[1m")
   (when (eq 'italic (plist-get attrs :slant)) "\uE000[3m")
   (when (eq t (plist-get attrs :underline)) "\uE000[4m")
   (when (and engrave-faces-ansi-use-face-colours
              (plist-get attrs :foreground))
     (engrave-faces-ansi--color-to-ansi
      (plist-get attrs :foreground)))
   (when (and engrave-faces-ansi-use-face-colours
              (plist-get attrs :background))
     (engrave-faces-ansi--color-to-ansi
      (plist-get attrs :background) t))))

;;;;; Color conversion

(defun engrave-faces-ansi--color-to-ansi (color &optional background)
  (if (eq color 'unspecified) nil
    (apply (pcase engrave-faces-ansi-color-mode
             ((or '3-bit '8-color) #'engrave-faces-ansi-color-3bit-code)
             ((or '4-bit '16-color) #'engrave-faces-ansi-color-4bit-code)
             ((or '8-bit '256-color) #'engrave-faces-ansi--color-8bit-code)
             ((or '24-bit '16m-color) #'engrave-faces-ansi-color-24bit-code))
           (append (mapcar (lambda (c) (/ c 257)) (color-values color)) (list background)))))

(defun engrave-faces-ansi--color-dist-squared (reference rgb)
  "Squared l2 distance between a REFERENCE and RBG values, each a list of 3 values (r g b)."
  (+ (* (nth 0 reference)
        (nth 0 rgb))
     (* (nth 1 reference)
        (nth 1 rgb))
     (* (nth 2 reference)
        (nth 2 rgb))))

;;;;;; 4-bit / 16-color

(defvar engrave-faces-ansi--256-to-16-map
  '(0   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
        0   4  4  4 12 12  2  6  4  4 12 12  2  2  6  4
        12 12  2  2  2  6 12 12 10 10 10 10 14 12 10 10
        10 10 10 14  1  5  4  4 12 12  3  8  4  4 12 12
        2   2  6  4 12 12  2  2  2  6 12 12 10 10 10 10
        14 12 10 10 10 10 10 14  1  1  5  4 12 12  1  1
        5   4 12 12  3  3  8  4 12 12  2  2  2  6 12 12
        10 10 10 10 14 12 10 10 10 10 10 14  1  1  1  5
        12 12  1  1  1  5 12 12  1  1  1  5 12 12  3  3
        3   7 12 12 10 10 10 10 14 12 10 10 10 10 10 14
        9   9  9  9 13 12  9  9  9  9 13 12  9  9  9  9
        13 12  9  9  9  9 13 12 11 11 11 11  7 12 10 10
        10 10 10 14  9  9  9  9  9 13  9  9  9  9  9 13
        9   9  9  9  9 13  9  9  9  9  9 13  9  9  9  9
        9  13 11 11 11 11 11 15  0  0  0  0  0  0  8  8
        8   8  8  8  7  7  7  7  7  7 15 15 15 15 15 15))

(defun engrave-faces-ansi-color-4bit-code (r g b &optional background)
  "Convert the (R G B) colour code to a correspanding 4bit ansi escape sequence."
  (format "\uE000[%sm"
          (pcase (nth (engrave-faces-ansi-color-rbg-to-256 r g b)
                      engrave-faces-ansi--256-to-16-map)
            ((and (pred (> 8)) n)
             (+ 30 (if background 10 0) n))
            (n (+ 82 (if background 10 0) n)))))

;;;;;; 3-bit / 8-color

(defun engrave-faces-ansi-color-3bit-code (r g b &optional background)
  "Convert the (R G B) colour code to a correspanding 3bit ansi escape sequence.
Brighter colours are induced via the addition of a bold code."
  (format "\uE000[%sm"
          (pcase (nth (engrave-faces-ansi-color-rbg-to-256 r g b)
                      engrave-faces-ansi--256-to-16-map)
            ((and (pred (> 8)) n)
             (+ 30 (if background 10 0) n))
            (n (format "1;%d" (+ 22 (if background 10 0) n))))))

;;;;;; 8-bit / 256-color

(defvar engrave-faces-ansi--color-6cube-values '(0 95 135 175 215 255))
(defun engrave-faces-ansi--color-to-6cube (value)
  "Map VALUE to the associated 6x6 colour cube value."
  (pcase value
    ((pred (> 48)) 0)
    ((pred (> 114)) 1)
    (_ (/ (- value 35) 40))))

(defun engrave-faces-ansi--color-8bit-code (r g b &optional background)
  "Convert the (R G B) colour code to a correspanding 8bit ansi escape sequence."
  (format (if background "\uE000[48;5;%dm" "\uE000[38;5;%dm")
          (engrave-faces-ansi-color-rbg-to-256 r g b)))

(defun engrave-faces-ansi-color-rbg-to-256 (r g b)
  "Convert the (R G B) colour code to the nearest 256-colour."
  (let ((6cube-r (engrave-faces-ansi--color-to-6cube r))
        (6cube-g (engrave-faces-ansi--color-to-6cube g))
        (6cube-b (engrave-faces-ansi--color-to-6cube b)))
    (let ((nearest-r (nth 6cube-r engrave-faces-ansi--color-6cube-values))
          (nearest-g (nth 6cube-g engrave-faces-ansi--color-6cube-values))
          (nearest-b (nth 6cube-b engrave-faces-ansi--color-6cube-values)))
      (if (and (= nearest-r r) (= nearest-g g) (= nearest-b b))
          (+ 16 (* 36 6cube-r) (* 6 6cube-g) 6cube-b)
        (let* ((grey-avg (/ (+ r g b) 3))
               (grey-index (if (> grey-avg 238) 23
                             (/ (- grey-avg 3) 10)))
               (grey (+ 8 (* 10 grey-index))))
          (if (> (engrave-faces-ansi--color-dist-squared (list grey grey grey)
                                                        (list r g b))
                 (engrave-faces-ansi--color-dist-squared (list nearest-r nearest-g nearest-b)
                                                        (list r g b)))
              (+ 232 grey-index)
            (+ 16 (* 36 6cube-r) (* 6 6cube-g) 6cube-b)))))))


;;;;;; 24-bit / 16m-color

(defun engrave-faces-ansi-color-24bit-code (r g b &optional background)
  (format (if background "\uE000[48;2;%d;%d;%dm" "\uE000[38;2;%d;%d;%dm") r g b))

;;; Applying the transformation

(defun engrave-faces-ansi--face-apply (faces content)
  "TODO record faces, and use `engrave-faces-ansi-face-nesting' to diff properties
with parent form more intelligent use of escape codes, and renewing properties which
are collateral damage from \"[0m\"."
  (let* ((face-str (engrave-faces-ansi-code (engrave-faces-merge-attributes faces))))
    (concat face-str content (if (string= face-str "") "" "\uE000[0m"))))

(defun engrave-faces-ansi--unescape-escape ()
  (goto-char (point-min))
  (while (re-search-forward "\uE000" nil t)
    (replace-match "\e")))

(declare-function ansi-color-apply-on-region "ansi-color"
                  (begin end &optional preserve-sequences))

;;;###autoload (autoload #'engrave-faces-ansi-buffer "engrave-faces-ansi" nil t)
;;;###autoload (autoload #'engrave-faces-ansi-file "engrave-faces-ansi" nil t)
(engrave-faces-define-backend "ansi" ".txt" #'engrave-faces-ansi--face-apply nil
                              (lambda () (ansi-color-apply-on-region (point-min) (point-max) t)))
(add-hook 'engrave-faces-ansi-after-hook #'engrave-faces-ansi--unescape-escape)

(provide 'engrave-faces-ansi)
;;; engrave-faces-ansi.el ends here
