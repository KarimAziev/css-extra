;;; css-extra.el --- Misc CSS commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/css-extra
;; Version: 0.1.0
;; Keywords: matching
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Misc CSS commands

;;; Code:

(require 'subr-x)

(declare-function lsp "lsp-mode")

(defcustom css-extra-base-font-size 16
  "Base font size declared on html element in pixels.
It is used for conversion between pixels to rems, and rems to pixels."
  :type 'integer
  :group 'css-extra)

(make-variable-buffer-local 'css-extra-base-font-size)

(defun css-extra-confirm-and-replace-region (beg end replacement)
  "Replace region between BEG and END with REPLACEMENT.
REPLACEMENT should be a string, or an unary function that string or nil."
  (when-let ((overlay (make-overlay beg end))
             (rep (if (functionp replacement)
                      (funcall replacement)
                    replacement)))
    (when (unwind-protect
              (progn (overlay-put overlay 'face 'error)
                     (overlay-put overlay 'after-string
                                  (concat
                                   "\s"
                                   (propertize rep
                                               'face 'success)))
                     (yes-or-no-p "Replace region?"))
            (delete-overlay overlay))
      (replace-region-contents beg end (lambda () rep))
      rep)))

(defun css-extra--cycle-rem-to-px (value base-font-size)
  "Convert VALUE to string representation as rem or pixels.

VALUE should be a string with px or rem suffix.

If the suffix of value is px, convert VALUE to rem.
If the suffix of value is rem, to pixels.

BASE-FONT-SIZE should be integer which is specify base unit for conversion."
  (pcase-let ((`(,fn ,ext ,new-ext)
               (pcase value
                 ((pred (string-suffix-p "rem"))
                  (list #'* "rem" "px"))
                 ((pred (string-suffix-p "px"))
                  (list #'/ "px" "rem")))))
    (concat
     (replace-regexp-in-string
      "\\.?[0]+$" ""
      (number-to-string
       (funcall fn
                (float
                 (string-to-number
                  (substring-no-properties
                   value 0 (- (length
                               value)
                              (length ext)))))
                base-font-size)))
     new-ext)))


(defun css-extra-convert-px-to-rem-by-regex (regex &optional use-confirm)
  "Replace REGEX pixel or rem matches in the current buffer.
Base size of fonts is taken from the variable `css-extra-base-font-size'.
If USE-CONFIRM is non nil, prompt user about every replacement."
  (save-excursion
    (goto-char (point-max))
    (with-undo-amalgamate
      (while (re-search-backward regex nil t
                                 1)
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (value (match-string-no-properties 0))
              (rep))
          (setq rep (css-extra--cycle-rem-to-px
                     value
                     css-extra-base-font-size))
          (if use-confirm
              (css-extra-confirm-and-replace-region
               beg end rep)
            (replace-region-contents beg end (lambda () rep))))))))

;;;###autoload
(defun css-extra-px-to-rem (&optional use-confirm)
  "Convert pixels to rems in the current buffer.
Base size of fonts is taken from the variable `css-extra-base-font-size'.
If USE-CONFIRM is non nil, prompt user about every replacement."
  (interactive "P")
  (css-extra-convert-px-to-rem-by-regex "\\_<\\(\\([-]?[0-9]+\\)px\\)\\_>"
                                        use-confirm))

;;;###autoload
(defun css-extra-rem-to-px (&optional use-confirm)
  "Convert rems to pixels in the current buffer.
Base size of fonts is taken from the variable `css-extra-base-font-size'.
If USE-CONFIRM is non nil, prompt user about every replacement."
  (interactive "P")
  (css-extra-convert-px-to-rem-by-regex
   "\\_<\\([-]?[0-9]+\\(?:[\\.]?[0-9]+\\)?rem\\)\\_>"
   use-confirm))

;;;###autoload
(defun css-extra-cycle-rem-to-px ()
  "Cycle between pixels and rems at point.
Base size of fonts is taken from the variable `css-extra-base-font-size'."
  (interactive)
  (pcase-let* ((`(,beg . ,end)
                (save-excursion
                  (let* ((a (save-excursion
                              (skip-chars-forward "-a-z0-9.")
                              (point)))
                         (b (save-excursion
                              (skip-chars-backward "-a-z0-9.")
                              (point))))
                    (if (string-blank-p (buffer-substring-no-properties a b))
                        nil
                      (cons a b)))))
               (value
                (when (and beg end)
                  (css-extra--cycle-rem-to-px
                   (buffer-substring-no-properties beg end)
                   css-extra-base-font-size))))
    (when value
      (replace-region-contents beg end (lambda () value)))))

(defun css-extra--skip-whitespace ()
  "Skip whitespace and comments on the current line."
  (skip-chars-forward "\s\t")
  (let ((comm-start))
    (while (setq comm-start (or (and (nth 4 (syntax-ppss (point)))
                                     (nth 8 (syntax-ppss (point))))
                                (when (looking-at (regexp-quote comment-start))
                                  (point))))
      (goto-char comm-start)
      (forward-comment 1)
      (skip-chars-forward "\s\t"))))

(defun css-extra--get-var-cell ()
  "Extract CSS property and value from current line as a pair."
  (let ((prop)
        (value))
    (save-excursion
      (goto-char (line-beginning-position))
      (css-extra--skip-whitespace)
      (when-let* ((line-end (line-end-position))
                  (prop-beg (point))
                  (prop-end (re-search-forward ":" line-end t 1)))
        (setq prop (buffer-substring-no-properties prop-beg (1- prop-end)))
        (css-extra--skip-whitespace)
        (when-let* ((beg (point))
                    (end (re-search-forward ";" line-end t 1)))
          (setq value (buffer-substring-no-properties beg (1- end))))))
    (cons prop value)))

;;;###autoload
(defun css-extra-copy-colors-as-vars ()
  "Copy CSS colors as variables to clipboard."
  (interactive)
  (let ((colors)
        (var-names)
        (alist)
        (result))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(#[a-z0-9]+\\);" nil t
                                1)
        (let ((value (match-string-no-properties 1)))
          (unless (or (nth 4 (syntax-ppss (point)))
                      (member value colors))
            (when-let* ((var-prefix (car (split-string
                                          (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position))
                                          "[\s\t:]" t)))
                        (var-name
                         (pcase var-prefix
                           ((or "background" "background-color") "bg-color")
                           ((guard (not (string-match-p "color" var-prefix)))
                            (concat var-prefix "-color"))
                           (_ var-prefix)))
                        (var
                         (unless (string-prefix-p "--" var-name)
                           (concat "--" var-name))))
              (push value colors)
              (if-let ((cell (assoc-string var alist)))
                  (setcdr cell (append (cdr cell)
                                       (list value)))
                (push (list var value) alist))
              (push var var-names))))))
    (pcase-dolist (`(,var . ,colors) alist)
      (let ((lines (seq-map-indexed (lambda (c i)
                                      (format "%s-%d: %s;" var i c))
                                    colors)))
        (setq result (append result lines))))
    (kill-new (string-join result "\n"))
    (message "Copied")
    result))

;;;###autoload
(defun css-extra-use-var-name-at-point ()
  "Replace value occurrences with CSS variable reference in buffer."
  (interactive)
  (pcase-let ((`(,var . ,value)
               (css-extra--get-var-cell))
              (occurences))
    (when (and var value)
      (save-excursion
        (forward-line 1)
        ;; (goto-char (point-max))
        (let ((regex (regexp-opt (list value)))
              (rep (format "var(%s)" var)))
          (with-undo-amalgamate
            (while (re-search-forward regex nil t 1)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (unless (looking-at "[a-z0-9]")
                  (delete-region beg end)
                  (insert rep)))
              (setq occurences (1+ (or occurences 0)))))))
      (when occurences
        (message "Replaced %d occurences" occurences)))))

(defun css-extra-tailwindcss-init ()
  "Initialize LSP if a Tailwind config exists in the current project."
  (require 'project)
  (when-let ((proj
              (when-let ((project (ignore-errors (project-current))))
                (if (fboundp 'project-root)
                    (project-root project)
                  (with-no-warnings
                    (car (project-roots project)))))))
    (when (file-exists-p (expand-file-name "tailwind.config.js" proj))
      (require 'lsp)
      (lsp))))

(provide 'css-extra)
;;; css-extra.el ends here