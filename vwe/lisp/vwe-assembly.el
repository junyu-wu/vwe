;;; vwe-assembly.el --- Assembly Programming -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; apt install nasm
;; apt install poppler-utils
;; https://software.intel.com/en-us/articles/intel-sdm

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@asm--find-labels()
  "Find lables."
  (interactive)
  (save-excursion
	(save-restriction
	  (let* ((exist-label-list '())
			 (lstr))
		(goto-char (point-min))
		(while (re-search-forward "[0-9 a-z A-Z \. _]+:$" nil t)
		  (c-remove-font-lock-face
		   (car (bounds-of-thing-at-point 'symbol))
		   (cdr (bounds-of-thing-at-point 'symbol)))
		  (setq lstr (substring (thing-at-point 'symbol) 0 (- (length (thing-at-point 'symbol)) 1))
				exist-label-list (cons lstr exist-label-list)))
		(regexp-opt exist-label-list 'symbols)))))

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package nasm-mode
  :load-path
  (lambda () (vwe@lib--path-vwe-site-lisp "nasm-mode"))
  :mode
  ("\\.\\(asm\\|s\\|lst\\)$")
  :hook
  ((before-save . delete-trailing-whitespace)
   (nasm-mode . (lambda() (font-lock-add-keywords nil '(("section\\.[0-9 a-z A-Z \. _ ]+" . 'nasm-section-name)))))))

(use-package company-asm
  :load-path
  (lambda () (vwe@lib--path-vwe-site-lisp "company"))
  :hook
  (nasm-mode . (lambda () (vwe@lib--package-load 'company-asm)))
  :config
  (add-to-list 'company-backends 'company-asm--manual-asm-backend))

(provide 'vwe-assembly)
;;; vwe-assembly.el ends here
