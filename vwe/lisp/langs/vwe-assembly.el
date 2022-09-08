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
;; apt install poppler-utils ;; pdf 解析
;; https://software.intel.com/en-us/articles/intel-sdm ;; intel cpu 命令说明文档

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

;;
;; `nasm-mode'
;;
(vwe@lib--pkg nasm-mode
  :init ((push '("\\.\\(asm\\|s\\|lst\\)$" . nasm-mode) auto-mode-alist))
  :config ((add-hook 'before-save-hook #'delete-trailing-whitespace)
		   (add-hook 'nasm-mode-hook
					 (lambda() (font-lock-add-keywords nil
													   '(("section\\.[0-9 a-z A-Z \. _ ]+" . 'nasm-section-name)))))
		   ;;
		   ;; `company-nasm'
		   ;;
		   (vwe@lib--pkg company-asm
			 :init ((with-eval-after-load 'company
					  (add-hook 'nasm-mode-hook
								(lambda ()
								  (vwe@pkg--company-make-mode-local-backends
								   'company-assembly)))))
			 :path (vwe@lib--path-vwe-site-lisp "company")
			 :undefer t)))

(provide 'vwe-assembly)
;;; vwe-assembly.el ends here
