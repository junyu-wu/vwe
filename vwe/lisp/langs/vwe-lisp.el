;;; vwe-lisp.el --- Common Lisp And Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
;; Keywords: languages, lisp

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

;;

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `elisp-mode'
;;
(vwe@lib--package 'elisp-mode
				  (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list (make-local-variable 'company-backends) '(company-elisp))))
				  nil nil nil nil t)

;;
;; `highlight-defined' 高亮elisp符号
;;
(vwe@lib--package 'highlight-defined
				  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
				  nil
				  (setq highlight-defined-face-use-itself t))

;;
;; `helpful' 根据上下文扩展帮助信息
;;
(vwe@lib--package 'helpful
				  nil
				  (vwe@lib--keymap-set emacs-lisp-mode-map '(([remap describe-key] helpful-key)
															 ([remap describe-symbol] helpful-symbol))))

(provide 'vwe-lisp)
;;; vwe-lisp.el ends here
