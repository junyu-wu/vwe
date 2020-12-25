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
(with-eval-after-load 'mum-key
  (mum-key-define emacs-lisp
				  ("emacs lisp"
				   (("b" eval-buffer "eval buffer")
					("d" eval-defun "eval defun")
					("r" eval-region "eval region")
					("e" eval-expression "eval expression")
					("k" describe-key "key")
					("f" describe-function "function")
					("v" describe-variable "variable")
					("RET" mum-key:global)))))
;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package elisp-mode
  :ensure nil
  :hook
  ((emacs-lisp-mode . (lambda ()
						(add-to-list (make-local-variable 'company-backends)
									 '(company-elisp)))))
  :bind
  (:map emacs-lisp-mode-map
		("C-M-<return>" . mum-key:emacs-lisp))
  :config
  ;; 高亮elisp符号
  (use-package highlight-defined
	:hook
	(emacs-lisp-mode . highlight-defined-mode)
	:init
	(setq highlight-defined-face-use-itself t))

  ;; 根据上下文扩展帮助信息
  (use-package helpful
	:bind
	(([remap describe-key] . helpful-key)
	 ([remap describe-symbol] . helpful-symbol))))

(provide 'vwe-lisp)
;;; vwe-lisp.el ends here
