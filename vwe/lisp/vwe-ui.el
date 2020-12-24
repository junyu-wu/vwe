;;; vwe-ui.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  WuJunyu

;; Author: WuJunyu <vistar_w@hotmail.com>
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

;;

;;; Code:

;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@ui--text-scale-reset (&optional size)
  "Reset text scale SIZE."
  (interactive
   (let* ((size (read-number (format "inc/dec %s:" text-scale-mode-amount))))
	 (list size)))
  (cond
   ((> size 0) (text-scale-increase size))
   ((< size 0) (text-scale-decrease (* size -1)))
   ((= size 0) (text-scale-adjust 0))))

(defun vwe@ui--init ()
  "UI init."
  (interactive)
  (setq-default cursor-type    'bar
				fill-column    80
				tab-width      4
				show-trailing-whitespace t)
  (setq frame-title-format
		(list (format "%s %%S: %%j " (system-name))
			  '(buffer-file-name "%f"
								 (dired-directory dired-directory "%b")))
		icon-title-format frame-title-format)

  (unless vwe@custom--frame-menu-bar?
	(menu-bar-mode -1))
  (unless vwe@custom--frame-tool-bar?
	(tool-bar-mode -1))
  (unless vwe@custom--frame-scroll-bar?
	(scroll-bar-mode -1)))

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@ui--init)

(use-package display-line-numbers
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))

(use-package whitespace
  :ensure nil
  :diminish
  (global-whitespace-mode . nil)
  :hook
  (after-init . global-whitespace-mode)
  :init
  (setq whitespace-line-column nil
		whitespace-style '(face line lines-tail)))

;; 显示缩进样式
(use-package highlight-indent-guides
  :diminish
  (highlight-indent-guides-mode . nil)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
		highlight-indent-guides-character ?\|
		highlight-indent-guides-responsive 'top)
  :config
  (defun vwe@pkg--highlighter-func (level responsive display)
	(if (> 1 level)
		nil
	  (highlight-indent-guides--highlighter-default
	   level responsive display)))
  (setq highlight-indent-guides-highlighter-function
		'vwe@pkg--highlighter-func))

(use-package symbol-overlay
  :diminish
  (symbol-overlay-mode . nil)
  :hook
  (prog-mode . symbol-overlay-mode)
  :init
  (setq symbol-overlay-idle-time 0.1)
  :config
  (defun vwe@pkg--symbol-overlay-turn-off (&rest _)
	"Turn off symbol highlighting."
	(interactive)
	(symbol-overlay-mode -1))
  (defun vwe@pkg--symbol-overlay-turn-on (&rest _)
	"Turn on symbol highlighting."
	(interactive)
	(when (derived-mode-p 'prog-mode)
	  (symbol-overlay-mode 1)))

  (advice-add #'set-mark :after #'vwe@pkg--symbol-overlay-turn-off)
  (advice-add #'deactivate-mark :after #'vwe@pkg--symbol-overlay-turn-on))

(use-package hl-todo
  :diminish
  (hl-todo-mode . nil)
  :hook
  (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
	(cl-pushnew `(,keyword . ,(face-foreground 'error))
				hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
	(cl-pushnew `(,keyword . ,(face-foreground 'warning))
				hl-todo-keyword-faces)))

(use-package mum-mark
  :load-path
  (lambda ()
	(vwe@lib--path-vwe-site-lisp "mum/mum-mark"))
  :hook
  (after-init . mum-mark-mode))

(use-package all-the-icons
  :init
  (setq inhibit-compacting-font-caches t))

(provide 'vwe-ui)
;;; vwe-ui.el ends here
