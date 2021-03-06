;;; vwe-display.el ---                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015   WuJunyu

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

;; ***************************************************************************
;; config
;; ***************************************************************************
;;
;; `hl-line'
;;
(add-hook 'after-init-hook #'global-hl-line-mode)

;;
;; `whitespace'
;;
(vwe@lib--package 'whitespace
				  (add-hook 'after-init-hook #'global-whitespace-mode)
				  nil
				  (setq whitespace-line-column nil
						whitespace-style '(lines lines-tail)))

;;
;; `highlight-indent-guides' 显示缩进样式
;;
(vwe@lib--package 'highlight-indent-guides
				  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
				  nil
				  (defun vwe@pkg--highlighter-func (level responsive display)
					"Highlighter function, whit LEVEL RESPONSIVE and DISPLAY."
					(if (> 1 level)
						nil
					  (highlight-indent-guides--highlighter-default level responsive display)))
				  (setq highlight-indent-guides-method 'character
						highlight-indent-guides-character ?\|
						highlight-indent-guides-responsive 'top
						highlight-indent-guides-highlighter-function 'vwe@pkg--highlighter-func))

;;
;; `symbol-overlay'
;;
(vwe@lib--package 'symbol-overlay
				  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
				  nil
				  (progn
					(setq symbol-overlay-idle-time 0.1)
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
					(advice-add #'deactivate-mark :after #'vwe@pkg--symbol-overlay-turn-on)))

;;
;; `all-the-icons'
;;
(vwe@lib--package 'all-the-icons)


(provide 'vwe-display)
;;; vwe-display.el ends here
