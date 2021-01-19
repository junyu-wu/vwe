;;; vwe-theme.el --- Theme                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wu Junyu

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

;;

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defvar vwe@theme--current-theme
  nil
  "Current theme.")

(defun vwe@theme--load (func &rest args)
  "Advice around `load-theme' with FUNC and ARGS."
  (mapc #'disable-theme custom-enabled-themes)
  (apply func args)
  (vwe@theme--face-init))

(defun vwe@theme--face-init()
  "Set default face."
  (set-face-attribute 'header-line nil
					  :background (face-attribute 'mode-line :background))
  (set-face-attribute 'region nil
					  :background (face-attribute 'cursor :background)
					  :inverse-video t)
  (set-face-attribute 'fringe nil
					  :inherit 'mode-line)
  (set-face-attribute 'show-paren-match nil
					  :foreground nil
					  :background (face-attribute 'secondary-selection :background))
  (with-eval-after-load 'whitespace
	(set-face-attribute 'whitespace-line nil
						:foreground nil))
  (with-eval-after-load 'font-lock
	(set-face-attribute 'font-lock-comment-face nil
						:distant-foreground (if (display-graphic-p) nil (face-attribute 'error :foreground))))
  (with-eval-after-load 'symbol-overlay
	(set-face-attribute 'symbol-overlay-default-face nil
						:inherit nil
						:foreground nil
						:distant-foreground (face-attribute 'default :foreground)
						:background (face-attribute 'cursor :background)
						:weight 'ultra-light))
  (with-eval-after-load 'ivy
	(set-face-attribute 'ivy-minibuffer-match-face-2 nil
						:foreground "SpringGreen"
						:underline '(:color "DarkOrange"))
	(set-face-attribute 'ivy-current-match nil
						:box `(:color "DarkOrange"))))

;; 主题切换
(defun vwe@theme--toggle (&optional theme)
  "Toggle THEME."
  (interactive
   (list
    (intern (completing-read "Find custom theme: "
                             (mapcar #'symbol-name
									 (custom-available-themes))))))
  (when theme
	(setq vwe@theme--current-theme theme)
	(load-theme theme t)))

(defun vwe@theme--init ()
  "Theme init."
  (interactive)
  ;; (setq custom-safe-themes t)
  (advice-add 'load-theme :around #'vwe@theme--load)
  (if (display-graphic-p)
	  (setq vwe@theme--current-theme vwe@custom--theme-gui)
	(setq vwe@theme--current-theme vwe@custom--theme-tty))
  (if  (and vwe@theme--current-theme)
	  (load-theme vwe@theme--current-theme t)
	(vwe@theme--face-init)))

;; ***************************************************************************
;; config
;; ***************************************************************************

(use-package doom-themes)

(use-package mum-style-theme
  :load-path
  (lambda () (vwe@lib--path-vwe-site-lisp "mum/mum-theme"))
  :init
  (vwe@lib--package-load 'mum-style-theme))

(vwe@theme--init)

(provide 'vwe-theme)
;;; vwe-theme.el ends here
