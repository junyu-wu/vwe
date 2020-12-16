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
(defun vwe@theme--load (origin-func theme &rest args)
  "Hook around `load-theme'.
ORIGIN-FUNC.
THEME.
ARGS."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hook-with-args 'vwe@custom--theme-load-before-hook theme)
  (apply origin-func theme args)
  (run-hook-with-args 'vwe@custom--theme-load-after-hook theme))

(defun vwe@theme--default-face()
  "Set default face."
  (set-face-attribute 'header-line nil
					  :background "#434C5E")
  (set-face-attribute 'region nil
					  :background "#555555")
  (set-face-attribute 'fringe nil
					  :background nil
					  :inherit 'mode-line)
  (set-face-attribute 'line-number-current-line nil
					  :background nil
					  :inherit 'mode-line)
  (set-face-attribute 'line-number nil
					  :background nil)
  (set-face-attribute 'font-lock-string-face nil
					  :background nil)
  (set-face-attribute 'font-lock-keyword-face nil
					  :background nil)
  (set-face-attribute 'font-lock-constant-face nil
					  :background nil)
  (set-face-attribute 'font-lock-warning-face nil
					  :background nil)
  (set-face-attribute 'font-lock-function-name-face nil
					  :box nil
					  :inherit 'mode-line-inactive)
  (set-face-attribute 'show-paren-match nil
					  :foreground nil
					  :background nil
					  :box `(:color "red" :line-width 1))
  (with-eval-after-load 'tab-bar
	(set-face-attribute 'tab-bar nil
						:background nil
						:inherit 'default)
	(set-face-attribute 'tab-line nil
						:background nil
						:inherit 'mode-line))
  (with-eval-after-load 'symbol-overlay
	(set-face-attribute 'symbol-overlay-default-face nil
						:foreground nil
						:background nil
						:inherit nil
						:box nil
						:underline `(:color "DarkOrange")))
  (with-eval-after-load 'cus-edit
    (set-face-attribute 'custom-link nil
						:foreground nil
						:inherit 'font-lock-function-name-face))
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
						:foreground nil))
  (with-eval-after-load 'whitespace
	(set-face-attribute 'whitespace-line nil
						:foreground nil))
  (with-eval-after-load 'ivy
	(set-face-attribute 'ivy-minibuffer-match-face-2 nil
						:foreground "green"
						:underline '(:color "DarkOrange"))
	(set-face-attribute 'ivy-current-match nil
						:underline '(:color "DarkOrange"))))

;; 主题切换
(defun vwe@theme--toggle (&optional theme)
  "Toggle THEME."
    (interactive
   (list
    (intern (completing-read "Find custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))))
  (when theme
		(load-theme theme)
		(vwe@theme--default-face)))

(defun vwe@theme--init ()
  "Theme init."
  (interactive)
  (setq custom-safe-themes t)
  (advice-add 'load-theme :around #'vwe@theme--load)
  (if (display-graphic-p)
	  (vwe@theme--toggle vwe@custom--theme-gui)
	(vwe@theme--toggle vwe@custom--theme-tty))
  (when (display-graphic-p)
    (vwe@theme--default-face)))

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic t
		doom-themes-treemacs-theme "doom-colors")
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package tao-theme)

(vwe@theme--init)

(provide 'vwe-theme)
;;; vwe-theme.el ends here
