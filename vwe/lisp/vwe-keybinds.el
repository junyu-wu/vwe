;;; vwe-keybinds.el --- Global Key Binding   -*- lexical-binding: t; -*-

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

;;

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defvar vwe@keybind--default-list
  '(("<f3>"        . split-window-horizontally)
	("C-<f3>"      . split-window-vertically)
	("<f12>"       . toggle-frame-fullscreen)
	("C-r"         . recentf-open-files)
	("C-'"         . imenu-list-smart-toggle)
	("M-y"         . undo-tree-redo)
	("M-@"         . vwe@lib--minibuffer-switch)
	("M-C-k"       . vwe@lib--buffer-kill-current)
	("M-C-y"       . vwe@lib--window-kill-current)
	("M-C-o"       . vwe@lib--window-maximize)
	("M-C-f"       . vwe@theme--toggle)
	("M-C-r"       . browse-kill-ring)
	("M-C-b"       . (lambda () (interactive) (vwe@buffer--switch-to)))
	;; ([mouse-4]     . (lambda () (interactive) (scroll-down 3)))
	;; ([mouse-5]     . (lambda () (interactive) (scroll-up 3)))
	)
  "Default keybind list.")

(defun vwe@keybind--init ()
  "Key bind init."
  (interactive)
  (vwe@lib--keymap-global-set vwe@keybind--default-list))

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@keybind--init)

(provide 'vwe-keybinds)
;;; vwe-keybinds.el ends here
