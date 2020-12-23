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
	("C-@"         . vwe@lib--minibuffer-switch)
	("C-c C-f"     . format-all-buffer))
  "Default keybind list.")

(defun vwe@keybind--init ()
  "Key bind init."
  (interactive)
  (vwe@lib--keymap-global-set vwe@keybind--default-list))

(with-eval-after-load 'mum-key
  (eval
   (mum-key--keymap-define global
						   ("global"
							(("g" counsel-etags-grep "grep")
							 ("b" ibuffer "ibuffer")
							 ("d" dired "dired")
							 ("s" swiper-thing-at-point "thing at point")
							 ("r" vwe@lib--replace "replace")
							 ("x" vwe@lib--frame-reset "reset frame")
							 ("t" vwe@theme--toggle "toggle theme")
							 ("f" format-all-buffer "format code")
							 ("p" vwe@prog--switch-mode "switch mode")
							 ("~" mum-mark--paren--paren-pair "paren mark")
							 ("(" vwe@base--paren-toggle-style "paren style")
							 ("+" vwe@ui--text-scale-reset "+/- text scale"))))))

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@keybind--init)

(vwe@lib--keymap-global-set '(("M-RET" . mum-key:global)
							  ("C-M-<return>" . mum-key:global)))

(provide 'vwe-keybinds)
;;; vwe-keybinds.el ends here
