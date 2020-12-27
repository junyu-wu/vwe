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
  '(("<f3>"                             . split-window-horizontally)
	("C-<f3>"                           . split-window-vertically)
	("<f12>"                            . toggle-frame-fullscreen)
	("C-r"                              . recentf-open-files)
	("C-'"                              . imenu-list-smart-toggle)
	("C-s"                              . swiper)
	("C-x C-b"                          . ibuffer)
	("C-x C-f"                          . counsel-find-file)
	("C-;"                              . iedit-mode)
	("C-M-;"                            . iedit-rectangle-mode)
	("C-="                              . er/expand-region)
	("C-,"                              . goto-last-change)
	("M-x"                              . counsel-M-x)
	("M-/"                              . company-complete)
	("C-M-/"                            . company-yasnippet)
	("M-y"                              . undo-tree-redo)
	("M-@"                              . vwe@lib--minibuffer-switch)
	("M-C-k"                            . vwe@lib--buffer-kill-current)
	("M-C-y"                            . vwe@lib--window-kill-current)
	("M-C-o"                            . vwe@lib--window-maximize)
	("M-C-r"                            . browse-kill-ring)
	("M-C-b"                            . vwe@buffer--switch-to)
	("C-@"                              . vwe@lib--minibuffer-switch)
	("C-c C-f"                          . format-all-buffer)
	("C-M-["                            . mc/edit-beginnings-of-lines)
	("C-M-]"                            . mc/edit-ends-of-lines)
	("C-M-|"                            . set-rectangular-region-anchor)
	("C-M-="                            . cua-rectangle-mark-mode)
	("M-'"                              . sp-up-sexp)
	("M-)"                              . sp-up-sexp)
	("M-("                              . sp-down-sexp)
	("M-C-'"                            . sp-backward-unwrap-sexp)
	("M-l"                              . string-inflection-lower-camelcase)
	("M-u"                              . string-inflection-upcase)
	("M-*"                              . treemacs)
	("M-C-,"                            . eacl-complete-multiline)
	("C-]"                              . counsel-etags-find-tag-at-point)

	([remap comment-dwim]               . comment-dwim-2)
	([remap move-beginning-of-line]     . mwim-beginning-of-code-or-line)
	([remap move-end-of-line]           . mwim-end-of-code-or-line)
	([remap goto-line]                  . goto-line-preview))
  "Default keybind list.")

(defun vwe@keybind--init ()
  "Key bind init."
  (interactive)
  (vwe@lib--keymap-global-set vwe@keybind--default-list))

(with-eval-after-load 'mum-key
  (mum-key-define global
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
					("l" vwe@lsp--run "lsp")
					("a" org-agenda "agenda")
					("c" org-capture "capture")
					("~" mum-mark--paren--paren-pair "paren mark")
					("(" vwe@base--paren-toggle-style "paren style")
					("+" vwe@ui--text-scale-reset "+/- text scale")))))
;; ***************************************************************************
;; config
;; ***************************************************************************

(vwe@keybind--init)

(vwe@lib--keymap-global-set '(("M-#" . mum-key:global)
							  ("M-RET" . mum-key:global)))

(provide 'vwe-keybinds)
;;; vwe-keybinds.el ends here
